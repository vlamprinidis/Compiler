open Llvm
open Llvm_analysis
open Ast
open Error
open Types
open Symbol
open Format

exception Error of string

let context = global_context ()
let the_module = create_module context "alan"
let builder = builder context

let int_type = i16_type context (* lltype *)
let byte_type = i8_type context
let bool_type = i1_type context
let proc_type = void_type context

(* Helping functions *)

let rec give_lltype alan_type =
    begin match alan_type with
        | TYPE_int                          -> int_type
        | TYPE_byte                         -> byte_type
        | TYPE_array (elem_type, _)         -> pointer_type (give_lltype elem_type) (* element type can only be a basic data type in alan - int or byte *)
        | _ -> fatal "alan to lltype didn't work"; raise Terminate
    end

let rec give_fret_lltype alan_type =
    begin match alan_type with
        | TYPE_int  -> int_type
        | TYPE_byte -> byte_type
        | TYPE_proc -> proc_type
        | _ -> fatal "alan to return type didn't work"; raise Terminate
    end

let rec give_par_lltype_lst par_lst =
    begin match par_lst with
        | pr :: tl ->
            begin match pr.par_type with
                | TYPE_array _  ->  (give_lltype pr.par_type) :: (give_par_lltype_lst tl)
                | _             ->  begin match pr.par_pass_way with
                                        | PASS_BY_VALUE     -> (give_lltype pr.par_type) :: (give_par_lltype_lst tl)
                                        | PASS_BY_REFERENCE -> (pointer_type (give_lltype pr.par_type)):: (give_par_lltype_lst tl)
                                    end
            end
        | [] -> []
    end

let rec give_locvar_lltype_lst loc_lst =
    begin match loc_lst with
        | (Local_var vr) :: tl  -> (give_lltype vr.var_type) :: (give_locvar_lltype_lst tl)
        | (Local_func _) :: tl  -> give_locvar_lltype_lst tl
        | []                    -> []
    end

let give_frame_lltype parent_frame_type func_pars func_local =

    let par_lltype_lst = give_par_lltype_lst func_pars in (* List of parameter lltypes *)
    let locvar_lltype_lst = give_locvar_lltype_lst func_local in (* List of local-variable lltyppes (without local funcs) *)

    let access_link_type = pointer_type parent_frame_type in 
    let arr_of_lltypes = Array.of_list ( access_link_type :: (par_lltype_lst @ locvar_lltype_lst) ) in (* Array of lltypes for frame lltype *)

    let ll_frame_type = struct_type context arr_of_lltypes in (* Frame lltype (llvm-struct type) *)

    ll_frame_type

let give_func_lltype parent_frame_type func_pars func_ret_type  =

    let par_lltype_lst = give_par_lltype_lst func_pars in (* List of parameter lltypes *)
    let access_link_type = pointer_type parent_frame_type in 

    let par_llarr = Array.of_list (access_link_type :: par_lltype_lst) in
    let fret_lltype = give_fret_lltype func_ret_type in (* LLtype of function's return value *)

    let func_lltype = function_type fret_lltype par_llarr in (*Function lltype *)

    func_lltype

let get_ptr_to_Nth_element struct_ptr n = build_struct_gep struct_ptr n "ptr to nth element" builder

let dereference ptr = build_load ptr "de-reference" builder

let get_ptr_to_new_array arr_lltype arr_size = 
    let arr_typ = array_type arr_lltype arr_size in
    let arr = build_alloca arr_typ "array allocation" builder in
    let ptr = get_ptr_to_Nth_element arr 0 in
    ptr

let store_at_struct llvalue_tostore struct_ptr idx =
    let element_ptr_llvalue = get_ptr_to_Nth_element struct_ptr idx in
    ignore (build_store llvalue_tostore element_ptr_llvalue builder)

(* Note: access_link == frame_ptr *)
let rec get_deep_access_link frame_ptr diff =
    if( diff = 0 ) 
    then begin
        frame_ptr
    end
    else begin
        let first_element = dereference (get_ptr_to_Nth_element frame_ptr 0) in
        get_deep_access_link first_element (diff-1)
    end
    
(* End of helping functions *)

and codegen_func func_ast =

    let parent_frame_type = match func_ast.parent with 
        | Some some_parent ->
            begin match some_parent.frame_type with
                | Some some_frame_type -> some_frame_type
                | None -> fatal "parent function does not have a frame type"; raise Terminate
            end
        | None -> fatal "function does not have a parent"; raise Terminate
    in

    let func_lltype = give_func_lltype parent_frame_type func_ast.func_pars func_ast.func_ret_type in
    let frame_type = give_frame_lltype parent_frame_type func_ast.func_pars func_ast.func_local in
    func_ast.frame_type <- Some frame_type;

    (* Declare first *)
    let func_llvalue = declare_function func_ast.full_name func_lltype the_module in

    (* Generate code for local functions *)
    let gen_loc_func loc = match loc with
        | Local_func loc_func -> codegen_func loc_func
        | Local_var _         -> ()  
    in
    List.iter gen_loc_func func_ast.func_local;

    (* Create function basic-block *)
    let f_bb = append_block context "entry block" func_llvalue in
    position_at_end f_bb builder;

    (* Create frame *)
    let frame_ptr = build_alloca frame_type "frame" builder in

    (* Store each parameter into the frame *)
    let store_par par_llvalue offset = ignore (store_at_struct par_llvalue frame_ptr offset) in
    let par_llvalue_lst = Array.to_list (params func_llvalue) in

    let give_offset pr = pr.par_offset in
    let par_offsets = List.map give_offset func_ast.func_pars in

    (* Store starting from position 0 -- access link is included*)
    List.iter2 store_par par_llvalue_lst (0 :: par_offsets);
    
    (* Store local var - simply allocate memory for the new arrays and store its pointer to the right position in frame *)
    let rec store_vars loc_lst =
        begin match loc_lst with
            | (Local_var vr) :: tl  ->  begin match vr.var_type with
                                            | TYPE_array (elem_type, arr_size) ->   let ptr_to_arr = get_ptr_to_new_array (give_lltype elem_type) arr_size in
                                                                                    store_at_struct ptr_to_arr frame_ptr vr.var_offset;
                                                                                    store_vars tl

                                            | _                                ->   store_vars tl
                                        end
            | (Local_func _) :: tl  -> store_vars tl
            | []                    -> []
        end
    in
    ignore (store_vars func_ast.func_local);

    (* Generate code for statements *)
    (* ignore ( List.fold_left (codegen_stmt_until frame_ptr) false func_ast.func_stmt ); *)

    ignore (stmt_help frame_ptr func_ast.func_stmt);

    (* LLVM requires a terminator block *)
    let _ = begin match block_terminator (insertion_block builder) with
        | None      ->  begin match func_ast.func_ret_type with
                            | TYPE_int      ->  ignore (build_ret (const_int int_type 0) builder)
                            | TYPE_byte     ->  ignore (build_ret (const_int byte_type 0) builder)
                            | TYPE_proc     ->  ignore (build_ret_void builder)
                            | _             ->  fatal "Codegen: invalid function return type"; raise Terminate
                        end

        | Some _    ->  ()
    end in 
    ()


and codegen_call frame_ptr call_ast =
    (* https://en.wikipedia.org/wiki/Nested_function *)
    let callee_full_name = match call_ast.callee_full_name with | Some name -> name | None -> fatal "codegen_call: callee full name error"; raise Terminate in
    let callee_func_llvalue = match (lookup_function callee_full_name the_module) with | Some fn -> fn | None -> fatal "codegen_call: Function not found"; raise Terminate in

    (* Must check if the callee is an existing function that doesn't need an access link *)
    (* Idea: compare number of arguments in expr list with arguments declared through llvm *)
    let access_link_is_required = 
        let llvm_args_num = Array.length (params callee_func_llvalue) in
        let expr_args_num = List.length call_ast.call_expr in
        llvm_args_num <> expr_args_num
    in

    let give_expr_llvalue declared exr = 
        begin match declared with
            | PASS_BY_VALUE      -> codegen_expr frame_ptr exr
            | PASS_BY_REFERENCE  -> 
                begin match exr.expr_raw with
                    | E_lvalue lval ->  begin match lval.l_value_raw with
                                            | L_id _     ->  codegen_lval frame_ptr lval

                                            | L_str str  ->  let global_str = build_global_string str "string to build" builder in
                                                             build_struct_gep global_str 0 "string as a char ptr" builder
                                        end
                    | _             ->  fatal "passing by reference a non-lvalue"; raise Terminate
                end
        end
    in

    let expr_arr = 
        let declared_lst = match call_ast.declared_pars with | Some lst -> lst | None -> fatal "codegen_call: declared_pars empty"; raise Terminate in
        let expr_llvalue_lst = List.map2 give_expr_llvalue declared_lst call_ast.call_expr in
        
        if ( access_link_is_required ) then begin
            let diff = call_ast.caller_nesting_scope - call_ast.callee_scope + 1  in
            let correct_frame = get_deep_access_link frame_ptr diff in

            Array.of_list ( correct_frame :: expr_llvalue_lst )
        end 
        else begin
            Array.of_list expr_llvalue_lst
        end
    in
    build_call callee_func_llvalue expr_arr "" builder

(* and codegen_stmt_until frame_ptr previous_stmt_is_terminator st  = (* returns true if terminal *)
    previous_stmt_is_terminator || codegen_stmt frame_ptr st *)

and stmt_help frame_ptr st_lst = (* returns true if terminal *)
begin match st_lst with
    | st_hd :: tl   ->      let st_next = codegen_stmt frame_ptr st_hd in 
                            if(st_next) then (true) else (stmt_help frame_ptr tl)
    | []            ->      false
end

and codegen_stmt frame_ptr stmt_ast = (* returns true if terminal *)
    begin match stmt_ast with
        | Null_stmt                 ->
            false

        | S_assign (lval,exr)        ->
            let exr_llvalue = codegen_expr frame_ptr exr in
            let element_ptr = codegen_lval frame_ptr lval in
            ignore (build_store exr_llvalue element_ptr builder);
            false

        | S_comp st_lst             ->
            (* List.fold_left (codegen_stmt_until frame_ptr) false st_lst *)
            stmt_help frame_ptr st_lst

        | S_call fcall              -> (* Call to a void function *)
            ignore (codegen_call frame_ptr fcall);
            false

        | S_if (cnd, st, st_option) ->
        (* http://llvm.org/docs/tutorial/OCamlLangImpl5.html *)
            let cond_val = codegen_cond frame_ptr cnd in

            (* Grab the first block so that we might later add the conditional branch
            * to it at the end of the function. *)
            let start_bb = insertion_block builder in
            let the_function = block_parent start_bb in

            let then_bb = append_block context "then" the_function in
            let merge_bb = append_block context "ifcont" the_function in

            position_at_end then_bb builder;
            let then_stmt_is_terminal = codegen_stmt frame_ptr st in
            let new_then_bb = insertion_block builder in (* Codegen of 'then' can change the current block *)

            if(not then_stmt_is_terminal)
            then begin
                (* Set an unconditional branch at the end of the then-block to the merge-block *)
                position_at_end new_then_bb builder; 
                ignore (build_br merge_bb builder)
            end ;

            let _ = match st_option with
                | Some st_some ->
                    let else_bb = append_block context "else" the_function in

                    position_at_end else_bb builder;
                    let else_stmt_is_terminal = codegen_stmt frame_ptr st_some in
                    let new_else_bb = insertion_block builder in

                    if( not else_stmt_is_terminal)
                    then begin
                        (* Set an unconditional branch at the end of the else-block to the merge-block*)
                        position_at_end new_else_bb builder; 
                        ignore (build_br merge_bb builder)
                    end;

                    (* Return to the end of the start-block to add the conditional branch *)
                    position_at_end start_bb builder;
                    ignore ( build_cond_br cond_val then_bb else_bb builder )
                    
                    (* position_at_end merge_bb builder; *)
                    (* then_stmt_is_terminal && else_stmt_is_terminal *)

                | None ->
                    (* Return to the end of the start-block to add the conditional branch *)
                    position_at_end start_bb builder;
                    ignore ( build_cond_br cond_val then_bb merge_bb builder )

                    (* position_at_end merge_bb builder; *)
                    (* false *)

            in
            (* Finally, set the builder to the end of the merge-block *)
            position_at_end merge_bb builder;
            false

        | S_while (cnd, st)          ->
            (* Grab the first block so that we later add the unconditional branch
            * to it at the end of the function. *)
            let start_bb = insertion_block builder in
            let the_function = block_parent start_bb in

            let while_bb = append_block context "while" the_function in
            let do_bb = append_block context "do" the_function in
            let merge_bb = append_block context "continue" the_function in

            (* Set an unconditional branch at the end of the 'start' block to the start of the while-block *)
            position_at_end start_bb builder; 
            ignore (build_br while_bb builder);

            position_at_end while_bb builder;  
            let cond_val = codegen_cond frame_ptr cnd in
            let new_while_bb = insertion_block builder in (* Codegen of 'while' can change the current block *)
            (* Add the conditional branch to either the do-block or the merge-block*)
            position_at_end new_while_bb builder;
            ignore (build_cond_br cond_val do_bb merge_bb builder);      

            position_at_end do_bb builder;
            let while_st_is_terminator = codegen_stmt frame_ptr st in
            let new_do_bb = insertion_block builder in (* Codegen of 'do' can change the current block *)
            
            if (not while_st_is_terminator) then 
            begin
                (* Set an unconditional branch to the start of the while-block *)
                position_at_end new_do_bb builder; 
                ignore (build_br while_bb builder)
            end;

            (* Finally, set the builder to the end of the merge-block. *)
            position_at_end merge_bb builder;

            false

        | S_return None             ->
            ignore (build_ret_void builder);
            true

        | S_return (Some exr)        ->
            let to_return_llvalue = codegen_expr frame_ptr exr in
            ignore (build_ret to_return_llvalue builder);
            true
    end

and codegen_expr frame_ptr expr_ast = (* never returns a (llvalue) ptr to anything *)
    begin match expr_ast.expr_raw with
        | E_int n                   -> const_int int_type n
        | E_char c                  -> const_int byte_type (Char.code c)
        | E_lvalue lval             ->  begin match lval.l_value_raw with
                                            | L_id _  -> dereference (codegen_lval frame_ptr lval)
                                            | L_str _ -> fatal "codegen_expr: Cannot generate an expression from a string"; raise Terminate
                                        end

        | E_call cl                 -> codegen_call frame_ptr cl
        | E_sign (SPlus,exr)        -> codegen_expr frame_ptr exr
        | E_sign (SMinus,exr)       -> build_neg (codegen_expr frame_ptr exr) "neg" builder
        | E_op (er1, er_op, er2)    ->
            let ller1 = codegen_expr frame_ptr er1 in
            let ller2 = codegen_expr frame_ptr er2 in
            begin match er_op with
                | Plus  -> build_add ller1 ller2 "add" builder
                | Minus -> build_sub ller1 ller2 "sub" builder
                | Mult  -> build_mul ller1 ller2 "mul" builder
                | Div   -> 
                    begin match (er1.expr_type, er2.expr_type) with 
                        | (Some TYPE_int, Some TYPE_int)   -> build_sdiv ller1 ller2 "sdiv" builder
                        | (Some TYPE_byte, Some TYPE_byte) -> build_udiv ller1 ller2 "udiv" builder
                        | _ -> fatal "exprgen div, type mismatch"; raise Terminate
                    end
                | Mod   -> 
                    begin match (er1.expr_type, er2.expr_type) with 
                        | (Some TYPE_int, Some TYPE_int)   -> build_srem ller1 ller2 "smod" builder
                        | (Some TYPE_byte, Some TYPE_byte) -> build_urem ller1 ller2 "umod" builder
                        | _ -> fatal "exprgen mod, type mismatch"; raise Terminate
                    end
            end 
    end

and codegen_lval frame_ptr l_value_ast = (* returns a (llvalue) pointer to the element *)
    let correct_frame = get_deep_access_link frame_ptr l_value_ast.l_value_nesting_diff in

    begin match l_value_ast.l_value_raw with
        | L_id (_, None)      ->    if (l_value_ast.is_ptr)
                                    then(
                                        dereference (get_ptr_to_Nth_element correct_frame l_value_ast.offset)
                                    ) else (
                                        get_ptr_to_Nth_element correct_frame l_value_ast.offset
                                    )
        | L_id (_, Some exr)  -> (* Only arrays here *) if (not l_value_ast.is_ptr) then (fatal "codegen_lval: must be an array");
                                    let lval_exr = codegen_expr frame_ptr exr in
                                    let arr_ptr = dereference (get_ptr_to_Nth_element correct_frame l_value_ast.offset) in
                                    build_in_bounds_gep arr_ptr [|lval_exr|] "ptr to EXPRth element in array" builder

        | L_str str           -> fatal "codegen_lval: Cannot give ptr to string; do it only during calls to existing funcs"; raise Terminate
            
    end

and codegen_cond frame_ptr cond_ast =
    let give_ll_cmp_op alan_op is_signed = match is_signed, alan_op with
        | _, Eq -> Icmp.Eq
        | _, Neq -> Icmp.Ne

        | true, Less -> Icmp.Slt
        | true, Great -> Icmp.Sgt
        | true, LessEq -> Icmp.Sle
        | true, GreatEq -> Icmp.Sge

        | false, Less -> Icmp.Ult
        | false, Great -> Icmp.Ugt
        | false, LessEq -> Icmp.Ule
        | false, GreatEq -> Icmp.Uge
    in

    let is_expr_signed exr = match exr.expr_type with
        | Some TYPE_int     -> true
        | Some TYPE_byte    -> false
        | _                 -> fatal "is_expr_signed: type is not int or byte"; raise Terminate
    in

    begin match cond_ast with
        | C_true                        -> const_int bool_type 1
        | C_false                       -> const_int bool_type 0
        | C_not cnd                     -> build_not (codegen_cond frame_ptr cnd) "not" builder
        | C_compare (er1, cmp_op, er2)  -> build_icmp (give_ll_cmp_op cmp_op (is_expr_signed er1)) (codegen_expr frame_ptr er1) (codegen_expr frame_ptr er2) "icmp" builder
        | C_logic (cnd1, lg_op, cnd2)   ->
            let start_bb = insertion_block builder in
            let the_function = block_parent start_bb in

            let middle_bb = append_block context "middle_bb" the_function in
            let merge_bb = append_block context "merge_bb" the_function in

            (* position_at_end start_bb builder; *)
            let cnd1_llvalue = codegen_cond frame_ptr cnd1 in

            let midway logical_operator = match logical_operator with
                | And ->
                    (* If cnd1 is true then (must compute cnd2) middle_bb else merge_bb *)
                    ignore (build_cond_br cnd1_llvalue middle_bb merge_bb builder);
                    let new_start_bb = insertion_block builder in

                    position_at_end middle_bb builder;
                    let middle_llvalue = build_and cnd1_llvalue (codegen_cond frame_ptr cnd2) "and" builder in
                    ignore (build_br merge_bb builder);
                    let new_middle_bb = insertion_block builder in

                    (new_start_bb, middle_llvalue, new_middle_bb)

                | Or  ->
                    (* If cnd1 is true then (no need to compute cnd2) merge_bb else middle_bb *)
                    ignore (build_cond_br cnd1_llvalue merge_bb middle_bb builder);
                    let new_start_bb = insertion_block builder in

                    position_at_end middle_bb builder;
                    let middle_llvalue = build_or cnd1_llvalue (codegen_cond frame_ptr cnd2) "or" builder in
                    ignore (build_br merge_bb builder);
                    let new_middle_bb = insertion_block builder in
                    
                    (new_start_bb, middle_llvalue, new_middle_bb)
            in

            let (new_start_bb, middle_llvalue, new_middle_bb) = midway lg_op in

            position_at_end merge_bb builder;
            let phi = build_phi [(cnd1_llvalue, new_start_bb) ; (middle_llvalue, new_middle_bb)] "phi" builder in
            position_at_end merge_bb builder;
            phi
    end

let codegen_existing_functions () = 
    
    let codegen_block func_llvalue =
        (* Create function basic-block *)
        let f_bb = append_block context "entry block" func_llvalue in
        position_at_end f_bb builder
    in

    let codegen_declare full_name ret_type pars = 
        let pars_arr = Array.of_list pars in
        let func_lltype = function_type ret_type pars_arr in
        declare_function full_name func_lltype the_module
    in

    let _    = codegen_declare "writeInteger"    proc_type [int_type] in
    let _    = codegen_declare "writeChar"       proc_type [byte_type] in
    let _    = codegen_declare "writeString"     proc_type [pointer_type byte_type] in
    let _    = codegen_declare "readInteger"     int_type  [] in
    let _    = codegen_declare "readChar"        byte_type [] in
    let _    = codegen_declare "readString"      proc_type [int_type; pointer_type byte_type] in
    let _    = codegen_declare "strlen"          int_type  [pointer_type byte_type] in
    let _    = codegen_declare "strcmp"          int_type  [pointer_type byte_type; pointer_type byte_type] in
    let _    = codegen_declare "strcpy"          proc_type [pointer_type byte_type; pointer_type byte_type] in
    let _    = codegen_declare "strcat"          proc_type [pointer_type byte_type; pointer_type byte_type] in

    (* extend (b : byte) : int *)
    let extend          = codegen_declare "extend" int_type [byte_type] in
    let _               = codegen_block extend in
    let extend_par      = param extend 0 in
    let extend_ret      = build_zext extend_par int_type "extend" builder in
    let _               = build_ret extend_ret builder in

    (* writeByte (b : byte) : proc *)
    let writeByte       = codegen_declare "writeByte" proc_type [byte_type] in
    let _               = codegen_block writeByte in
    let writeByte_par   = param writeByte 0 in
    let from_extend     = build_call extend [|writeByte_par|] "extend call" builder in
    let writeInteger    = match lookup_function "writeInteger" the_module with | Some fn -> fn | _ -> fatal "writeInteger missing"; raise Terminate in
    let _               = build_call writeInteger [|from_extend|] "" builder in
    let _               = build_ret_void builder in

    (* shrink (i : int) : byte *)
    let shrink          = codegen_declare "shrink" byte_type [int_type] in
    let _               = codegen_block shrink in
    let shrink_par      = param shrink 0 in
    let shrink_ret      = build_trunc shrink_par byte_type "shrink" builder in
    let _               = build_ret shrink_ret builder in

    (* readByte () : byte *)
    let readByte        = codegen_declare "readByte" byte_type [] in
    let _               = codegen_block readByte in
    let readInteger     = match lookup_function "readInteger" the_module with | Some fn -> fn | _ -> fatal "readInteger missing"; raise Terminate in
    let from_readInt    = build_call readInteger [||] "" builder in
    let from_shrink     = build_call shrink [|from_readInt|] "" builder in
    let _               = build_ret from_shrink builder in
    
    ()
     

let codegen tree =

    (* generate code for existing functions first *)
    codegen_existing_functions ();

    (* Top level function has no parent - assign dummy *)
    tree.parent <- (Some {
        full_name = "";
        func_id = "";
        func_pars = [];
        func_ret_type = TYPE_none;
        func_local = [];
        func_stmt = [];
        func_nesting_scope = 0;
        parent = None;
        frame_type = Some pointer_type bool_type;
        isMain = false;
    });
    
    if(tree.full_name <> "main") then begin
        let main_lltype = function_type proc_type [||] in
        let main_llvalue = declare_function "main" main_lltype the_module in
        let main_bb = append_block context "entry block" main_llvalue in
        
        position_at_end main_bb builder;
        codegen_func tree;
        
        let tree_llvalue = match (lookup_function tree.full_name the_module) with | Some fn -> fn | None -> fatal "main_call: Function not found"; raise Terminate in
        (* let access_link = build_alloca (pointer_type bool_type) "tree access link" builder in *)
        let access_link = const_pointer_null (pointer_type (pointer_type bool_type)) in

        position_at_end main_bb builder;
        ignore (build_call tree_llvalue [|access_link|] "" builder);
        ignore (build_ret_void builder)

    end else begin
        codegen_func tree
    end;

    assert_valid_module the_module;
    the_module

