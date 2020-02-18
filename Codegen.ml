open Llvm
open Ast
open Error

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
        | TYPE_array (elem_type, arr_size)  -> array_type (give_lltype elem_type) arr_size (* element type can only be a basic data type in alan - int or byte *)
    end

let rec give_fret_lltype alan_type =
    begin match alan_type with
        | TYPE_int  -> int_type
        | TYPE_byte -> byte_type
        | TYPE_proc -> proc_type
    end

let rec give_par_lltype_lst par_lst =
    begin match par_lst with
        | pr :: tl ->
            begin match pr.pass_way with
                | PASS_BY_VALUE     -> (give_lltype pr.par_type) :: (give_par_lltype_lst tl)
                | PASS_BY_REFERENCE -> 
                    begin match pr.par_type with
                        | TYPE_array (arr_pr_type, _) -> (pointer_type (give_lltype arr_pr_type)) :: (give_par_lltype_lst tl)
                        | no_arr_pr_type              -> (pointer_type (give_lltype no_arr_pr_type)) :: (give_par_lltype_lst tl)
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

let give_frame_lltype func_ast =

    let par_lltype_lst = give_par_lltype_lst func_ast.func_pars in (* List of parameter lltypes *)
    let locvar_lltype_lst = give_locvar_lltype_lst func_ast.func_local in (* List of local-variable lltyppes (without local funcs) *)
    
    let access_link_type = pointer_type lltype in

    let arr_of_lltypes = Array.of_list ( access_link_type :: (par_lltype_lst @ locvar_lltype_lst) ) in (* Array of lltypes for frame lltype *)
    let ll_frame_type = struct_type context arr_of_lltypes in (* Frame lltype (llvm-struct type) *)

    ll_frame_type

let give_func_lltype func_ast =

    let par_lltype_lst = give_par_lltype_lst func_ast.func_pars in (* List of parameter lltypes *)
    let par_llarr = Array.of_list par_lltype_lst in (* Arrays of the above *)
    
    let fret_lltype = give_fret_lltype func_ast.func_ret_type in (* LLtype of function's return value *)

    let func_lltype = function_type par_llarr fret_lltype in (*Function lltype *)
    (*let func_llvalue = define_function func_ast.func_id func_lltype the_module in (* Function llvalue *)*)

    func_lltype

let get_ptr_to_Nth_element struct_ptr n str = build_struct_gep struct_ptr n str builder

let dereference ptr = build_load ptr "de-reference" builder

let get_Nth_element struct_ptr n str =
    let ptr_to_element = get_ptr_to_Nth_element struct_ptr n str in
    dereference ptr_to_element

(* Note: access_link == frame_ptr *)
let rec get_deep_access_link frame_ptr diff =
    if( diff = 0 ) 
    then begin
        frame_ptr
    end
    else begin
        let first_element = get_Nth_element frame_ptr 0 "access_link" in
        get_deep_access_link (first_element (diff-1))
    end
    
(* End of helping functions *)

let rec codegen_func func_ast =
    
    let func_lltype = give_func_lltype func_ast in (*Function lltype *)
    let func_llvalue = define_function func_ast.func_id func_lltype the_module in (* Function llvalue *)

    (* Generate code for local functions *)
    let gen_loc_func loc = 
        begin match loc with
            | Local_func loc_func -> codegen_func loc_func
            | Local_var _         -> ()  
        end
    in
    List.iter gen_loc_func func_ast.func_local;

    (* Create function basic-block *)
    let f_bb = entry_block func_llvalue in
    position_at_end f_bb builder;

    (* Create frame *)
    let frame_type = give_frame_lltype func_ast in
    let frame_ptr = build_alloca frame_type "frame" builder in

    let store_at valuetostore_llvalue idx =
        let element_ptr_llvalue = build_struct_gep frame_ptr idx "GEP" builder in
        ignore (build_store valuetostore_llvalue element_ptr_llvalue builder)
    in

    (* Store each parameter into the frame *)
    let rec store_par_llvalue_lst_to_frame par_llvalue_lst idx =
        begin match par_llvalue_lst with
            | par_llvalue :: tl ->
                store_at par_llvalue idx;
                store_par_llvalue_lst_to_frame tl (idx+1)
            | [] -> ()
        end
    in
    let par_llvalue_lst = Array.to_list (params func_llvalue) in
    (* Store starting from position 0 -- access link is included*)
    store_par_llvalue_lst_to_frame par_llvalue_lst 0;
    
    (* Generate code for statements *)
    List.iter (codegen_stmt frame_ptr) func_ast.func_stmt ;


and codegen_func_call frame_ptr call_ast =
    let rec give_expr_llvalue_lst expr_lst =
        begin match expr_lst with
            | exr :: tl -> (codegen_expr frame_ptr exr) :: (give_expr_llvalue_lst tl)
            | [] -> []
        end     
    in

    (* https://en.wikipedia.org/wiki/Nested_function *)
    let callee_func = begin match call_ast.callee_func_ast with
        | Some func_ast -> func_ast
        | _             -> fatal "callee func ast error"; exit 1
    end in
    let diff = call_ast.caller_nesting_scope - callee_func.func_nesting_scope + 1  in
    let correct_frame = get_deep_access_link frame_ptr diff in

    let expr_llvalue_lst = correct_frame :: (give_expr_llvalue_lst call_ast.call_expr) in

    let expr_arr = Array.of_list expr_llvalue_lst in
    build_call call_ast.call_id expr_array "call" builder

and codegen_stmt frame_ptr stmt_ast =
    begin match stmt_ast with
        | Null_stmt                 ->
            ()

        | S_assign (lval,exr)        ->
            let exr_llvalue = codegen_expr frame_ptr exr in
            let element_ptr = codegen_lval frame_ptr lval in
            ignore (build_store exr_llvalue element_ptr builder)

        | S_comp st_lst             ->
            List.iter (codegen_stmt frame_ptr) st_lst

        | S_call fcall              -> (* Call to a void function *)
            ignore (codegen_call frame_ptr fcall)

        | S_if (cnd, st, st_option) ->
            let cond_val = codegen_cond frame_ptr cnd in

            (* Grab the first block so that we might later add the conditional branch
            * to it at the end of the function. *)
            let start_bb = insertion_block builder in
            let the_function = block_parent start_bb in

            let then_bb = append_block context "then" the_function in
            let merge_bb = append_block context "ifcont" the_function in

            position_at_end then_bb builder;
            ignore (codegen_stmt st);
            let new_then_bb = insertion_block builder in (* Codegen of 'then' can change the current block *)

            (* Set an unconditional branch at the end of the then-block to the merge-block *)
            position_at_end new_then_bb builder; ignore (build_br merge_bb builder);

            begin match st_option with
                | Some st_some ->
                    let else_bb = append_block context "else" the_function in

                    position_at_end else_bb builder;
                    ignore (codegen_stmt st_some);
                    let new_else_bb = insertion_block builder in

                    (* Set an unconditional branch at the end of the else-block to the merge-block*)
                    position_at_end new_else_bb builder; 
                    ignore (build_br merge_bb builder);
                    
                    (* Return to the end of the start-block to add the conditional branch *)
                    position_at_end start_bb builder;
                    ignore (build_cond_br cond_val then_bb else_bb builder);

                | None ->
                    (* Return to the end of the start-block to add the conditional branch *)
                    position_at_end start_bb builder;
                    ignore (build_cond_br cond_val then_bb merge_bb builder);

            end
            (* Finally, set the builder to the end of the merge-block *)
            position_at_end merge_bb builder

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
            ignore (codegen_stmt st);
            let new_do_bb = insertion_block builder in (* Codegen of 'do' can change the current block *)
            (* Set an unconditional branch to the start of the while-block *)
            position_at_end new_do_bb builder; 
            ignore (build_br while_bb builder);

            (* Finally, set the builder to the end of the merge-block. *)
            position_at_end merge_bb builder

        | S_return None             ->
            ignore (build_ret_void builder)
        | S_return (Some exr)        ->
            let to_return_llvalue = codegen_expr frame_ptr exr in
            ignore (build_ret to_return_llvalue)

(* TO CHECK THIS -- need to add frame *)
and codegen_expr frame_ptr expr_ast = 
    begin match expr_ast.expr_raw with
        | E_int n                   -> const_int int_type n
        | E_char c                  -> const_int byte_type c
        | E_val v                   -> dereference (codegen_lval frame_ptr v)
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
                        | (TYPE_int, TYPE_int)   -> build_sdiv ller1 ller2 "sdiv" builder
                        | (TYPE_byte, TYPE_byte) -> build_udiv ller1 ller2 "udiv" builder
                    end
                | Mod   -> 
                    begin match (er1.expr_type, er2.expr_type) with 
                        | (TYPE_int, TYPE_int)   -> build_srem ller1 ller2 "smod" builder
                        | (TYPE_byte, TYPE_byte) -> build_urem ller1 ller2 "umod" builder
                    end
            end 
    end

and codegen_lval frame_ptr l_value_ast = (* This will always return a pointer to an element *)
    let correct_frame = get_deep_access_link frame_ptr l_value_ast.l_value_nesting_diff in

    begin match l_value_ast.l_value_raw with
        | L_exp (lval_id, None) ->
            begin match l_value_ast.is_parameter, l_value_ast.is_local with
                | true, false -> (* Parameters *)
                    begin match l_value_ast.l_value_type with
                        | Some (TYPE_array (arr_typ,_)) -> (* Arrays are passed by reference only - They are pointers to the first element (of the array) in the frames *)
                            get_Nth_element correct_frame l_value_ast.offset lval_id
                        | Some _                        ->
                            begin match l_value_ast.is_reference with
                                | true  -> get_Nth_element correct_frame l_value_ast.offset lval_id
                                | false -> get_ptr_to_Nth_element correct_frame l_value_ast.offset lval_id
                            end
                        | _                             -> fatal "none,params"; llvalue
                    end
                | false, true -> (* Locals *)
                    begin match l_value_ast.l_value_type with
                        | Some (TYPE_array (arr_typ,_)) -> (* Array_type in frame here -- get pointer to the first element of the array *)
                            let array_in_frame = get_Nth_element correct_frame l_value_ast.offset lval_id in
                            get_ptr_to_Nth_element array_in_frame 0 lval_id
                        | Some _                        -> (* No references here *)
                            get_ptr_to_Nth_element correct_frame l_value_ast.offset lval_id
                        | _                             -> fatal "none, locals"; llvalue
                    end
                | _           -> fatal "none, false, false"; llvalue
            end

        | L_exp (lval_id,Some exr) -> (* Only arrays here *)
            begin match l_value_ast.is_parameter, l_value_ast.is_local with
                | true, false -> (* Parameters *)
                    let exr_llvalue = codegen_expr frame_ptr exr in
                    let arr_ptr = get_Nth_element correct_frame l_value_ast.offset lval_id in
                    build_gep arr_ptr [|exr_llvalue|] "identifier with some expression-param" builder

                | false, true -> (* Locals *)
                    let exr_llvalue = codegen_expr frame_ptr exr in
                    let array_in_frame = get_Nth_element correct_frame l_value_ast.offset lval_id in
                    let arr_ptr = get_ptr_to_Nth_element array_in_frame 0 lval_id in
                    build_gep arr_ptr [|exr_llvalue|] "identifier with some expression-local" builder

                | _           -> fatal "some, false, false"; llvalue
            end

        | L_str str                 -> 
            let global_str = build_global_string str "string to build" builder in
            build_struct_gep global_str 0 "string as a pointer to the first element of an array of chars" builder
    end

and codegen_cond frame_ptr cond_ast =
    let give_ll_cmp_op alan_op is_signed =
        begin match is_signed, alan_op with
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
        end
    in
    let is_expr_signed exr =
        begin match exr.expr_type with
            | Some TYPE_int     -> true
            | Some TYPE_byte    -> false
        end
    in
    begin match cond_ast with
        | C_true    -> const_int bool_type 1
        | C_false   -> const_int bool_type 0
        | C_not cnd -> build_not (codegen_cond frame_ptr cnd) "not" builder
        | C_compare (er1, cmp_op, er2) -> 
            build_icmp (give_ll_cmp_op cmp_op (is_expr_signed er1)) (codegen_expr frame_ptr er1) (codegen_expr frame_ptr er2) "icmp" builder
        | C_logic (cnd1, lg_op, cnd2) ->
            let start_bb = insertion_block builder in
            let the_function = block_parent start_bb in

            let middle_bb = append_block context the_function "middle_bb" in
            let merge_bb  = append_block context the_function "merge_bb"  in

            (* position_at_end start_bb builder; *)
            let cnd1_llvalue = codegen_cond frame_ptr cnd1 in

            (* Helper function *)
            let finish new_start_bb middle_llvalue new_middle_bb =
                position_at_end merge_bb;
                let phi = build_phi [(cnd1_llvalue, new_start_bb) ; (middle_llvalue, new_middle_bb)] "phi" builder in
                position_at_end merge_bb;
                phi
            in
            (* End of helper *)

            begin match lg_op with
                | And ->
                    (* If cnd1 is true then (must compute cnd2) middle_bb else merge_bb *)
                    build_cond_br cnd1_llvalue middle_bb merge_bb;
                    let new_start_bb = insertion_block builder in

                    position_at_end middle_bb;
                    let middle_llvalue = build_and cnd1_llvalue (codegen_cond frame_ptr cnd2) "and" builder in
                    build_br merge_bb builder;
                    let new_middle_bb = insertion_block builder in

                    finish new_start_bb middle_llvalue new_middle_bb
                | Or  ->
                    (* If cnd1 is true then (no need to compute cnd2) merge_bb else middle_bb *)
                    build_cond_br cnd1_llvalue merge_bb middle_bb;
                    let new_start_bb = insertion_block builder in

                    position_at_end middle_bb;
                    let middle_llvalue = build_or cnd1_llvalue (codegen_cond frame_ptr cnd2) "or" builder in
                    build_br merge_bb builder;
                    let new_middle_bb = insertion_block builder in

                    finish new_start_bb middle_llvalue new_middle_bb
            end            
    end