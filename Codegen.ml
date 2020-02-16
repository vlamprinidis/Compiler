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
            let pr_lltyp = give_lltype pr.par_type in
            begin match pr.pass_way with
                | PASS_BY_VALUE     -> pr_lltyp :: (give_par_lltype_lst tl)
                | PASS_BY_REFERENCE -> (pointer_type pr_lltyp) :: (give_par_lltype_lst tl)
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

(* End of helping functions *)
(* Functions to be created here *)
let find_func_from_call call_ast = exit 1

let rec codegen_func access_link func_ast =
    let func_lltype = give_func_lltype func_ast in (*Function lltype *)
    let func_llvalue = define_function func_ast.func_id func_lltype the_module in (* Function llvalue *)

    (* Basic block *)
    let f_bb = entry_block func_llvalue in
    position_at_end f_bb builder;

    (* Create frame *)
    let frame_type = give_frame_lltype func_ast in
    let frame_llvalue = build_alloca frame_type "frame" builder in

    let store_at valuetostore_llvalue idx =
        let element_ptr_llvalue = build_struct_gep frame_llvalue idx "GEP" builder in
        let _ = build_store valuetostore_llvalue element_ptr_llvalue builder in
        ()
    in

    (* Store access_link at 0 position in frame *)
    store_at access_link 0;

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
    let start_of_locals = (List.length par_llvalue_lst) + 1 in (* For later *)
    (* Store starting from position 1 *)
    store_par_llvalue_lst_to_frame par_llvalue_lst 1;

    (* Create pointer to it *)

    
   

    List.iter (codegen_stmt ll_frame) func_ast.func_stmt ;

and rec codegen_par par_ast =

and rec codegen_local local_ast = 

and rec codegen_func_call access_link call_ast =
    let rec give_expr_list =
        exit 1     
    in

    let expr_list = give_expr_list in

    let expr_arr = Array.of_list expr_list in
    build_call func expr_array "call" builder

    

and rec codegen_stmt ll_frame stmt_ast =
    begin match stmt_ast with
        | Null_stmt                 ->
            ()

        | S_assign (lval,er)        ->

        | S_comp st_lst             ->
            List.iter (codegen_stmt ll_frame) st_lst

        | S_call fcall              ->
        | S_if (cnd, st, st_option) ->
            let cond_val = codegen_cond cnd in

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
            let cond_val = codegen_cond cnd in
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
        | S_return (Some er)        ->

(* TO CHECK THIS -- need to add frame *)
and rec codegen_expr expr_ast = 
    begin match expr_ast.expr_raw with
        | E_int n                   -> const_int int_type n
        | E_char c                  -> const_int byte_type c
        | E_val v                   -> codegen_lval v
        | E_call cl                 -> codegen_call cl
        | E_sign (SPlus,er)         -> codegen_expr er
        | E_sign (SMinus,er)        -> build_neg (codegen_expr er) "neg" builder
        | E_op (er1, er_op, er2)    ->
            let ller1 = codegen_expr er1 in
            let ller2 = codegen_expr er2 in
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

(* TO COMPLETE THIS *)
and rec codegen_lval l_value_ast current_frame =
    let find_var id nest_scope = 

    in
    (*let find_arr_el id *)
    begin match l_value_ast.l_value_raw with
        (*to do*)
        | L_exp (lval_id,None)      -> find_var lval_id (l_value_ast.l_value_nesting_scope)
        | L_exp (lval_id,Some expr) -> find_arr_el lval_id (get_offset expr) (l_value_ast.l_value_nesting_scope)
        | L_str str                 -> do_str str
    end

(* TO FIX THIS *)
and rec codegen_cond cond_ast =
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
    let is_expr_signed er =
        begin match er.expr_type with
            | Some TYPE_int     -> true
            | Some TYPE_byte    -> false
        end
    in
    begin match cond_ast with
        | C_true    -> const_int bool_type 1
        | C_false   -> const_int bool_type 0
        | C_not cnd -> build_not (codegen_cond cnd) "not" builder
        | C_compare (er1, cmp_op, er2) -> 
            build_icmp (give_ll_cmp_op cmp_op (is_expr_signed er1)) (codegen_expr er1) (codegen_expr er2) "icmp" builder
        | C_logic (cnd1, lg_op, cnd2) ->
            begin match lg_op with
                | And ->
                    build_and (codegen_cond cnd1) (codegen_cond cnd2) "and" builder
                | Or ->
                    build_or (codegen_cond cnd1) (codegen_cond cnd2) "or" builder
            end
    end