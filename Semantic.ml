open Format

open Identifier
open Types
open Symbol
open Ast
open Error

let show_offsets = true

let rec pretty_typ ppf typ =
  match typ with
  | TYPE_none ->
      fprintf ppf "<undefined>"
  | TYPE_int ->
      fprintf ppf "int"
  | TYPE_byte ->
      fprintf ppf "byte"
  | TYPE_array (et, sz) ->
      pretty_typ ppf et;
      if sz > 0 then
        fprintf ppf " [%d]" sz
      else
        fprintf ppf " []"
  | TYPE_proc ->
      fprintf ppf "proc"

let pretty_mode ppf mode =
  match mode with
  | PASS_BY_REFERENCE ->
      fprintf ppf "reference "
  | _ ->
      ()

let printSymbolTable () =
  let rec walk ppf scp =
    if scp.sco_nesting <> 0 then begin
      fprintf ppf "scope: ";
      let entry ppf e =
        fprintf ppf "%a" pretty_id e.entry_id;
        match e.entry_info with
        | ENTRY_none ->
            fprintf ppf "<none>"
        | ENTRY_variable inf ->
            if show_offsets then
              fprintf ppf "[%d]" inf.variable_offset
        | ENTRY_function inf ->
            let param ppf e =
              match e.entry_info with
                | ENTRY_parameter inf ->
                   fprintf ppf "%a%a : %a"
                      pretty_mode inf.parameter_mode
                      pretty_id e.entry_id
                      pretty_typ inf.parameter_type
                | _ ->
                    fprintf ppf "<invalid>" in
            let rec params ppf ps =
              match ps with
              | [p] ->
                  fprintf ppf "%a" param p
              | p :: ps ->
                  fprintf ppf "%a; %a" param p params ps;
              | [] ->
                  () in
            fprintf ppf "(%a) : %a"
              params inf.function_paramlist
              pretty_typ inf.function_result
        | ENTRY_parameter inf ->
            if show_offsets then
              fprintf ppf "[%d]" inf.parameter_offset
        | ENTRY_temporary inf ->
            if show_offsets then
              fprintf ppf "[%d]" inf.temporary_offset in
      let rec entries ppf es =
        match es with
          | [e] ->
              fprintf ppf "%a" entry e
          | e :: es ->
              fprintf ppf "%a, %a" entry e entries es;
          | [] ->
              () in
      match scp.sco_parent with
      | Some scpar ->
          fprintf ppf "%a\n%a"
            entries scp.sco_entries
            walk scpar
      | None ->
          fprintf ppf "<impossible>\n"
    end in
  let scope ppf scp =
    if scp.sco_nesting == 0 then
      fprintf ppf "no scope\n"
    else
      walk ppf scp in
  printf "%a----------------------------------------\n"
    scope !currentScope

(* Start *)
let funTypeStack = Stack.create ()

let print_offsets_in_ast func_ast =
    printf "%s: \n" func_ast.full_name;
    printf "%s: \n" "Parameters";
    let print_par par = ignore (printf "%s: %d \n" par.par_id par.par_offset) in
    List.iter print_par func_ast.func_pars; 

    printf "%s: \n" "Locals";
    let print_var locvar = match locvar with | Local_func _ -> () | Local_var var -> ignore (printf "%s: %d \n" var.var_id var.var_offset) in
    List.iter print_var func_ast.func_local;
    ignore (printf "%s \n" " ")

let myPerr fnm str msg = printf "\nIn function: '%s': '%s'\nMessage: %s\n" fnm str msg

let rec make_cond fnm cond_ast =
    begin match cond_ast with
        | C_true ->
            ()
            
        | C_false ->
            ()
            
        | (C_not cond_cond) ->
            make_cond fnm cond_cond
            
        | C_compare (cond_expr_left, cond_compare_op, cond_expr_right) ->
            make_expr fnm cond_expr_left;
            make_expr fnm cond_expr_right;
            if( (cond_expr_left.expr_type = Some TYPE_int || cond_expr_left.expr_type = Some TYPE_byte) && cond_expr_left.expr_type = cond_expr_right.expr_type )
            then ( () )
            else ( myPerr fnm "" "Compare type mismatch"; fatal "Compare type mismatch" )
            
        | C_logic (cond_cond_left, cond_logic_op, cond_cond_right) ->
            make_cond fnm cond_cond_left;
            make_cond fnm cond_cond_right
    end

(*check if parameter call is ok*)
and make_call fnm call_ast =
    let e = lookupEntry (id_make call_ast.call_id) LOOKUP_ALL_SCOPES true in
    begin match e.entry_info with
        | ENTRY_function func_entry ->
            call_ast.callee_full_name     <- func_entry.function_full_name;
            call_ast.callee_scope         <- e.entry_scope.sco_nesting;
            call_ast.caller_nesting_scope <- !currentScope.sco_nesting - 1;

            List.iter (make_expr fnm) call_ast.call_expr;
            
            let get_param_typ param_entry = match param_entry.entry_info with 
                                        | ENTRY_parameter par -> (par.parameter_type,par.parameter_mode)
                                        | _ -> fatal "Parameter declaration gone wrong -- Why?"; (TYPE_none,PASS_BY_VALUE)
            in
            let get_pass_mode_from tupl = match tupl with | (_,parmode) -> parmode in

            let get_actual_param_typ expr_ = match expr_.expr_type with
                                            | Some ex_type_some -> (ex_type_some,expr_.expr_raw)
                                            | None -> fatal "Sth gone terribly wrong"; (TYPE_none,E_int 0)
            in
            
            let declared_param_typ_lst = List.map get_param_typ func_entry.function_paramlist in

            let only_pass_mode_lst = List.map get_pass_mode_from declared_param_typ_lst in
            call_ast.declared_pars <- Some only_pass_mode_lst;

            let actual_param_typ_lst = List.map get_actual_param_typ call_ast.call_expr in
            
            let equal_declared_actual (dec_type,dec_mode) (act_type,act_raw) = 
                match (dec_mode, act_raw) with
                    | (PASS_BY_VALUE, _) -> equalType dec_type act_type
                    | (PASS_BY_REFERENCE, E_lvalue _) -> equalType dec_type act_type
                    | _ -> false
            in
            
            let myPrint_dec (s,info) =
                pretty_typ std_formatter s;
                printf " ";
                match info with
                    | PASS_BY_VALUE -> printf "%s\n" " - [PASS_BY_VALUE]"
                    | PASS_BY_REFERENCE -> printf "%s\n" " - [PASS_BY_REFERENCE]"
            in
            
            let myPrint_act (s,info) =
                pretty_typ std_formatter s;
                printf " ";
                match info with
                    | E_lvalue _ -> printf "%s\n" " - [L_value]"
                    | _ -> printf "%s\n" " - [NOT_L_value]"
            in

            if( ( List.length declared_param_typ_lst = List.length actual_param_typ_lst ) && 
                ( List.for_all2 equal_declared_actual declared_param_typ_lst actual_param_typ_lst )
                )
            
            then ( call_ast.return_type <- Some func_entry.function_result )
            
            else(
                myPerr fnm call_ast.call_id "Incorrect argument format";
                printf "%s\n" "Expected parameters: ";
                List.iter myPrint_dec declared_param_typ_lst;
                print_newline ();
                printf "%s\n" "Given parameters:    "; 
                List.iter myPrint_act actual_param_typ_lst;
                print_newline ();
                fatal "Incorrect argument format" 
            )
            
        | _ ->
            myPerr fnm call_ast.call_id "Call of a non-function"; fatal "Call of a non-function"
    end

and make_expr fnm expr_ast = 
    begin match expr_ast.expr_raw with
        | E_int ex_int -> 
            expr_ast.expr_type <- Some TYPE_int
            
        | E_char ex_char -> 
            expr_ast.expr_type <- Some TYPE_byte
            
        | E_lvalue ex_l_value ->
            make_l_value fnm ex_l_value;
            expr_ast.expr_type <- ex_l_value.l_value_type
            
        | E_call ex_func_call ->
            make_call fnm ex_func_call;
            expr_ast.expr_type <- ex_func_call.return_type;
            if( ex_func_call.return_type = Some TYPE_proc ) then ( myPerr fnm ex_func_call.call_id "This is an expression and function cannot be a procedure"; fatal "This is an expression and function cannot be a procedure" )
            
        | E_sign (ex_sign, ex_expr) ->
            make_expr fnm ex_expr;
            if(ex_expr.expr_type = Some TYPE_int)
            then (expr_ast.expr_type <- ex_expr.expr_type)
            else (myPerr fnm "" "Sign must be followed by an integer"; fatal "Sign must be followed by an integer")
            
        | E_op (ex_expr_left, ex_op, ex_expr_right) ->
            make_expr fnm ex_expr_left;
            make_expr fnm ex_expr_right;
            
            if((ex_expr_left.expr_type = Some TYPE_int || ex_expr_left.expr_type = Some TYPE_byte) && ex_expr_left.expr_type = ex_expr_right.expr_type)
            then (expr_ast.expr_type <- ex_expr_left.expr_type)
            else ( myPerr fnm "" "Operation type mismatch"; fatal "Operation type mismatch")
    end

and make_l_value fnm l_value_ast =
    begin match l_value_ast.l_value_raw with
        | L_id (lval_id, lval_expr_opt) ->
            let e = lookupEntry (id_make lval_id) LOOKUP_ALL_SCOPES true in
            l_value_ast.l_value_nesting_diff <- ( !currentScope.sco_nesting - e.entry_scope.sco_nesting );

            let e_typ = match e.entry_info with
                | ENTRY_variable var_info ->
                    l_value_ast.offset <- var_info.variable_offset;
                    l_value_ast.is_local <- true;
                    let info_var_type = var_info.variable_type in
                    let _ = begin match info_var_type with 
                        | TYPE_array _  -> l_value_ast.is_ptr <- true
                        | _             -> ()
                    end in
                    Some info_var_type

                | ENTRY_parameter par_info ->
                    l_value_ast.offset <- par_info.parameter_offset;
                    let _ = begin match par_info.parameter_mode with
                        | PASS_BY_REFERENCE -> (l_value_ast.is_ptr <- true) (* arrays are always by reference *)
                        | _                 -> ()
                    end in
                    l_value_ast.is_parameter <- true;
                    Some par_info.parameter_type

                | _ ->
                    myPerr fnm lval_id "Identifier does not exist"; 
                    fatal "Identifier not valid";
                    None
            in

            begin match lval_expr_opt with
                | Some lval_expr_some -> 
                    (*Check if not array*)
                    begin match e_typ with
                        | Some (TYPE_array (elem_typ,_)) -> 
                            make_expr fnm lval_expr_some;
                            (*Check if index is int*)
                            if( lval_expr_some.expr_type <> Some TYPE_int )
                            then ( myPerr fnm lval_id "Array index must be an integer"; fatal "Array index must be an integer" );
                            l_value_ast.l_value_type <- Some elem_typ                                
                            
                        | _ -> 
                            myPerr fnm lval_id "Variable not an array"; fatal "Variable not an array"
                    end
                | None ->
                    l_value_ast.l_value_type <- e_typ
            end
            
        | L_str lval_str -> (*String literal = TYPE_array (TYPE_byte,size < 0)*)
            l_value_ast.l_value_type <- Some TYPE_array (TYPE_byte,-1)
    end

and make_stmt fnm stmt_ast = 
    begin match stmt_ast with
        | Null_stmt -> 
            ()
            
        | S_assign (st_l_value, st_expr) ->
            make_expr fnm st_expr;
            make_l_value fnm st_l_value;
            begin match (st_l_value.l_value_type, st_expr.expr_type) with
                | (Some TYPE_array (TYPE_byte,siz),_)   -> if( siz < 0 ) then (myPerr fnm (match st_l_value.l_value_raw with | L_str str -> str | _ -> "") "Cannot assign value to a string literal"; fatal "Cannot assign value to a string literal" )
                | (Some TYPE_proc,_)                    -> fatal "L value is a procedure -- How did this happen?" (*Obsolete*)
                | (_,Some TYPE_proc)                    -> fatal "Cannot assign a procedure" (*Obsolete*)
                | (typ1,typ2)                           -> if(typ1 <> typ2) then ( myPerr fnm (match st_l_value.l_value_raw with | L_id (x,_) -> x | _ -> "") "Assignment type mismatch"; fatal "Assignment type mismatch" )
            end
            
        | S_comp st_stmt_lst ->
            List.iter (make_stmt fnm) st_stmt_lst
            
        | S_call st_func_call -> 
            make_call fnm st_func_call;
            if( st_func_call.return_type <> Some TYPE_proc ) then ( myPerr fnm st_func_call.call_id "This is a statement, function must be a procedure"; fatal "This is a statement, function must be a procedure" )
            
        | S_if (st_cond,st_stmt,st_stmt_opt) ->
            make_cond fnm st_cond;
            make_stmt fnm st_stmt;
            begin match st_stmt_opt with
                | Some st_stmt_some -> 
                    make_stmt fnm st_stmt_some
                | None -> 
                    ()
            end
                
        | S_while (st_cond,st_stmt) -> 
            make_cond fnm st_cond;
            make_stmt fnm st_stmt
            
        | S_return st_expr_opt -> 
            let funTypeTop = Stack.pop funTypeStack in
            begin match st_expr_opt with
                | Some st_expr_some ->
                    Stack.push funTypeTop funTypeStack;
                    if( funTypeTop = TYPE_proc )
                    then (myPerr fnm "" "Procedure cannot return a value"; fatal "Procedure cannot return a value" )
                    else ( make_expr fnm st_expr_some; if( st_expr_some.expr_type <> Some funTypeTop ) then ( myPerr fnm "" "Return type mismatch"; fatal "Return type mismatch" ) )
                | None ->
                    Stack.push funTypeTop funTypeStack;
                    if( funTypeTop <> TYPE_proc )
                    then ( myPerr fnm "" "Return statement cannot be empty in this function"; fatal "Return statement cannot be empty in this function" )
            end    
    end

and make_par f par_ast =
    let fnm = match f.entry_info with | ENTRY_function finf -> ( match finf.function_full_name with | Some nm -> nm | _ -> "BAD-NONE" ) | _ -> "BAD" in
    begin match (par_ast.par_type,par_ast.par_pass_way) with
        | (TYPE_array _, PASS_BY_VALUE) -> myPerr fnm par_ast.par_id "An array cannot be passed by value"; fatal "An array cannot be passed by value"
        | _ ->  
            let newPar = newParameter (id_make par_ast.par_id) par_ast.par_type par_ast.par_pass_way f true in
            par_ast.symb_id <- Some newPar.entry_id
    end

and make_local fnm local_ast = 
    begin match local_ast with
        | Local_func loc_func ->
            make_func loc_func;
            ()
        | Local_var loc_var ->
            (*In case an array is declared, its size needs to be a positive integer*)
            begin match loc_var.var_type with
                | TYPE_array (_,siz)    -> if(siz <= 0) then (myPerr fnm loc_var.var_id "Array size must be a positive integer"; fatal "Array size must be a positive integer")
                | _                     -> ()
            end;
            let newVar = newVariable (id_make loc_var.var_id) loc_var.var_type true in
            loc_var.var_offset <- (match newVar.entry_info with | ENTRY_variable inf -> inf.variable_offset | _ -> fatal "var bad luck"; raise Terminate)
    end

and make_func func_ast =

    let f_SYM = newFunction (id_make func_ast.func_id) true in
    let _ = begin match f_SYM.entry_info with
        | ENTRY_function func_info -> func_info.function_full_name <- Some func_ast.full_name
        | _                        -> fatal "entry must have been function_entry, ??"
    end in
    Stack.push func_ast.func_ret_type funTypeStack;
    (* Nesting *)
    func_ast.func_nesting_scope <- f_SYM.entry_scope.sco_nesting;
    
    openScope ();
    
    List.iter (make_par f_SYM) func_ast.func_pars;
    endFunctionHeader f_SYM func_ast.func_ret_type;

    let set_parent_and_full_name loc = 
        begin match loc with
            | Local_func loc_func -> 
                loc_func.parent <- Some func_ast;
                let new_name = String.concat "#" [func_ast.full_name ; loc_func.func_id] in
                loc_func.full_name <- new_name
            | Local_var _         -> ()
        end
    in
    List.iter set_parent_and_full_name func_ast.func_local;
    List.iter (make_local func_ast.full_name) func_ast.func_local;
    
    (* Helping func *)
    let set_par_offset par_ast = 
        let par_symb_id = match par_ast.symb_id with | Some sid -> sid | None -> fatal "symb id bad luck "; raise Terminate in
        let parEntry = lookupEntry par_symb_id LOOKUP_CURRENT_SCOPE false in
        let parOffset = match parEntry.entry_info with | ENTRY_parameter inf -> inf.parameter_offset | _ -> fatal "par bad luck "; raise Terminate in
        par_ast.par_offset <- parOffset
    in
    List.iter set_par_offset func_ast.func_pars;

    (* print_offsets_in_ast func_ast; *)

    List.iter (make_stmt func_ast.full_name) func_ast.func_stmt;
    
    (* printSymbolTable (); *)
    closeScope ();
    ignore (Stack.pop funTypeStack)


let init_existing_functions () =
    begin   
        (* Helping function *)
        let set_full_name fsym str = 
            begin match fsym.entry_info with
                | ENTRY_function func_info -> func_info.function_full_name <- Some str
                | _                        -> fatal "entry must have been function_entry, ??"; raise Terminate
            end
        in
        (********* add existing functions **********************************************************************************************************************)
    
        (***************** writeInteger (n : int) : proc *******************************************************************************************************)
            let f_wInt = newFunction (id_make "writeInteger") true in
            set_full_name f_wInt "writeInteger";
            openScope ();
            
            make_par f_wInt {par_id = "n"; par_pass_way = PASS_BY_VALUE; par_type = TYPE_int; par_offset = 0; symb_id = None;};
            endFunctionHeader f_wInt TYPE_proc;
            
            closeScope ();
            
        (***************** writeByte (b : byte) : proc *********************************************************************************************************)
            let f_wByte = newFunction (id_make "writeByte") true in
            set_full_name f_wByte "writeByte";
            openScope ();
            
            make_par f_wByte {par_id = "b"; par_pass_way = PASS_BY_VALUE; par_type = TYPE_byte; par_offset = 0; symb_id = None;};
            endFunctionHeader f_wByte TYPE_proc;
            
            closeScope ();
            
        (***************** writeChar (b : byte) : proc *********************************************************************************************************)
            let f_wChar = newFunction (id_make "writeChar") true in
            set_full_name f_wChar "writeChar";
            openScope ();
            
            make_par f_wChar {par_id = "b"; par_pass_way = PASS_BY_VALUE; par_type = TYPE_byte; par_offset = 0; symb_id = None;};
            endFunctionHeader f_wChar TYPE_proc;
            
            closeScope ();
            
        (***************** writeString (s : reference byte []) : proc ******************************************************************************************)
            let f_wStr = newFunction (id_make "writeString") true in
            set_full_name f_wStr "writeString";
            openScope ();
            
            make_par f_wStr {par_id = "s"; par_pass_way = PASS_BY_REFERENCE; par_type = TYPE_array (TYPE_byte,-1); par_offset = 0; symb_id = None;};
            endFunctionHeader f_wStr TYPE_proc;
            
            closeScope ();
            
        (***************** readInteger () : int ****************************************************************************************************************)
            let f_rInt = newFunction (id_make "readInteger") true in
            set_full_name f_rInt "readInteger";
            openScope ();
            
            endFunctionHeader f_rInt TYPE_int;
            
            closeScope ();
            
        (***************** readByte () : byte ******************************************************************************************************************)
            let f_rByte = newFunction (id_make "readByte") true in
            set_full_name f_rByte "readByte";
            openScope ();
            
            endFunctionHeader f_rByte TYPE_byte;
            
            closeScope ();
            
        (***************** readChar () : byte ******************************************************************************************************************)
            let f_rChar = newFunction (id_make "readChar") true in
            set_full_name f_rChar "readChar";
            openScope ();
            
            endFunctionHeader f_rChar TYPE_byte;
            
            closeScope ();
            
        (***************** readString (n : int, s : reference byte []) : proc **********************************************************************************)
            let f_rStr = newFunction (id_make "readString") true in
            set_full_name f_rStr "readString";
            openScope ();
            
            List.iter (make_par f_rStr) [{par_id = "n"; par_pass_way = PASS_BY_VALUE; par_type = TYPE_int; par_offset = 0; symb_id = None;};
                                        {par_id = "s"; par_pass_way = PASS_BY_REFERENCE; par_type = TYPE_array (TYPE_byte,-1); par_offset = 1; symb_id = None;}];
            endFunctionHeader f_rStr TYPE_proc;
            
            closeScope ();
            
        (***************** extend (b : byte) : int *************************************************************************************************************)
            let f_ext = newFunction(id_make "extend") true in
            set_full_name f_ext "extend";
            openScope ();
            
            make_par f_ext {par_id = "b"; par_pass_way = PASS_BY_VALUE; par_type = TYPE_byte; par_offset = 0; symb_id = None;};
            endFunctionHeader f_ext TYPE_int;
            
            closeScope ();
            
        (***************** shrink (i : int) : byte *************************************************************************************************************)
            let f_shrink = newFunction(id_make "shrink") true in
            set_full_name f_shrink "shrink";
            openScope ();
            
            make_par f_shrink {par_id = "i"; par_pass_way = PASS_BY_VALUE; par_type = TYPE_int; par_offset = 0; symb_id = None;};
            endFunctionHeader f_shrink TYPE_byte;
            
            closeScope ();
            
        (***************** strlen (s : reference byte []) : int ************************************************************************************************)
            let f_len = newFunction(id_make "strlen") true in
            set_full_name f_len "strlen";
            openScope ();
            
            make_par f_len {par_id = "s"; par_pass_way = PASS_BY_REFERENCE; par_type = TYPE_array (TYPE_byte,-1); par_offset = 0; symb_id = None;};
            endFunctionHeader f_len TYPE_int;
            
            closeScope ();
            
        (***************** strcmp (s1 : reference byte [], s2 : reference byte []) : int ***********************************************************************)
            let f_cmp = newFunction (id_make "strcmp") true in
            set_full_name f_cmp "strcmp";
            openScope ();
            
            List.iter (make_par f_cmp) [{par_id = "s1"; par_pass_way = PASS_BY_REFERENCE; par_type = TYPE_array (TYPE_byte,-1); par_offset = 0; symb_id = None;};
                                        {par_id = "s2"; par_pass_way = PASS_BY_REFERENCE; par_type = TYPE_array (TYPE_byte,-1); par_offset = 1; symb_id = None;}];
            endFunctionHeader f_cmp TYPE_int;
            
            closeScope ();
            
        (***************** strcpy (trg : reference byte [], src : reference byte []) : proc ********************************************************************)
            let f_cpy = newFunction (id_make "strcpy") true in
            set_full_name f_cpy "strcpy";
            openScope ();
            
            List.iter (make_par f_cpy) [{par_id = "trg"; par_pass_way = PASS_BY_REFERENCE; par_type = TYPE_array (TYPE_byte,-1); par_offset = 0; symb_id = None;};
                                        {par_id = "src"; par_pass_way = PASS_BY_REFERENCE; par_type = TYPE_array (TYPE_byte,-1); par_offset = 1; symb_id = None;}];
            endFunctionHeader f_cpy TYPE_proc;
            
            closeScope ();
            
        (***************** strcat (trg : reference byte [], src : reference byte []) : proc ********************************************************************)
            let f_cat = newFunction (id_make "strcat") true in
            set_full_name f_cat "strcat";
            openScope ();
            
            List.iter (make_par f_cat) [{par_id = "trg"; par_pass_way = PASS_BY_REFERENCE; par_type = TYPE_array (TYPE_byte,-1); par_offset = 0; symb_id = None;};
                                        {par_id = "src"; par_pass_way = PASS_BY_REFERENCE; par_type = TYPE_array (TYPE_byte,-1); par_offset = 1; symb_id = None;}];
            endFunctionHeader f_cat TYPE_proc;

            closeScope ();
            
        (********* end of existing functions *******************************************************************************************************************)
    end

let seman tree =

    initSymbolTable 256;

    openScope ();

    init_existing_functions ();
    make_func tree;
    if(tree.full_name <> "main" && List.length tree.func_pars > 0 ) then (fatal "Top-level function must be named 'main' if arguments are given"; raise Terminate);

    closeScope ();