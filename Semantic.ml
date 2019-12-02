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

let funTypeStack = Stack.create ()


    
let seman tree =

    (* help funcs *)
    
    let rec make_cond cond_ast =
        match cond_ast with
            | C_true ->
                ()
                
            | C_false ->
                ()
                
            | (C_not cond_cond) ->
                make_cond cond_cond
                
            | C_compare (cond_expr_left, cond_compare_op, cond_expr_right) ->
                make_expr cond_expr_left;
                make_expr cond_expr_right;
                if( (cond_expr_left.expr_type = Some TYPE_int || cond_expr_left.expr_type = Some TYPE_byte) && cond_expr_left.expr_type = cond_expr_right.expr_type )
                then( () )
                else( fatal "Compare type mismatch" )
                
            | C_logic (cond_cond_left, cond_logic_op, cond_cond_right) ->
                make_cond cond_cond_left;
                make_cond cond_cond_right
    
    (*check if parameter call is ok*)
    and make_call func_call_ast =
        let e = lookupEntry (id_make func_call_ast.call_id) LOOKUP_ALL_SCOPES true in
        begin match e.entry_info with
        | ENTRY_function func_entry ->
            List.iter make_expr func_call_ast.call_expr;
            
            let get_param_typ param_entry = match param_entry.entry_info with 
                                        | ENTRY_parameter par -> (par.parameter_type,par.parameter_mode)
                                        | _ -> fatal "Parameter declaration gone wrong -- Why?"; (TYPE_none,PASS_BY_VALUE)
            in
            let get_actual_param_typ expr_ = match expr_.expr_type with
                                            | Some ex_type_some -> (ex_type_some,expr_.expr_raw)
                                            | None -> fatal "Sth gone terribly wrong"; (TYPE_none,E_int 0)
            in
            
            let declared_param_typ_lst = List.map get_param_typ func_entry.function_paramlist in
            let actual_param_typ_lst = List.map get_actual_param_typ func_call_ast.call_expr in
            
            let equal_declared_actual (dec_type,dec_mode) (act_type,act_raw) = 
                match (dec_mode, act_raw) with
                    | (PASS_BY_VALUE, _) -> equalType dec_type act_type
                    | (PASS_BY_REFERENCE, E_val _) -> equalType dec_type act_type
                    | _ -> false
            in
            
            let myPrint_dec (s,info) =
                pretty_typ std_formatter s;
                match info with
                    | PASS_BY_VALUE -> fprintf std_formatter "[PASS_BY_VALUE] "
                    | PASS_BY_REFERENCE -> fprintf std_formatter "[PASS_BY_REFERENCE] "
            in
            
            let myPrint_act (s,info) =
                pretty_typ std_formatter s;
                match info with
                    | E_val _ -> fprintf std_formatter "[L_value] "
                    | _ -> fprintf std_formatter "[NOT_L_value] "
            in

            if( ( List.length declared_param_typ_lst = List.length actual_param_typ_lst ) && 
                ( List.for_all2 equal_declared_actual declared_param_typ_lst actual_param_typ_lst )
              )
            
            then( func_call_ast.return_type <- Some func_entry.function_result )
            
            else(
                Printf.printf "%s" "Expected parameters: ";
                List.iter myPrint_dec declared_param_typ_lst;
                print_newline ();
                Printf.printf "%s" "Given parameters:    "; 
                List.iter myPrint_act actual_param_typ_lst;
                print_newline ();
                fatal "Incorrect argument format" 
            )
            
        | _ ->
            fatal "Call of a non-function"
        end
    and make_expr expr_ast = 
        match expr_ast.expr_raw with
            | E_int ex_int -> 
                expr_ast.expr_type <- Some TYPE_int
                
            | E_char ex_char -> 
                expr_ast.expr_type <- Some TYPE_byte
                
            | E_val ex_l_value ->
                make_l_value ex_l_value;
                expr_ast.expr_type <- ex_l_value.l_value_type
                
            | E_call ex_func_call ->
                make_call ex_func_call;
                expr_ast.expr_type <- ex_func_call.return_type
                
            | E_sign (ex_sign, ex_expr) ->
                make_expr ex_expr;
                if(ex_expr.expr_type = Some TYPE_int)
                then(expr_ast.expr_type <- ex_expr.expr_type)
                else(fatal "Sign must be followed by an integer")
                
            | E_op (ex_expr_left, ex_op, ex_expr_right) ->
                make_expr ex_expr_left;
                make_expr ex_expr_right;
                
                if((ex_expr_left.expr_type = Some TYPE_int || ex_expr_left.expr_type = Some TYPE_byte) && ex_expr_left.expr_type = ex_expr_right.expr_type)
                then(expr_ast.expr_type <- ex_expr_left.expr_type)
                else(fatal "Type mismatch")
            
    and make_l_value l_value_ast =
        match l_value_ast.l_value_raw with
            | L_exp (lval_id, lval_expr_opt) ->
                let e = lookupEntry (id_make lval_id) LOOKUP_ALL_SCOPES true in
                let e_typ = match e.entry_info with
                    | ENTRY_variable var_info ->
                        Some var_info.variable_type
                    | ENTRY_parameter par_info ->
                        Some par_info.parameter_type
                    | _ -> 
                        fatal "Identifier not valid";
                        None
                in
                begin match lval_expr_opt with
                    | Some lval_expr_some -> 
                        (*Check if not array*)
                        begin match e_typ with
                            | Some (TYPE_array (elem_typ,_)) -> 
                                make_expr lval_expr_some;
                                (*Check if index is int*)
                                if( lval_expr_some.expr_type <> Some TYPE_int )
                                then( fatal "Array index must be an integer" );
                                l_value_ast.l_value_type <- Some elem_typ
                            | _ -> 
                                fatal "Variable not an array"
                        end
                    | None ->
                        l_value_ast.l_value_type <- e_typ
                end
            | L_str lval_str -> (*String literal = TYPE_array (TYPE_byte,size < 0)*)
                l_value_ast.l_value_type <- Some TYPE_array (TYPE_byte,-1)
    in
    
    let rec make_stmt stmt_ast = 
        match stmt_ast with
            | Null_stmt -> 
                ()
                
            | S_assign (st_l_value, st_expr) ->
                make_expr st_expr;
                make_l_value st_l_value;
                begin match (st_l_value.l_value_type, st_expr.expr_type) with
                    | (Some TYPE_array (TYPE_byte,siz),_)   -> if( siz < 0 ) then ( fatal "Cannot assign value to a string literal" )
                    | (Some TYPE_proc,_)                    -> fatal "L value is a procedure -- How did this happen?" (*Obsolete*)
                    | (_,Some TYPE_proc)                    -> fatal "Cannot assign a procedure" (*Obsolete*)
                    | (typ1,typ2)                           -> if(typ1 <> typ2)then(fatal "Assignment type mismatch")
                end
                
            | S_comp st_stmt_lst ->
                List.iter make_stmt st_stmt_lst
                
            | S_call st_func_call -> 
                make_call st_func_call
                
            | S_if (st_cond,st_stmt,st_stmt_opt) ->
                make_cond st_cond;
                make_stmt st_stmt;
                begin match st_stmt_opt with
                    | Some st_stmt_some -> 
                        make_stmt st_stmt_some
                    | None -> 
                        ()
                end
                    
            | S_while (st_cond,st_stmt) -> 
                make_cond st_cond;
                make_stmt st_stmt
                
            | S_return st_expr_opt -> 
                let funTypeTop = Stack.pop funTypeStack in
                begin match st_expr_opt with
                    | Some st_expr_some ->
                        Stack.push funTypeTop funTypeStack;
                        if( funTypeTop = TYPE_proc )
                        then( fatal "Procedure cannot return a value" )
                        else( make_expr st_expr_some; if( st_expr_some.expr_type <> Some funTypeTop ) then ( fatal "Return type mismatch" ) )
                    | None ->
                        Stack.push funTypeTop funTypeStack;
                        if( funTypeTop <> TYPE_proc )
                        then( fatal "Return statement cannot be empty here" )
                end        
    in
    
    let make_par f par_ast =
        match (par_ast.par_type,par_ast.par_pass_way) with
            | (TYPE_array _, PASS_BY_VALUE) -> fatal "An array cannot be passed by value"
            | _ -> ignore (newParameter (id_make par_ast.par_id) par_ast.par_type par_ast.par_pass_way f true); ()
    in
    
    let rec make_local local_ast = 
        match local_ast with
            | Local_func loc_func ->
                make_func loc_func;
                ()
            | Local_var loc_var ->
                (*In case an array is declared, its size needs to be a positive integer*)
                begin match loc_var.var_type with
                    | TYPE_array (_,siz)    -> if(siz <= 0)then(fatal "Array size must be a positive integer")
                    | _                     -> ()
                end;
                ignore (newVariable (id_make loc_var.var_id) loc_var.var_type true);
                ()
                
    and make_func f_ast =
        
        let f_SYM = newFunction (id_make f_ast.func_id) true in
        Stack.push f_ast.func_type funTypeStack;
        openScope ();
        
        List.iter (make_par f_SYM) f_ast.func_pars;
        endFunctionHeader f_SYM f_ast.func_type;
        
        List.iter make_local f_ast.func_local;
        List.iter make_stmt f_ast.func_stmt;
        
        (* printSymbolTable (); *)
        closeScope ();
        ignore (Stack.pop funTypeStack);
        ()
    in
    
    initSymbolTable 256;
    openScope ();
    
(********* add existing functions **********************************************************************************************************************)
    
(***************** writeInteger (n : int) : proc *******************************************************************************************************)
    let f_wInt = newFunction (id_make "writeInteger") true in
    openScope ();
    
    make_par f_wInt {par_id = "n"; par_pass_way = PASS_BY_VALUE; par_type = TYPE_int};
    endFunctionHeader f_wInt TYPE_proc;
    
    closeScope ();
    
(***************** writeByte (b : byte) : proc *********************************************************************************************************)
    let f_wByte = newFunction (id_make "writeByte") true in
    openScope ();
    
    make_par f_wByte {par_id = "b"; par_pass_way = PASS_BY_VALUE; par_type = TYPE_byte};
    endFunctionHeader f_wByte TYPE_proc;
    
    closeScope ();
    
(***************** writeChar (b : byte) : proc *********************************************************************************************************)
    let f_wChar = newFunction (id_make "writeChar") true in
    openScope ();
    
    make_par f_wChar {par_id = "b"; par_pass_way = PASS_BY_VALUE; par_type = TYPE_byte};
    endFunctionHeader f_wChar TYPE_proc;
    
    closeScope ();
    
(***************** writeString (s : reference byte []) : proc ******************************************************************************************)
    let f_wStr = newFunction (id_make "writeString") true in
    openScope ();
    
    make_par f_wStr {par_id = "s"; par_pass_way = PASS_BY_REFERENCE; par_type = TYPE_array (TYPE_byte,-1)};
    endFunctionHeader f_wStr TYPE_proc;
    
    closeScope ();
    
(***************** readInteger () : int ****************************************************************************************************************)
    let f_rInt = newFunction (id_make "readInteger") true in
    openScope ();
    
    endFunctionHeader f_rInt TYPE_int;
    
    closeScope ();
    
(***************** readByte () : byte ******************************************************************************************************************)
    let f_rByte = newFunction (id_make "readByte") true in
    openScope ();
    
    endFunctionHeader f_rByte TYPE_byte;
    
    closeScope ();
    
(***************** readChar () : byte ******************************************************************************************************************)
    let f_rChar = newFunction (id_make "readChar") true in
    openScope ();
    
    endFunctionHeader f_rChar TYPE_byte;
    
    closeScope ();
    
(***************** readString (n : int, s : reference byte []) : proc **********************************************************************************)
    let f_rStr = newFunction (id_make "readString") true in
    openScope ();
    
    List.iter (make_par f_rStr) [{par_id = "n"; par_pass_way = PASS_BY_VALUE; par_type = TYPE_int};
                                 {par_id = "s"; par_pass_way = PASS_BY_REFERENCE; par_type = TYPE_array (TYPE_byte,-1)}];
    endFunctionHeader f_rStr TYPE_proc;
    
    closeScope ();
    
(***************** extend (b : byte) : int *************************************************************************************************************)
    let f_ext = newFunction(id_make "extend") true in
    openScope ();
    
    make_par f_ext {par_id = "b"; par_pass_way = PASS_BY_VALUE; par_type = TYPE_byte};
    endFunctionHeader f_ext TYPE_int;
    
    closeScope ();
    
(***************** shrink (i : int) : byte *************************************************************************************************************)
    let f_shrink = newFunction(id_make "shrink") true in
    openScope ();
    
    make_par f_shrink {par_id = "i"; par_pass_way = PASS_BY_VALUE; par_type = TYPE_int};
    endFunctionHeader f_shrink TYPE_byte;
    
    closeScope ();
    
(***************** strlen (s : reference byte []) : int ************************************************************************************************)
    let f_len = newFunction(id_make "strlen") true in
    openScope ();
    
    make_par f_len {par_id = "s"; par_pass_way = PASS_BY_REFERENCE; par_type = TYPE_array (TYPE_byte,-1)};
    endFunctionHeader f_len TYPE_int;
    
    closeScope ();
    
(***************** strcmp (s1 : reference byte [], s2 : reference byte []) : int ***********************************************************************)
    let f_cmp = newFunction (id_make "strcmp") true in
    openScope ();
    
    List.iter (make_par f_cmp) [{par_id = "s1"; par_pass_way = PASS_BY_REFERENCE; par_type = TYPE_array (TYPE_byte,-1)};
                                {par_id = "s2"; par_pass_way = PASS_BY_REFERENCE; par_type = TYPE_array (TYPE_byte,-1)}];
    endFunctionHeader f_cmp TYPE_int;
    
    closeScope ();
    
(***************** strcpy (trg : reference byte [], src : reference byte []) : proc ********************************************************************)
    let f_cpy = newFunction (id_make "strcpy") true in
    openScope ();
    
    List.iter (make_par f_cpy) [{par_id = "trg"; par_pass_way = PASS_BY_REFERENCE; par_type = TYPE_array (TYPE_byte,-1)};
                                {par_id = "src"; par_pass_way = PASS_BY_REFERENCE; par_type = TYPE_array (TYPE_byte,-1)}];
    endFunctionHeader f_cpy TYPE_proc;
    
    closeScope ();
    
(***************** strcat (trg : reference byte [], src : reference byte []) : proc ********************************************************************)
    let f_cat = newFunction (id_make "strcat") true in
    openScope ();
    
    List.iter (make_par f_cat) [{par_id = "trg"; par_pass_way = PASS_BY_REFERENCE; par_type = TYPE_array (TYPE_byte,-1)};
                                {par_id = "src"; par_pass_way = PASS_BY_REFERENCE; par_type = TYPE_array (TYPE_byte,-1)}];
    endFunctionHeader f_cat TYPE_proc;

    closeScope ();
    
(********* end of existing functions *******************************************************************************************************************)

    make_func tree;
    
    closeScope ();

    (****** end of seman ******)