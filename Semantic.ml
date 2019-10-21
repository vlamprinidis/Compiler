open Format

open Identifier
open Types
open Symbol
open Ast

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
                make_expr cond_expr_right
                
            | C_logic (cond_cond_left, cond_logic_op, cond_cond_right) ->
                make_cond cond_cond_left;
                make_cond cond_cond_right
            
    and make_call func_call_ast =
        lookupEntry (id_make func_call_ast.call_id) LOOKUP_ALL_SCOPES true;
        List.iter make_expr func_call_ast.call_expr
        
    and make_expr expr_ast = 
        match expr_ast with
            | E_int ex_int -> 
                ()
                
            | E_char ex_char -> 
                ()
                
            | E_val ex_l_value ->
                make_l_value ex_l_value
                
            | E_call ex_func_call ->
                make_call ex_func_call
                
            | E_sign (ex_sign, ex_expr) ->
                make_expr ex_expr
                
            | E_op (ex_expr_left, ex_op, ex_expr_right) ->
                make_expr ex_expr_left;
                make_expr ex_expr_right
            
    and make_l_value l_value_ast =
        match l_value_ast with
            | L_exp (lval_id, lval_expr_opt) ->
                lookupEntry (id_make lval_id) LOOKUP_ALL_SCOPES true;
                begin match lval_expr_opt with
                    | Some lval_expr_some -> 
                        make_expr lval_expr_some
                    | None -> 
                        ()
                end
            | L_str lval_str -> 
                ()
    in
    
    let rec make_stmt stmt_ast = 
        match stmt_ast with
            | Null_stmt -> 
                ()
                
            | S_assign (st_l_value, st_expr) ->
                make_expr st_expr;
                make_l_value st_l_value
                
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
                begin match st_expr_opt with
                    | Some st_expr_some -> 
                        make_expr st_expr_some
                    | None -> 
                        ()
                end        
    in
    
    let make_par f par_ast =
        newParameter (id_make par_ast.par_id) par_ast.par_type par_ast.par_pass_way f true;
        ()
    in
    
    let rec make_local local_ast = 
        match local_ast with
            | Local_func loc_func ->
                make_func loc_func;
                ()
            | Local_var loc_var ->
                newVariable (id_make loc_var.var_id) loc_var.var_type true;
                ()
    and make_func f_ast =
        
        let f_SYM = newFunction (id_make f_ast.func_id) true in
        openScope ();
        
        List.iter (make_par f_SYM) f_ast.func_pars;
        endFunctionHeader f_SYM f_ast.func_type;
        
        List.iter make_local f_ast.func_local;
        List.iter make_stmt f_ast.func_stmt;
        
        printSymbolTable ();
        closeScope ();
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
    
    make_par f_wStr {par_id = "s"; par_pass_way = PASS_BY_REFERENCE; par_type = TYPE_byte};
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
                                 {par_id = "s"; par_pass_way = PASS_BY_REFERENCE; par_type = TYPE_byte}];
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
    
    make_par f_len {par_id = "s"; par_pass_way = PASS_BY_REFERENCE; par_type = TYPE_byte};
    endFunctionHeader f_len TYPE_int;
    
    closeScope ();
    
(***************** strcmp (s1 : reference byte [], s2 : reference byte []) : int ***********************************************************************)
    let f_cmp = newFunction (id_make "strcmp") true in
    openScope ();
    
    List.iter (make_par f_cmp) [{par_id = "s1"; par_pass_way = PASS_BY_REFERENCE; par_type = TYPE_byte};
                                {par_id = "s2"; par_pass_way = PASS_BY_REFERENCE; par_type = TYPE_byte}];
    endFunctionHeader f_cmp TYPE_int;
    
    closeScope ();
    
(***************** strcpy (trg : reference byte [], src : reference byte []) : proc ********************************************************************)
    let f_cpy = newFunction (id_make "strcpy") true in
    openScope ();
    
    List.iter (make_par f_cpy) [{par_id = "trg"; par_pass_way = PASS_BY_REFERENCE; par_type = TYPE_byte};
                                {par_id = "src"; par_pass_way = PASS_BY_REFERENCE; par_type = TYPE_byte}];
    endFunctionHeader f_cpy TYPE_proc;
    
    closeScope ();
    
(***************** strcat (trg : reference byte [], src : reference byte []) : proc ********************************************************************)
    let f_cat = newFunction (id_make "strcat") true in
    openScope ();
    
    List.iter (make_par f_cat) [{par_id = "trg"; par_pass_way = PASS_BY_REFERENCE; par_type = TYPE_byte};
                                {par_id = "src"; par_pass_way = PASS_BY_REFERENCE; par_type = TYPE_byte}];
    endFunctionHeader f_cat TYPE_proc;

    closeScope ();
    
(********* end of existing functions *******************************************************************************************************************)

    make_func tree;
    
    closeScope ();

    (****** end of seman ******)