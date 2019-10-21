open Types
open Symbol

type id = string

(* type reference = Is_ref | Not_ref *)

(*type pass_way = PASS_BY_REFERENCE | PASS_BY_VALUE*)

type sign = SPlus | SMinus

type op = 
| Plus
| Minus
| Mult
| Div
| Mod

type compare_op =
| Eq
| Neq
| Less
| Great
| LessEq
| GreatEq

type logic_op = 
| And
| Or

(*
type data_type =
| Int
| Byte

type _type = 
| Basic of data_type
| Array of data_type
*)

type func = {
    func_id : id;
    func_pars : par list;
    func_type : typ;
    func_local : local list;
    func_stmt : stmt list;
}

and par = {
    par_id : id;
    par_pass_way : pass_mode;
    par_type : typ;
}

and local = 
| Local_func of func
| Local_var of var

and var = {
    var_id : id;
    var_type : typ;
    var_const : int option;
}

and stmt = 
| Null_stmt
| S_assign of l_value * expr
| S_comp of stmt list
| S_call of func_call
| S_if of cond * stmt * (stmt option)
| S_while of cond * stmt
| S_return of expr option

and func_call = {
    call_id : id;
    call_expr : expr list;
}

and expr = 
| E_int of int
| E_char of char
| E_val of l_value
| E_call of func_call
| E_sign of sign * expr
| E_op of expr * op * expr

and l_value = 
| L_exp of id * (expr option)
| L_str of string

and cond = 
| C_true
| C_false
| C_not of cond
| C_compare of expr * compare_op * expr
| C_logic of cond * logic_op * cond

