open Types
open Symbol
open Llvm
open Identifier

type my_id = string

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
    mutable full_name : my_id;
    func_id : my_id;
    func_pars : par list;
    func_ret_type : typ;
    func_local : local list;
    func_stmt : stmt list;
    mutable func_nesting_scope : int;
    mutable parent : func option;
    mutable frame_type : lltype option;
    mutable isMain : bool;
}

and par = {
    par_id : my_id;
    par_pass_way : pass_mode;
    par_type : typ;
    mutable par_offset: int;
    mutable symb_id : Identifier.id option;
}

and local = 
| Local_func of func
| Local_var of var

and var = {
    var_id : my_id;
    var_type : typ;
    mutable var_offset : int;
}

and stmt = 
| Null_stmt
| S_assign of l_value * expr
| S_comp of stmt list
| S_call of call
| S_if of cond * stmt * (stmt option)
| S_while of cond * stmt
| S_return of expr option

and call = {
    call_id : my_id;
    call_expr : expr list;
    mutable return_type : typ option;
    mutable callee_full_name : my_id option;
    mutable callee_scope : int;
    mutable caller_nesting_scope : int;
    mutable declared_pars : (pass_mode list) option;
}

and raw_expr = 
| E_int of int
| E_char of char
| E_lvalue of l_value
| E_call of call
| E_sign of sign * expr
| E_op of expr * op * expr

and expr = {
    expr_raw : raw_expr;
    mutable expr_type : typ option;
}

and raw_l_value = 
| L_id of my_id * (expr option)
| L_str of string

and l_value = {
    l_value_raw : raw_l_value;
    mutable l_value_type : typ option;
    mutable l_value_nesting_diff : int;
    mutable offset : int;
    mutable is_parameter : bool;
    mutable is_local : bool;
    mutable is_ptr : bool;
}

and cond = 
| C_true
| C_false
| C_not of cond
| C_compare of expr * compare_op * expr
| C_logic of cond * logic_op * cond

