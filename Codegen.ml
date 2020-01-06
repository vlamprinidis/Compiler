open Llvm
open Ast
open Error

exception Error of string

let context = global_context ()
let the_module = create_module context "alan"
let builder = builder context

let int_type = i16_type context (* lltype *)
let byte_type = i8_type context


let rec codegen_expr expr_ast = 
    begin match expr_ast.expr_raw with
        | E_int n -> const_int int_type n
        | E_char c -> const_int byte_type c
        | E_val v -> codegen_lval v
        | E_call cl -> codegen_call cl
        | E_sign (SPlus,er) -> codegen_expr er
        | E_sign (SMinus,er) ->  const_neg ( codegen_expr er )
    end
(*Work here*)
and codegen_lval l_value_ast = 
    begin match l_value_ast.l_value_raw with
        | L_exp (lval_id,None) -> 
        | 
    end