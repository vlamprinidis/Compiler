%{
open Ast
open Types
open Symbol
%}

%token T_byte
%token T_else
%token T_false
%token T_if
%token T_int 
%token T_proc 
%token T_reference
%token T_return
%token T_while
%token T_true
%token<int> T_const
%token<string> T_id
%token T_rparen
%token T_lparen
%token T_plus
%token T_minus
%token T_times
%token<char> T_char 
%token<string> T_string
%token T_assign
%token T_div
%token T_mod
%token T_not
%token T_and
%token T_or
%token T_eq
%token T_neq
%token T_lt
%token T_gt
%token T_le
%token T_ge
%token T_lbra
%token T_rbra
%token T_lcurl
%token T_rcurl
%token T_colon
%token T_semi
%token T_comma
%token T_eof
 
%left T_or
%left T_and
%nonassoc T_eq T_neq T_le T_ge
%left T_plus T_minus
%left T_times T_div T_mod
%nonassoc T_not uplus uminus


%start program
%type <Ast.func> program

%%

program   : func_def T_eof { $1 }

func_def : T_id T_lparen fpar_list_opt T_rparen T_colon r_type local_def_rep compound_stmt { 
    {   
        func_id = $1;
        func_pars = $3;
        func_type = $6;
        func_local = $7;
        func_stmt = $8;
    }
}

fpar_list_opt : fpar_list { $1 }
	| /* nothing */ { [] }       

fpar_list : fpar_def fpar_def_rep { $1 :: $2 }

fpar_def_rep : T_comma fpar_def fpar_def_rep { $2 :: $3 }
	| /* nothing */ { [] }
          
fpar_def : T_id T_colon fpar_def_opt alan_type { { par_id = $1 ; par_pass_way = $3 ; par_type = $4 ;} }

fpar_def_opt : T_reference { PASS_BY_REFERENCE }
	| /* nothing */ { PASS_BY_VALUE }

data_type : T_int { TYPE_int }
	| T_byte { TYPE_byte }

alan_type : data_type { $1 }

alan_type : data_type T_lbra T_rbra { TYPE_array ($1,-1) (* -1 => array size here is not important *) }

r_type : data_type { $1 }
	| T_proc { TYPE_proc }

local_def_rep: local_def local_def_rep { $1 :: $2 }
	| /* nothing */ { [] }

local_def : func_def { Local_func $1 }
	| var_def { Local_var $1 }

var_def : T_id T_colon data_type var_def_opt T_semi { { var_id = $1 ; var_type = $3 ; var_const = $4 ;} }

var_def_opt : T_lbra T_const T_rbra { Some $2 }
	| /* nothing */ { None }

stmt : T_semi { Null_stmt }
	| l_value T_assign expr T_semi { S_assign ($1,$3) }
	| compound_stmt { S_comp $1 }
	| func_call T_semi { S_call $1 }
	| T_if T_lparen cond T_rparen stmt { S_if ($3,$5,None) }
	| T_if T_lparen cond T_rparen stmt T_else stmt { S_if ($3,$5,Some $7) }
	| T_while T_lparen cond T_rparen stmt  { S_while ($3,$5) }
	| T_return return_opt T_semi { S_return $2 }
	
return_opt : expr { Some $1 }
	| /* nothing */ { None }

compound_stmt : T_lcurl stmt_rep T_rcurl { $2 }

stmt_rep : stmt stmt_rep { $1 :: $2 }
	| /* nothing */ { [] }

func_call : T_id T_lparen expr_list_opt T_rparen { { call_id = $1 ; call_expr = $3 ; } }

expr_list_opt : expr_list { $1 }
	| /* nothing */ { [] }

expr_list : expr expr_list_rep { $1 :: $2 }

expr_list_rep : T_comma expr expr_list_rep { $2 :: $3 }
	| /* nothing */ { [] }

expr : T_const { E_int $1 }
	| T_char { E_char $1 }
	| l_value { E_val $1 }
	| T_lparen expr T_rparen { $2 }
	| func_call { E_call $1 } 
	/*| sign expr {()}*/
	| expr T_plus expr { E_op ($1, Plus, $3) }
	| expr T_minus expr { E_op ($1, Minus, $3) }
	| expr T_times expr { E_op ($1, Mult, $3) }
	| expr T_div expr { E_op ($1, Div, $3) }
	| expr T_mod expr { E_op ($1, Mod, $3) }
	| T_plus expr %prec uplus { E_sign (SPlus, $2) }
	| T_minus expr %prec uminus { E_sign (SMinus, $2) }

l_value : T_id expr_opt { L_exp ($1, $2) }
	| T_string { L_str $1 }
	
expr_opt : T_lbra expr T_rbra { Some $2 }
	| /* nothing */ { None }
	
cond : T_true { C_true }
	| T_false { C_false }
	| T_lparen cond T_rparen { $2 }
	| T_not cond { C_not $2 }
	| expr T_eq expr { C_compare($1, Eq, $3) }
	| expr T_neq expr { C_compare($1, Neq, $3) }
	| expr T_lt expr { C_compare($1, Less, $3) }
	| expr T_gt expr { C_compare($1, Great, $3) }
	| expr T_le expr { C_compare($1, LessEq, $3) }
	| expr T_ge expr { C_compare($1, GreatEq, $3) }
	| cond T_and cond { C_logic($1, And, $3) }
	| cond T_or cond { C_logic($1, Or, $3) }