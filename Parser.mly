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
%token T_const
%token T_var
%token T_rparen
%token T_lparen
%token T_plus
%token T_minus
%token T_times
%token T_char 
%token T_string
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
%type <unit> program
%type <unit> func_def
%type <unit> fpar_list
%type <unit> fpar_def
%type <unit> type
%type <unit> r_type
%type <unit> local_def
%type <unit> var_def
%type <unit> stmt
%type <unit> compound_stmt
%type <unit> func_call
%type <unit> expr_list
%type <unit> expr
%type <unit> l_value
%type <unit> cond
%type <unit> fpar_def_rep
%type <unit> fpar_def_opt
%type <unit> type_opt
%type <unit> local_def_rep
%type <unit> var_def_opt
%type <unit> return_opt
%type <unit> stmt_rep
%type <unit> expr_list_opt
%type <unit> expr_list_rep
%type <unit> expr_opt
%type <unit> data_type

%%

program   : func_def T_eof { () }

func_def : T_var T_lparen fpar_list_opt T_rparen T_colon r_type local_def_rep compound_stmt{ () }

fpar_list_opt : fpar_list {()}
	| /* nothing */ { () }       

fpar_list : fpar_def fpar_def_rep { () }

fpar_def_rep : T_comma fpar_def fpar_def_rep { () }
	| /* nothing */ { () }
          
fpar_def : T_var T_colon fpar_def_opt type { () }

fpar_def_opt : T_reference {()}
	| /* nothing */ { () }

data_type : T_int {()}
	| T_byte { () }

type : data_type type_opt { () }

type_opt : T_lbra T_rbra {()}
| /* nothing */ { () }

r_type : data_type {()}
	| T_proc { () }

local_def_rep: local_def local_def_rep {()}
	| /* nothing */ { () }

local_def : func_def {()}
	| var_def {()}

var_def : T_var T_colon data_type var_def_opt T_semi {()}

var_def_opt : T_lbra T_const T_rbra {()}
	| /* nothing */ { () }

stmt : T_semi {()}
	| l_value T_assign expr T_semi {()}
	| compound_stmt {()}
	| func_call T_semi {()}
	| T_if T_lparen cond T_rparen stmt {()}
	| T_if T_lparen cond T_rparen stmt T_else stmt {()}
	| T_while T_lparen cond T_rparen stmt  {()}
	| T_return return_opt T_semi {()}
	
return_opt : expr {()}
	| /* nothing */ { () }

compound_stmt : T_lcurl stmt_rep T_rcurl {()}

stmt_rep : stmt stmt_rep {()}
	| /* nothing */ { () }

func_call : T_var T_lparen expr_list_opt T_rparen {()}

expr_list_opt : expr_list {()}
	| /* nothing */ { () }

expr_list : expr expr_list_rep {()}

expr_list_rep : T_comma expr expr_list_rep {()}
	| /* nothing */ { () }

expr : T_const {()}
	| T_char {()}
	| l_value {()}
	| T_lparen expr T_rparen {()}
	| func_call {()} 
	/*| sign expr {()}*/
	| expr T_plus expr {()}
	| expr T_minus expr {()}
	| expr T_times expr {()}
	| expr T_div expr {()}
	| expr T_mod expr {()}
	| T_plus expr %prec uplus {()}
	| T_minus expr %prec uminus {()}

l_value : T_var expr_opt {()}
	| T_string {()}
	
expr_opt : T_lbra expr T_rbra {()}
	| /* nothing */ {()}
	
cond : T_true {()}
	| T_false {()}
	| T_lparen cond T_rparen {()}
	| T_not cond {()}
	| expr T_eq expr {()}
	| expr T_neq expr {()}
	| expr T_lt expr {()}
	| expr T_gt expr {()}
	| expr T_le expr {()}
	| expr T_ge expr {()}
	| cond T_and cond {()}
	| cond T_or cond {()}