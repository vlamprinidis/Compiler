{
	open Parser
    
    let escape1char scp = match scp with
    | 'n' -> '\n'
    | 't' -> '\t'
    | 'r' -> '\r'
    | '0' -> '\x00'
    | _ -> scp
    
    let escape2char scp = Char.chr (int_of_string ("0x" ^(String.sub scp 2 (String.(length scp)-2))))
}

let digit  = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let white  = [' ' '\t' '\r' '\n']
let chars = [^ '"' '\n' '\t' '\'' '\\']
let escape1 = '\\' ['n' 't' 'r' '0' '\\' '\'' '"']
let escape2 = '\\' ('x' ( digit | ['a'-'f'] ) ( digit | ['a'-'f'] ) )
let notln = [^ '\n']

rule lexer = parse
    "byte"  { T_byte }
  | "else"    { T_else }
  | "false"    { T_false }
  | "if"     { T_if }
  | "int"  { T_int }
  | "proc"    { T_proc }
  | "reference"     { T_reference }
  | "return"   { T_return }
  | "while" {T_while}
  | "true" {T_true}

  | digit+ as n   { T_const (int_of_string n)}
  | letter(letter|digit|'_')* as x   { T_id x}
  | "'" (chars as ch) "'" {T_char ch}
  | "'" (escape1 as scp1) "'" {T_char  (escape1char scp1.[1])}
  | "'" (escape2 as scp2) "'" {T_char  (escape2char scp2)}
  | '"' (( chars | escape1 | escape2 )* as str) '"' {T_string str}

  | '='      { T_assign }
  | '+'      { T_plus }
  | '-'      { T_minus }
  | '*'      { T_times }
  | '/'		 { T_div}
  | '%' {T_mod}
  | '!' {T_not}
  | '&' {T_and}
  | '|' {T_or}
  | "==" {T_eq}
  | "!=" {T_neq}
  | '<' {T_lt}
  | '>' {T_gt}
  | "<=" {T_le}
  | ">=" {T_ge}
  
  
  | '('      { T_lparen }
  | ')'      { T_rparen }
  | '[' {T_lbra}
  | ']' {T_rbra}
  | '{' {T_lcurl}
  | '}' {T_rcurl}
  | ',' {T_comma}
  | ':' {T_colon}
  | ';' {T_semi}

  | white+               { lexer lexbuf }
  | '-''-' notln* { lexer lexbuf }
    
  | "(*" { (*print_endline "comments start";*) comments 0 lexbuf }

  |  eof          { T_eof }
  |  _ as chr     { Printf.eprintf "invalid character: '%c' (ascii: %d)"
                      chr (Char.code chr);
                    lexer lexbuf }
                    
and comments level = parse
    | "*)" { (*Printf.printf "comments (%d) end\n" level;*)
             if level = 0 then lexer lexbuf
             else comments (level-1) lexbuf
           }
    | "(*" { (*Printf.printf "comments (%d) start\n" (level+1);*)
             comments (level+1) lexbuf
           }
    | _ { comments level lexbuf }
    | eof { (*print_endline "comments are not closed";*)
            exit 1
          }
