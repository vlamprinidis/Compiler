{
	open Parser
}

let digit  = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let white  = [' ' '\t' '\r' '\n']
let chars = [^ '"' '\n' '\t' '\'' '\\']
let escape ='\\' (['n' 't' 'r' '0' '\\' '\'' '"'] | ('x' ( digit | ['a'-'f'] ) ( digit | ['a'-'f'] ) ))
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

  | digit+   { T_const }
  | letter(letter|digit|'_')*   { T_var }
  | "'" ( chars | escape ) "'" {T_char}
  | '"' ( chars | escape )* '"' {T_string}

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
    
  | "(*" { print_endline "comments start"; comments 0 lexbuf }

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

{
  let string_of_token token =
    match token with
      | T_eof    -> "T_eof"
      | T_byte  -> "T_byte"
      | T_else    -> "T_else"
      | T_false    -> "T_false"
      | T_if     -> "T_if"
      | T_int  -> "T_int"
      | T_proc    -> "T_proc"
      | T_reference     -> "T_reference"
      | T_return   -> "T_return"
      | T_while     -> "T_while"
      | T_true -> "T_true"
	  | T_const  -> "T_const"
      | T_var    -> "T_var"
      | T_rparen -> "T_rparen"
      | T_plus   -> "T_plus"
      | T_minus  -> "T_minus"
      | T_times  -> "T_times"
	  | T_div -> "T_div"
	  | T_mod -> "T_mod"
	  | T_not -> "T_not"
	  | T_and -> "T_and"
	  | T_or -> "T_or"
	  | T_eq -> "T_eq"
	  | T_neq -> "T_neq"
	  | T_lt -> "T_lt"
	  | T_gt -> "T_gt"
	  | T_le -> "T_le"
	  | T_ge -> "T_ge"
	  | T_lparen -> "T_lparen"
	  | T_lbra -> "T_lbra"
	  | T_rbra -> "T_rbra"
	  | T_lcurl -> "T_lcurl"
	  | T_rcurl -> "T_rcurl"
	  | T_comma -> "T_comma"
	  | T_colon -> "T_colon"
	  | T_semi -> "T_semi"
	  | T_assign -> "T_assign"
	  | T_char -> "T_char"
	  | T_string -> "T_string"
      
      (*
  rule count lines words chars = parse
  | '\n' {count (lines + 1) words (chars + 1) lexbuf }
  | [^ ' ' '\t' '\n']+ as word{ count lines (words + 1) (chars + String.length word) lexbuf }
  | _  { count lines words (chars+1) lexbuf }
  | eof  { (lines, words, chars) }
  *)

}