open Lexer
open Parser
open Semantic

let main =
  let lexbuf = Lexing.from_channel stdin in
  try
    let tree = Parser.program Lexer.lexer lexbuf in
    seman tree;
    
    (*
    let 
        rec loop () =
          let token = lexer lexbuf in
          Printf.printf "token=%s, lexeme=\"%s\"\n"
            (string_of_token token) (Lexing.lexeme lexbuf);
          if token <> T_eof then loop ()
    in loop ();
	exit 0
    *)
  with Parsing.Parse_error ->
    Printf.eprintf "syntax error\n";
    exit 1