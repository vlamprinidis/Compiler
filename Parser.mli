type token =
  | T_byte
  | T_else
  | T_false
  | T_if
  | T_int
  | T_proc
  | T_reference
  | T_return
  | T_while
  | T_true
  | T_const
  | T_var
  | T_rparen
  | T_lparen
  | T_plus
  | T_minus
  | T_times
  | T_char
  | T_string
  | T_assign
  | T_div
  | T_mod
  | T_not
  | T_and
  | T_or
  | T_eq
  | T_neq
  | T_lt
  | T_gt
  | T_le
  | T_ge
  | T_lbra
  | T_rbra
  | T_lcurl
  | T_rcurl
  | T_colon
  | T_semi
  | T_comma
  | T_eof

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
