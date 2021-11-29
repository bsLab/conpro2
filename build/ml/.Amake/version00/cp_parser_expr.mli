type token =
    STRING of (Cp_syntax.file_pos*string)
  | IDENT of (Cp_syntax.file_pos*string)
  | PARAM of (Cp_syntax.file_pos*string)
  | DATAWIDTH
  | CYCLETIME
  | U_NS
  | U_US
  | EOI
  | SEP
  | LPAREN
  | RPAREN
  | EQ
  | NEQ
  | LT
  | GT
  | LEQ
  | GEQ
  | PLUS
  | MINUS
  | TIMES
  | DIVI

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Cp_syntax.expr_syntax
