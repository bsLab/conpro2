type token =
    STRING of (Cp_syntax.file_pos*string)
  | IDENT of (Cp_syntax.file_pos*string)
  | VALUE of (char*Cp_syntax.file_pos*string)
  | PARAM of (Cp_syntax.file_pos*string)
  | S_VERSION
  | S_PARAMETER
  | S_FUN
  | S_TARGETS
  | S_TARGET
  | FOREACH
  | FOR
  | DO
  | WHEN
  | IF
  | THEN
  | ELSE
  | BEGIN
  | END
  | MATCH
  | WITH
  | IN
  | EOI
  | VAR
  | SEP1
  | SEP2
  | DOT
  | LPAREN
  | RPAREN
  | LBRAK
  | RBRAK
  | LCURL
  | RCURL
  | FOLLOW
  | EQ
  | NEQ
  | LT
  | GT
  | ASSIGN
  | GEQ
  | PLUS
  | MINUS
  | TIMES
  | DIVI
  | AND
  | OR
  | XOR
  | COMMENT
  | TO
  | DOWNTO

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Cp_syntax.tdi_syntax
