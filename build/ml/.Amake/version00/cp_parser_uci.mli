type token =
    STRING of (Cp_syntax.file_pos*string)
  | CHAR of (Cp_syntax.file_pos*char)
  | PARAM of (Cp_syntax.file_pos*string)
  | TYPE_SPEC of (Cp_syntax.file_pos*string)
  | IDENT of (Cp_syntax.file_pos*string)
  | VALUE of (char*Cp_syntax.file_pos*string)
  | S_PARAMETER
  | S_MODULES
  | S_EXPORT
  | S_IMPORT
  | S_TYPES
  | S_DATA
  | S_CODE
  | S_TEMP
  | VAR
  | REG
  | SIG
  | OBJ
  | QUE
  | CHA
  | BLK
  | FUNC
  | CON
  | VAL
  | CELLS
  | ARR
  | MODULE
  | METHOD
  | TYPE
  | T_LOGIC
  | T_INT
  | T_CHAR
  | T_BOOL
  | T_NAT
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
  | AND
  | OR
  | XOR
  | NOT
  | LBRAK
  | RBRAK
  | LPAREN
  | RPAREN
  | LCURL
  | RCURL
  | SEP1
  | SEP2
  | COMMENT
  | TO
  | DOWNTO
  | IN
  | WITH
  | OF
  | ASS
  | DOT
  | PARENT
  | BEGIN
  | END
  | IMMED
  | ALU
  | TEMP
  | LABEL
  | MOVE
  | EXPR
  | FUN
  | BIND
  | JUMP
  | FALSEJUMP
  | NOP

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Cp_syntax.uci_syntax
