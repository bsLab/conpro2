type token =
    STRING of (Cp_syntax.file_pos*string)
  | VALUE of (Cp_syntax.file_pos*string)
  | IDENT of (Cp_syntax.file_pos*string)
  | COMMENT
  | VERSION
  | EOI
  | SEP
  | LBRAK
  | RBRAK
  | LCURL
  | RCURL
  | ADD
  | SYN_TOOL
  | SYN_TOP
  | SYN_VER
  | SYN_VHDL_MAP
  | SYN_SYNTH_SET
  | SYN_TECH_SET
  | SYN_VHDL_LIB
  | EQ

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Cp_syntax.synth_tools_syntax
