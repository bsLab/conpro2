{
(*
**      ==================================
**      OOOO   OOOO OOOO  O      O   OOOO
**      O   O  O    O     O     O O  O   O
**      O   O  O    O     O     O O  O   O
**      OOOO   OOOO OOOO  O     OOO  OOOO
**      O   O     O    O  O    O   O O   O
**      O   O     O    O  O    O   O O   O
**      OOOO   OOOO OOOO  OOOO O   O OOOO
**      ================================== 
**      BSSLAB, Dr. Stefan Bosse, http://www.bsslab.de
**
**      COPYRIGHT: THIS SOFTWARE, EXECUTABLE AND SOURCE CODE IS OWNED BY BSSLAB.
**                 THIS SOURCE CODE MAY NOT BE COPIED, EXTRACTED,
**                 MODIFIED, OR OTHERWISE USED IN A CONTEXT
**                 OUTSIDE OF THE SOFTWARE SYSTEM.
**
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2008 BSSLAB
**    $CREATED:     7.11.2008
**    $VERSION:     2.09
**
**    $INFO:
**
**  CONPRO lexer for the MicroCode Interface
**
**    $ENDOFINFO
*)
    open Cp_parser_uci
    open Cp_syntax
    exception EOF

(* To buffer string literals *)

let initial_string_buffer = String.create 256
let string_buff = ref initial_string_buffer
let string_index = ref 0

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0

let store_string_char c =
  if !string_index >= String.length (!string_buff) then begin
    let new_buff = String.create (String.length (!string_buff) * 2) in
      String.blit (!string_buff) 0 new_buff 0 (String.length (!string_buff));
      string_buff := new_buff
  end;
  String.unsafe_set (!string_buff) (!string_index) c;
  incr string_index

let get_stored_string () =
  let s = String.sub (!string_buff) 0 (!string_index) in
  string_buff := initial_string_buffer;
  s
(* To store the position of the beginning of a string and comment *)
let string_start_pos = ref 0;;

}


rule token = parse
    [' ' '\t' '\n' '\r'] { token lexbuf }
    | '"' 
      { reset_string_buffer();
        let string_start = Lexing.lexeme_start lexbuf in
        string_start_pos := string_start;
        string lexbuf;
        lexbuf.Lexing.lex_start_pos <-
          string_start - lexbuf.Lexing.lex_abs_pos;
        STRING ({f_name= !file_name; f_cpos=lexbuf.Lexing.lex_curr_pos},
               get_stored_string();); 
      }
    | "--"  { comment lexbuf; token lexbuf }
    | ':' { SEP1 }
    | ',' { SEP2 }
    | '.' { DOT }
    | '(' { LPAREN }
    | ')' { RPAREN }
    | '[' { LBRAK }
    | ']' { RBRAK }
    | '{' { LCURL }
    | '}' { RCURL }
    | '=' { EQ }
    | '<' { LT }
    | '>' { GT }
    | "<=" { LEQ }
    | ">=" { GEQ }
    | "<>" { NEQ }
    | '+' { PLUS }
    | '-' { MINUS }
    | '*' { TIMES }
    | '/' { DIVI }
    | "and" { AND }
    | "or" { OR }
    | "xor" { XOR }
    | "not" { NOT }
    
    | "parameter" { S_PARAMETER }
    | "modules" { S_MODULES }
    | "export" { S_EXPORT }
    | "import" { S_IMPORT }
    | "data" { S_DATA }
    | "temp" { S_TEMP }
    | "types" { S_TYPES }
    | "code" { S_CODE }
    
    | "register" { REG }
    | "signal" { SIG }
    | "variable" { VAR }
    | "object" { OBJ }
    | "queue" { QUE }
    | "channel" { CHA }
    | "block" { BLK }
    | "function" { FUNC }
    | "constant" { CON }
    | "value" { VAL }
    | "cells" { CELLS }
    | "array" { ARR }
    | "module" { MODULE }
    | "method" { METHOD }
    | "type" { TYPE }
    
    | "L" { TYPE_SPEC ({f_name= !file_name; f_cpos=lexbuf.Lexing.lex_curr_pos},"L") }
    | "N" { TYPE_SPEC ({f_name= !file_name; f_cpos=lexbuf.Lexing.lex_curr_pos},"N") }
    | "I" { TYPE_SPEC ({f_name= !file_name; f_cpos=lexbuf.Lexing.lex_curr_pos},"I") }
    | "B" { TYPE_SPEC ({f_name= !file_name; f_cpos=lexbuf.Lexing.lex_curr_pos},"B") }
    | "C" { TYPE_SPEC ({f_name= !file_name; f_cpos=lexbuf.Lexing.lex_curr_pos},"C") }
    
    | "CT" { PARAM ({f_name= !file_name; f_cpos=lexbuf.Lexing.lex_curr_pos},"CT") }
    | "ET" { PARAM ({f_name= !file_name; f_cpos=lexbuf.Lexing.lex_curr_pos},"ET") }
    | "LT" { PARAM ({f_name= !file_name; f_cpos=lexbuf.Lexing.lex_curr_pos},"LT") }
    | "PT" { PARAM ({f_name= !file_name; f_cpos=lexbuf.Lexing.lex_curr_pos},"PT") }
    | "LHS" { PARAM ({f_name= !file_name; f_cpos=lexbuf.Lexing.lex_curr_pos},"LHS") }
    | "RHS" { PARAM ({f_name= !file_name; f_cpos=lexbuf.Lexing.lex_curr_pos},"RHS") }
    | "LRHS" { PARAM ({f_name= !file_name; f_cpos=lexbuf.Lexing.lex_curr_pos},"LRHS") }
    | "PARENT" { PARAM ({f_name= !file_name; f_cpos=lexbuf.Lexing.lex_curr_pos},"PARENT") }
    | "IMPORT" { PARAM ({f_name= !file_name; f_cpos=lexbuf.Lexing.lex_curr_pos},"IMPORT") }
    
    
    | "immed" { IMMED }
    | "temp" { TEMP }
    | "alu" { ALU }
    
    | "fun" { FUN }
    | "move" { MOVE }
    | "expr" { EXPR }
    | "label" { LABEL }
    | "bind" { BIND }
    | "jump" { JUMP }
    | "falsejump" { FALSEJUMP }
    | "nop" { NOP }
    
    
    | "begin" { BEGIN }
    | "end" { END }
    | "with" { WITH }
    | "in" { IN }
    | "to" { TO }
    | "downto" { DOWNTO }
    
    
    
    | '$' { VAR }
    | ['0' - '9']+
        { VALUE ('d',{f_name= !file_name; f_cpos=lexbuf.Lexing.lex_curr_pos},
                Lexing.lexeme lexbuf) 
        }
    | ['0' - '9'] 'x' ['0'-'9' 'a'-'f']+
        { VALUE ('x',{f_name= !file_name; f_cpos=lexbuf.Lexing.lex_curr_pos},
                Lexing.lexeme lexbuf) 
        }
    | ['0' - '9']+ '.' ['0'-'9']+
        { VALUE ('f',{f_name= !file_name; f_cpos=lexbuf.Lexing.lex_curr_pos},
                Lexing.lexeme lexbuf) 
        }
    | ['0' - '9' 'a' - 'z' 'A' - 'Z' '_' '%'  ]+ 
        { IDENT ({f_name= !file_name; f_cpos=lexbuf.Lexing.lex_curr_pos},
                Lexing.lexeme lexbuf) 
        }

    | eof  { raise EOF }
and string = parse
    '"'
      { () }
  | '\\' ("\010" | "\013" | "\013\010") [' ' '\009'] *
      { string lexbuf }
  | eof { raise EOF }
  | _
      { store_string_char(Lexing.lexeme_char lexbuf 0);
        string lexbuf }
and comment = parse
    ['\n' '\r'] { COMMENT }
    | eof  { raise EOF }    
    | _ { comment lexbuf }
