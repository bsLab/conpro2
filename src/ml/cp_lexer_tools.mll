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
**    $INITIAL:     (C) 2006-2009 BSSLAB
**    $CREATED:     19.2.2009
**    $VERSION:     2.04
**
**    $INFO:
**
**  CONPRO synth_tools lexer
**
**    $ENDOFINFO
*)
    open Cp_parser_tools
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

    | ';' { EOI }
    | '=' { EQ }
    | '[' { LBRAK }
    | ']' { RBRAK }
    | '{' { LCURL }
    | '}' { RCURL }
    | ',' { SEP }
    | '+' { ADD }
    | "syn_tool" { SYN_TOOL }
    | "syn_top" { SYN_TOP } 
    | "syn_ver" { SYN_VER }
    | "syn_vhdl_map" { SYN_VHDL_MAP }
    | "syn_synth_set" { SYN_SYNTH_SET }
    | "syn_tech_set" { SYN_TECH_SET }
    | "syn_vhdl_lib" { SYN_VHDL_LIB }
    | "#version" { VERSION }
    | ['0' - '9']+
        { VALUE ({f_name= !file_name; f_cpos=lexbuf.Lexing.lex_curr_pos},
                Lexing.lexeme lexbuf) 
        }
    | ['a' - 'z' 'A' - 'Z' '_'   ] ['0' - '9' 'a' - 'z' 'A' - 'Z' '_']+ 
        { IDENT ({f_name= "EXPR"; f_cpos=lexbuf.Lexing.lex_curr_pos},
                Lexing.lexeme lexbuf) 
        }
    | eof  { raise EOF }
and string = parse
  |  '"' { () }
  | '\\' '"' { 
        store_string_char '"';
        string lexbuf }
  | eof { raise EOF }
  | _
      { store_string_char(Lexing.lexeme_char lexbuf 0);
        string lexbuf }
and comment = parse
    ['\n' '\r'] { COMMENT }
    | eof  { raise EOF }    
    | _ { comment lexbuf }
    
