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
**    $INITIAL:     (C) 2006-2011 BSSLAB
**    $CREATED:     1.3.2006
**    $VERSION:     2.09
**
**    $INFO:
**
**  CONPRO lexer
**
**    $ENDOFINFO
*)
    open Cp_parser
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
let this_char = ref None 

}


rule token = parse
    [' ' '\t' '\n' '\r'] { token lexbuf }
    | "--"  { comment lexbuf; token lexbuf }
    | '\''
      {
        this_char := None;
        getchar lexbuf;
        CHAR ({f_name= !file_name; f_cpos=lexbuf.Lexing.lex_curr_pos},
               (match !this_char with Some c -> c| None -> progerr "getchar");); 
        
      }
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

    | [';'] { EOI }
    | ':' { SEP }
    | '(' { LPAREN }
    | ')' { RPAREN }
    | '[' { LBRAK }
    | ']' { RBRAK }
    | '{' { LLIST }
    | '}' { RLIST }
    | ',' { ARGSEP }
    | '=' { EQ }
    | ">>"  { MAPS }
    | "<<"  { MAPSREV }
    | '<' { LT }
    | '>' { GT }
    | "<=" { LEQ }
    | ">=" { GEQ }
    | "<>" { NEQ }
    | "<-" { ASSIGN }
    | ":=" { INIT }
    | '+' { PLUS }
    | '-' { MINUS }
    | '*' { TIMES }
    | '/' { DIVI }
    | '@' { CONCAT }
    | '#' { ID }
    | '^' { EXP }
    | '~' { LOG }
    | ">?" { MAX }
    | "<?" { MIN }
    | "|Z|" { Z }
    | "land" { LAND }
    | "lor" { LOR }
    | "lxor" { LXOR }
    | "lnot" { LNOT }
    | "and" { BAND }
    | "or" { BOR }
    | "xor" { BXOR }
    | "not" { BNOT }
    | "lsl" { LSL }
    | "lsr" { LSR }
    | "asl" { ASL }
    | "asr" { ASR }
    | '.' { DOT }
    | "variable" { VAR }
    | "var" { VAR }
    | "register" { REG }
    | "reg" { REG }
    | "signal" { SIGNAL }
    | "sig" { SIGNAL }
    | "array" { ARRAY }
    | "component" { COMP }
    | "comp" { COMP }
    | "channel" { CHAN }
    | "chan" { CHAN }
    | "queue" { QUEUE }
    | "map" { MAP } 
    | "of" { OF }
    | "in" { IN }
    | "is" { IS }
    | "constant" { CONST }
    | "const" { CONST }
    | "monitor" { MON }
    | "debug" { DEBUG }
    | "block" { DBLOCK }
    | "process" { PROCESS }
    | "module" { MODULE }
    | "function" { FUNCTION }
    | "return" { RETURN }
    | "export" { EXPORT }
    | "import" { IMPORT }
    | "include" { INCLUDE }
    | "domain" { DOMAIN }
    | "type" { TYPE }
    | "wait" { WAIT }
    | "for" { FOR }
    | "while" { WHILE }
    | "do" { DO }
    | "always" { ALWAYS }
    | "loop" { LOOP }
    | "if" { IF }
    | "then" { THEN }
    | "else" { ELSE }
    | "match" { MATCH }
    | "when" { WHEN }
    | "others" { OTHERS }
    | "begin" { BLOCK_START }
    | "end" { BLOCK_END }
    | "to" { TO }
    | "downto" { DOWNTO }
    | "with" { WITH }
    | "try" { TRY }
    | "raise" { RAISE }
    | "exception" { EXCEPTION }
    | "open" { OPEN }
    | "object" { OBJECT }
    | "port" { PORT }
    | "to_int" { TO_INT }
    | "to_bool" { TO_BOOL }
    | "to_logic" { TO_LOG }
    | "to_char" { TO_CHR }
    | "sizeof" { SIZEOF }
    | "nanosec" { NSEC }
    | "microsec" { USEC }
    | "millisec" { MSEC }
    | "sec" { SEC }
    | "gigahz" { GHZ }
    | "megahz" { MHZ }
    | "kilohz" { KHZ }
    | "hz" { HZ }
    | "()" { UNIT }
    | ['0' - '9' 'a' - 'z' 'A' - 'Z' '_' '%'  '!' '?']+ 
        { IDENT ({f_name= !file_name; f_cpos=lexbuf.Lexing.lex_curr_pos},
                Lexing.lexeme lexbuf) 
        }
    | ['0' - '9' 'a' - 'z' 'A' - 'Z' '_']+ ['\'']* 
        { IDENT ({f_name= !file_name; f_cpos=lexbuf.Lexing.lex_curr_pos},
                Str.global_replace (Str.regexp "'") "X" (Lexing.lexeme lexbuf)) 
        }
    | eof  { ENDFILE }
and comment = parse
    ['\n' '\r'] { COMMENT }
    | eof  { raise EOF }
    | _ { comment lexbuf }
and string = parse
    '"'
      { () }
  | '\\' ("\010" | "\013" | "\013\010") [' ' '\009'] *
      { string lexbuf }
  | eof { raise EOF }
  | _
      { store_string_char(Lexing.lexeme_char lexbuf 0);
        string lexbuf }
    
and getchar = parse
    '\''
      { () }
  | eof { raise EOF }
  | _
      { this_char := Some (Lexing.lexeme_char lexbuf 0);
        getchar lexbuf }
    
