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
**    $INITIAL:     (C) 2006-2010 BSSLAB
**    $CREATED:     7.7.2008
**    $VERSION:     2.21
**
**    $INFO:
**
**  CONPRO lexer for the External Module Interface
**
**    $ENDOFINFO
*)
    open Cp_parser_emi
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
    | [';'] { EOI }
    | ':' { SEP1 }
    | ',' { SEP2 }
    | '(' { LPAREN }
    | ')' { RPAREN }
    | '[' { LBRAK }
    | ']' { RBRAK }
    | '{' { LCURL }
    | '}' { RCURL }
    | '=' { EQ }
    | '<' { LT }
    | '>' { GT }
    | "<=" { ASSIGN }
    | ":=" { ASSIGN2 }
    | "=>" { FOLLOW }
    | ">=" { GEQ }
    | "/=" { NEQ }
    | '+' { PLUS }
    | '-' { MINUS }
    | '*' { TIMES }
    | '/' { DIVI }
    | '^' { EXP2 }
    | '\'' { ACC }
    | '&' { CONC }
    | '|' { OR }
    | "and" { AND }
    | "or" { OR }
    | "xor" { XOR }
    | "not" { NOT }
    
    
    | "#version" { S_VERSION }
    | "#parameter" { S_PARAMETER }
    | "#methods" { S_METHODS }
    | "#signals" { S_SIGNALS }
    | "#interface" { S_INTERFACE }
    | "#mapping" { S_MAPPING }
    | "#access" { S_ACCESS }
    | "#process" { S_PROCESS }
    | "#data" { S_DATA }
    | "#control" { S_CONTROL }
    | "#import" { S_IMPORT }
    | "#set" { S_SET }
    | "#top" { S_TOP }
    | "#assert" { S_CHECK }
    
    | "#lhs" { LHS }
    | "#rhs" { RHS }
    | "#lrhs" { LRHS }
    
    | "foreach" { FOREACH }
    | "for" { FOR }
    | "do" { DO }
    | "in" { IN }
    | "is" { IS }
    | "when" { WHEN }
    | "wait" { WAIT }
    | "for" { FOR }
    | "until" { UNTIL }
    | "if" { IF }
    | "then" { THEN }
    | "else" { ELSE }
    | "elsif" { ELSIF }
    | "begin" { BEGIN }
    | "end" { END }
    | "signal" { SIGNAL }
    | "constant" { CONSTANT }
    | "variable" { VARIABLE }
    | "shared" { SHARED }
    | "null" { NULL }
    | "others" { OTHERS }
    | "sequence" { SEQUENCE }
    | "type" { TYPE }
    | "case" { CASE }
    | "with" { WITH }
    | "when" { WHEN }
    | "array" { ARRAY }
    
    | "of" { OF }
    | "to" { TO }
    | "downto" { DOWNTO }
    | "in" { IN }
    | "out" { OUT }
    | "is" { IS }
      
    | "logic" { LOGIC }
    | "std_logic" { LOGIC }
    | "std_logic_vector" { LOGIC }
    | "natural" { NATURAL }
    | "bool" { BOOL }
    | "int" { INT }
    | "integer" { NATURAL }
    | "signed" { SIGNED }
    | "char" { CHAR }
    | "string" { STRINGS }
    | "var" { VAR }
    

    | "nanosec" { NSEC }
    | "microsec" { USEC }
    | "millisec" { MSEC }
    | "sec" { SEC }
    | "gigahz" { GHZ }
    | "megahz" { MHZ }
    | "kilohz" { KHZ }
    | "hz" { HZ }
   
    | '$' { VAR }
    | ['0' - '9']+
        { VALUE ('d',{f_name= !file_name; f_cpos=lexbuf.Lexing.lex_curr_pos},
                Lexing.lexeme lexbuf) 
        }
    | '0' 'x' ['0'-'9' 'a'-'f']+
        { VALUE ('x',{f_name= !file_name; f_cpos=lexbuf.Lexing.lex_curr_pos},
                Lexing.lexeme lexbuf) 
        }
    | '0' 'b' ['0'-'1']+
        { VALUE ('b',{f_name= !file_name; f_cpos=lexbuf.Lexing.lex_curr_pos},
                Lexing.lexeme lexbuf) 
        }
    | '0' 'o' ['0'-'8']+
        { VALUE ('o',{f_name= !file_name; f_cpos=lexbuf.Lexing.lex_curr_pos},
                Lexing.lexeme lexbuf) 
        }
    | ['0' - '9']+ '.' ['0'-'9']+
        { VALUE ('f',{f_name= !file_name; f_cpos=lexbuf.Lexing.lex_curr_pos},
                Lexing.lexeme lexbuf) 
        }
    | "false"
        { VALUE ('L',{f_name= !file_name; f_cpos=lexbuf.Lexing.lex_curr_pos},
                "false") 
        }
    | "true"
        { VALUE ('L',{f_name= !file_name; f_cpos=lexbuf.Lexing.lex_curr_pos},
                "true") 
        }
    | '\'' ['0' '1' 'x' 'z' 'Z' 'X' 'H' 'L'] '\''  
        { VALUE ('B',{f_name= !file_name; f_cpos=lexbuf.Lexing.lex_curr_pos},
                Lexing.lexeme lexbuf) 
        }
    | ['a' - 'z' 'A' - 'Z' '_'   ] ['0' - '9' 'a' - 'z' 'A' - 'Z' '_' '.' '!']+ 
        { IDENT ({f_name= !file_name; f_cpos=lexbuf.Lexing.lex_curr_pos},
                Lexing.lexeme lexbuf) 
        }
    | ['a' - 'z' 'A' - 'Z']  
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
