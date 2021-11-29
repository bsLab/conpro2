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

let grammar = [|
     0,"$accept","%entry% $end";
     1,"main","VERSION STRING EOI";
     2,"main","LBRAK structs RBRAK EOI";
     3,"structs","struct1 EOI";
     4,"structs","struct1 EOI structs";
     5,"struct1","LCURL struct_entries RCURL";
     6,"struct_entries","struct_entry EOI";
     7,"struct_entries","struct_entry EOI struct_entries";
     8,"struct_entry","SYN_TOOL EQ STRING";
     9,"struct_entry","SYN_TOP EQ STRING";
    10,"struct_entry","SYN_VER EQ VALUE";
    11,"struct_entry","SYN_VHDL_MAP EQ LBRAK list2 RBRAK";
    12,"struct_entry","SYN_VHDL_MAP EQ LBRAK RBRAK";
    13,"struct_entry","SYN_VHDL_LIB EQ LBRAK list1 RBRAK";
    14,"struct_entry","SYN_VHDL_LIB EQ LBRAK RBRAK";
    15,"struct_entry","SYN_SYNTH_SET EQ LBRAK list21 RBRAK";
    16,"struct_entry","SYN_SYNTH_SET EQ LBRAK RBRAK";
    17,"struct_entry","SYN_TECH_SET EQ LBRAK list21 RBRAK";
    18,"struct_entry","SYN_TECH_SET EQ LBRAK RBRAK";
    19,"list21","str SEP LBRAK list2 RBRAK EOI";
    20,"list21","str SEP LBRAK list2 RBRAK EOI list21";
    21,"list2","str SEP str EOI";
    22,"list2","str SEP str EOI list2";
    23,"list1","str EOI";
    24,"list1","str EOI list1";
    25,"str","STRING";
    26,"str","STRING ADD str";
    27,"%entry%","'\001' main";
|]
let print s = print_string s; print_newline ()
let print_grammar state =
  let glen = Array.length grammar in
  if state > 0 && state < glen then (
    let i,rname,rule = grammar.(state) in
    print (Printf.sprintf "%4d %s: %s" i rname rule)
    )
open Parsing
let parse_error s = 
  let print s = print_string s; print_newline () in
  if (Parsing.parser_trace ()) then (
  print "====== RULE STACK TRACE =======";
  let mutable r = pop_rule_stack () in
  while r <> (-1) do
    print_grammar r;
    r <- pop_rule_stack ();
  done;
  print "====== TOKEN STACK TRACE =======";
  let mutable t = pop_token_stack () in
  while t <> "" do
    print_string t; print_string " ";
    t <- pop_token_stack ();
  done;
  print_newline ()
  )
let yytransl_const = [|
  260 (* COMMENT *);
  261 (* VERSION *);
  262 (* EOI *);
  263 (* SEP *);
  264 (* LBRAK *);
  265 (* RBRAK *);
  266 (* LCURL *);
  267 (* RCURL *);
  268 (* ADD *);
  269 (* SYN_TOOL *);
  270 (* SYN_TOP *);
  271 (* SYN_VER *);
  272 (* SYN_VHDL_MAP *);
  273 (* SYN_SYNTH_SET *);
  274 (* SYN_TECH_SET *);
  275 (* SYN_VHDL_LIB *);
  276 (* EQ *);
    0|]

let yytransl_block = [|
  257 (* STRING *);
  258 (* VALUE *);
  259 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\004\000\004\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\008\000\008\000\006\000\006\000\007\000\007\000\
\009\000\009\000\000\000"

let yylen = "\002\000\
\003\000\004\000\002\000\003\000\003\000\002\000\003\000\003\000\
\003\000\003\000\005\000\004\000\005\000\004\000\005\000\004\000\
\005\000\004\000\006\000\007\000\004\000\005\000\002\000\003\000\
\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\027\000\000\000\000\000\000\000\
\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\005\000\000\000\002\000\004\000\
\008\000\009\000\010\000\000\000\000\000\000\000\000\000\007\000\
\000\000\012\000\000\000\000\000\016\000\000\000\000\000\018\000\
\000\000\014\000\000\000\000\000\000\000\011\000\000\000\015\000\
\000\000\017\000\013\000\000\000\026\000\000\000\000\000\024\000\
\000\000\000\000\022\000\000\000\000\000\020\000"

let yydgoto = "\002\000\
\005\000\008\000\009\000\018\000\019\000\043\000\051\000\046\000\
\044\000"

let yysindex = "\008\000\
\012\255\000\000\010\255\009\255\000\000\025\255\011\255\024\255\
\029\255\000\000\016\255\017\255\018\255\019\255\020\255\021\255\
\022\255\032\255\038\255\039\255\009\255\045\255\046\255\047\255\
\040\255\042\255\043\255\044\255\000\000\011\255\000\000\000\000\
\000\000\000\000\000\000\003\255\004\255\005\255\006\255\000\000\
\041\255\000\000\048\255\049\255\000\000\050\255\051\255\000\000\
\052\255\000\000\053\255\054\255\062\255\000\000\062\255\000\000\
\056\255\000\000\000\000\062\255\000\000\059\255\062\255\000\000\
\062\255\057\255\000\000\061\255\062\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\060\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\063\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\015\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\064\255\000\000\000\000\000\000\000\000\
\066\255\000\000\000\000\000\000\067\255\000\000"

let yygindex = "\000\000\
\000\000\033\000\000\000\025\000\000\000\201\255\010\000\221\255\
\219\255"

let yytablesize = 76
let yytable = "\047\000\
\047\000\052\000\049\000\041\000\041\000\041\000\041\000\066\000\
\001\000\067\000\006\000\042\000\045\000\048\000\050\000\061\000\
\003\000\062\000\007\000\004\000\025\000\025\000\052\000\011\000\
\012\000\013\000\014\000\015\000\016\000\017\000\010\000\047\000\
\020\000\070\000\021\000\022\000\023\000\024\000\025\000\026\000\
\027\000\028\000\029\000\030\000\031\000\033\000\034\000\036\000\
\035\000\037\000\038\000\039\000\053\000\032\000\040\000\055\000\
\054\000\057\000\056\000\060\000\058\000\059\000\041\000\063\000\
\065\000\068\000\069\000\000\000\003\000\064\000\000\000\000\000\
\023\000\006\000\021\000\019\000"

let yycheck = "\037\000\
\038\000\039\000\038\000\001\001\001\001\001\001\001\001\063\000\
\001\000\065\000\001\001\009\001\009\001\009\001\009\001\053\000\
\005\001\055\000\010\001\008\001\006\001\007\001\060\000\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\006\001\069\000\
\009\001\069\000\006\001\020\001\020\001\020\001\020\001\020\001\
\020\001\020\001\011\001\006\001\006\001\001\001\001\001\008\001\
\002\001\008\001\008\001\008\001\012\001\021\000\030\000\007\001\
\009\001\007\001\009\001\006\001\009\001\009\001\001\001\008\001\
\006\001\009\001\006\001\255\255\009\001\060\000\255\255\255\255\
\009\001\011\001\009\001\009\001"

let yynames_const = "\
  COMMENT\000\
  VERSION\000\
  EOI\000\
  SEP\000\
  LBRAK\000\
  RBRAK\000\
  LCURL\000\
  RCURL\000\
  ADD\000\
  SYN_TOOL\000\
  SYN_TOP\000\
  SYN_VER\000\
  SYN_VHDL_MAP\000\
  SYN_SYNTH_SET\000\
  SYN_TECH_SET\000\
  SYN_VHDL_LIB\000\
  EQ\000\
  "

let yynames_block = "\
  STRING\000\
  VALUE\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : Cp_syntax.file_pos*string) in
    Obj.repr((
# 43 "../../src/ml/cp_parser_tools.mly"
                        Cp_syntax.TS_version _2 ) : Cp_syntax.synth_tools_syntax))
; (fun parser_env ->
    let _2 = (peek_val parser_env 2 : 'structs) in
    Obj.repr((
# 44 "../../src/ml/cp_parser_tools.mly"
                             Cp_syntax.TS_structs _2 ) : Cp_syntax.synth_tools_syntax))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'struct1) in
    Obj.repr((
# 50 "../../src/ml/cp_parser_tools.mly"
                  [ _1 ] ) : 'structs))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'struct1) in
    let _3 = (peek_val parser_env 0 : 'structs) in
    Obj.repr((
# 51 "../../src/ml/cp_parser_tools.mly"
                          _1 :: _3 ) : 'structs))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'struct_entries) in
    Obj.repr((
# 55 "../../src/ml/cp_parser_tools.mly"
                                 Cp_syntax.TS_struct _2 ) : 'struct1))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'struct_entry) in
    Obj.repr((
# 59 "../../src/ml/cp_parser_tools.mly"
                       [ _1 ] ) : 'struct_entries))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'struct_entry) in
    let _3 = (peek_val parser_env 0 : 'struct_entries) in
    Obj.repr((
# 60 "../../src/ml/cp_parser_tools.mly"
                                      _1 :: _3 ) : 'struct_entries))
; (fun parser_env ->
    let _3 = (peek_val parser_env 0 : Cp_syntax.file_pos*string) in
    Obj.repr((
# 64 "../../src/ml/cp_parser_tools.mly"
                         Cp_syntax.TS_param ("syn_tool", _3) ) : 'struct_entry))
; (fun parser_env ->
    let _3 = (peek_val parser_env 0 : Cp_syntax.file_pos*string) in
    Obj.repr((
# 65 "../../src/ml/cp_parser_tools.mly"
                         Cp_syntax.TS_param("syn_top", _3) ) : 'struct_entry))
; (fun parser_env ->
    let _3 = (peek_val parser_env 0 : Cp_syntax.file_pos*string) in
    Obj.repr((
# 66 "../../src/ml/cp_parser_tools.mly"
                        Cp_syntax.TS_param ("syn_ver", _3) ) : 'struct_entry))
; (fun parser_env ->
    let _4 = (peek_val parser_env 1 : 'list2) in
    Obj.repr((
# 67 "../../src/ml/cp_parser_tools.mly"
                                         Cp_syntax.TS_paraml2 ("syn_vhdl_map", _4) ) : 'struct_entry))
; (fun parser_env ->
    Obj.repr((
# 68 "../../src/ml/cp_parser_tools.mly"
                                   Cp_syntax.TS_paraml2 ("syn_vhdl_map", []) ) : 'struct_entry))
; (fun parser_env ->
    let _4 = (peek_val parser_env 1 : 'list1) in
    Obj.repr((
# 69 "../../src/ml/cp_parser_tools.mly"
                                         Cp_syntax.TS_paraml ("syn_vhdl_lib", _4) ) : 'struct_entry))
; (fun parser_env ->
    Obj.repr((
# 70 "../../src/ml/cp_parser_tools.mly"
                                   Cp_syntax.TS_paraml ("syn_vhdl_lib", []) ) : 'struct_entry))
; (fun parser_env ->
    let _4 = (peek_val parser_env 1 : 'list21) in
    Obj.repr((
# 71 "../../src/ml/cp_parser_tools.mly"
                                           Cp_syntax.TS_paraml21 ("syn_synth_set", _4) ) : 'struct_entry))
; (fun parser_env ->
    Obj.repr((
# 72 "../../src/ml/cp_parser_tools.mly"
                                    Cp_syntax.TS_paraml21 ("syn_synth_set", []) ) : 'struct_entry))
; (fun parser_env ->
    let _4 = (peek_val parser_env 1 : 'list21) in
    Obj.repr((
# 73 "../../src/ml/cp_parser_tools.mly"
                                          Cp_syntax.TS_paraml21 ("syn_tech_set", _4) ) : 'struct_entry))
; (fun parser_env ->
    Obj.repr((
# 74 "../../src/ml/cp_parser_tools.mly"
                                   Cp_syntax.TS_paraml21 ("syn_tech_set", []) ) : 'struct_entry))
; (fun parser_env ->
    let _1 = (peek_val parser_env 5 : 'str) in
    let _4 = (peek_val parser_env 2 : 'list2) in
    Obj.repr((
# 78 "../../src/ml/cp_parser_tools.mly"
                                    [_1,_4] ) : 'list21))
; (fun parser_env ->
    let _1 = (peek_val parser_env 6 : 'str) in
    let _4 = (peek_val parser_env 3 : 'list2) in
    let _7 = (peek_val parser_env 0 : 'list21) in
    Obj.repr((
# 79 "../../src/ml/cp_parser_tools.mly"
                                           (_1,_4) :: _7 ) : 'list21))
; (fun parser_env ->
    let _1 = (peek_val parser_env 3 : 'str) in
    let _3 = (peek_val parser_env 1 : 'str) in
    Obj.repr((
# 82 "../../src/ml/cp_parser_tools.mly"
                      [ _1,_3] ) : 'list2))
; (fun parser_env ->
    let _1 = (peek_val parser_env 4 : 'str) in
    let _3 = (peek_val parser_env 2 : 'str) in
    let _5 = (peek_val parser_env 0 : 'list2) in
    Obj.repr((
# 83 "../../src/ml/cp_parser_tools.mly"
                            (_1,_3) :: _5 ) : 'list2))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'str) in
    Obj.repr((
# 86 "../../src/ml/cp_parser_tools.mly"
              [ _1] ) : 'list1))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'str) in
    let _3 = (peek_val parser_env 0 : 'list1) in
    Obj.repr((
# 87 "../../src/ml/cp_parser_tools.mly"
                    _1 :: _3 ) : 'list1))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : Cp_syntax.file_pos*string) in
    Obj.repr((
# 91 "../../src/ml/cp_parser_tools.mly"
             _1 ) : 'str))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : Cp_syntax.file_pos*string) in
    let _3 = (peek_val parser_env 0 : 'str) in
    Obj.repr((
# 92 "../../src/ml/cp_parser_tools.mly"
                     let p1,s1 = _1 in let p2,s2= _3 in (p1,s1^s2) ) : 'str))
(* Entry main *)
; (fun parser_env -> raise (YYexit (peek_val parser_env 0)))
|]
let yytables =
  { actions=yyact;
    transl_const=yytransl_const;
    transl_block=yytransl_block;
    lhs=yylhs;
    len=yylen;
    defred=yydefred;
    dgoto=yydgoto;
    sindex=yysindex;
    rindex=yyrindex;
    gindex=yygindex;
    tablesize=yytablesize;
    table=yytable;
    check=yycheck;
    error_function=parse_error;
    names_const=yynames_const;
    names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (yyparse yytables 1 lexfun lexbuf : Cp_syntax.synth_tools_syntax)
