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

let grammar = [|
     0,"$accept","%entry% $end";
     1,"main","expr EOI";
     2,"expr","expr PLUS expr";
     3,"expr","MINUS expr";
     4,"expr","expr MINUS expr";
     5,"expr","expr TIMES expr";
     6,"expr","expr DIVI expr";
     7,"expr","expr EQ expr";
     8,"expr","expr LT expr";
     9,"expr","expr GT expr";
    10,"expr","expr LEQ expr";
    11,"expr","expr GEQ expr";
    12,"expr","expr NEQ expr";
    13,"expr","LPAREN expr RPAREN";
    14,"expr","element";
    15,"element","element U_NS";
    16,"element","element U_US";
    17,"element","IDENT";
    18,"element","PARAM";
    19,"element","STRING";
    20,"%entry%","'\001' main";
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
  260 (* DATAWIDTH *);
  261 (* CYCLETIME *);
  262 (* U_NS *);
  263 (* U_US *);
  264 (* EOI *);
  265 (* SEP *);
  266 (* LPAREN *);
  267 (* RPAREN *);
  268 (* EQ *);
  269 (* NEQ *);
  270 (* LT *);
  271 (* GT *);
  272 (* LEQ *);
  273 (* GEQ *);
  274 (* PLUS *);
  275 (* MINUS *);
  276 (* TIMES *);
  277 (* DIVI *);
    0|]

let yytransl_block = [|
  257 (* STRING *);
  258 (* IDENT *);
  259 (* PARAM *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\003\000\003\000\
\003\000\003\000\003\000\000\000"

let yylen = "\002\000\
\002\000\003\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\001\000\002\000\002\000\
\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\019\000\017\000\018\000\000\000\000\000\020\000\
\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\015\000\
\016\000\013\000\007\000\012\000\008\000\009\000\010\000\011\000\
\000\000\000\000\000\000\000\000"

let yydgoto = "\002\000\
\008\000\009\000\010\000"

let yysindex = "\006\000\
\255\254\000\000\000\000\000\000\000\000\255\254\255\254\000\000\
\035\255\254\254\076\255\086\255\000\000\255\254\255\254\255\254\
\255\254\255\254\255\254\255\254\255\254\255\254\255\254\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\086\255\086\255\096\255\096\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\021\255\000\000\053\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\055\255\067\255\251\254\049\255"

let yygindex = "\000\000\
\000\000\005\000\000\000"

let yytablesize = 113
let yytable = "\003\000\
\004\000\005\000\005\000\024\000\025\000\005\000\001\000\000\000\
\006\000\000\000\011\000\012\000\005\000\005\000\005\000\005\000\
\000\000\007\000\027\000\028\000\029\000\030\000\031\000\032\000\
\033\000\034\000\035\000\036\000\014\000\000\000\000\000\014\000\
\014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
\014\000\014\000\013\000\000\000\000\000\000\000\014\000\015\000\
\016\000\017\000\018\000\019\000\020\000\021\000\022\000\023\000\
\006\000\000\000\000\000\006\000\003\000\000\000\002\000\003\000\
\000\000\002\000\006\000\006\000\006\000\006\000\003\000\003\000\
\002\000\002\000\004\000\000\000\000\000\004\000\000\000\000\000\
\000\000\000\000\000\000\000\000\004\000\004\000\026\000\014\000\
\015\000\016\000\017\000\018\000\019\000\020\000\021\000\022\000\
\023\000\014\000\015\000\016\000\017\000\018\000\019\000\000\000\
\000\000\022\000\023\000\014\000\015\000\016\000\017\000\018\000\
\019\000"

let yycheck = "\001\001\
\002\001\003\001\008\001\006\001\007\001\011\001\001\000\255\255\
\010\001\255\255\006\000\007\000\018\001\019\001\020\001\021\001\
\255\255\019\001\014\000\015\000\016\000\017\000\018\000\019\000\
\020\000\021\000\022\000\023\000\008\001\255\255\255\255\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\008\001\255\255\255\255\255\255\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\008\001\255\255\255\255\011\001\008\001\255\255\008\001\011\001\
\255\255\011\001\018\001\019\001\020\001\021\001\018\001\019\001\
\018\001\019\001\008\001\255\255\255\255\011\001\255\255\255\255\
\255\255\255\255\255\255\255\255\018\001\019\001\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\012\001\013\001\014\001\015\001\016\001\017\001\255\255\
\255\255\020\001\021\001\012\001\013\001\014\001\015\001\016\001\
\017\001"

let yynames_const = "\
  DATAWIDTH\000\
  CYCLETIME\000\
  U_NS\000\
  U_US\000\
  EOI\000\
  SEP\000\
  LPAREN\000\
  RPAREN\000\
  EQ\000\
  NEQ\000\
  LT\000\
  GT\000\
  LEQ\000\
  GEQ\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVI\000\
  "

let yynames_block = "\
  STRING\000\
  IDENT\000\
  PARAM\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'expr) in
    Obj.repr((
# 47 "../../src/ml/cp_parser_expr.mly"
                 _1 ) : Cp_syntax.expr_syntax))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'expr) in
    let _3 = (peek_val parser_env 0 : 'expr) in
    Obj.repr((
# 55 "../../src/ml/cp_parser_expr.mly"
                       Cp_syntax.TE_expr ("+",_1,_3) ) : 'expr))
; (fun parser_env ->
    let _2 = (peek_val parser_env 0 : 'expr) in
    Obj.repr((
# 56 "../../src/ml/cp_parser_expr.mly"
                   Cp_syntax.TE_expr ("-",Cp_syntax.TE_empty,_2) ) : 'expr))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'expr) in
    let _3 = (peek_val parser_env 0 : 'expr) in
    Obj.repr((
# 57 "../../src/ml/cp_parser_expr.mly"
                        Cp_syntax.TE_expr ("-",_1,_3) ) : 'expr))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'expr) in
    let _3 = (peek_val parser_env 0 : 'expr) in
    Obj.repr((
# 58 "../../src/ml/cp_parser_expr.mly"
                        Cp_syntax.TE_expr ("*",_1,_3) ) : 'expr))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'expr) in
    let _3 = (peek_val parser_env 0 : 'expr) in
    Obj.repr((
# 59 "../../src/ml/cp_parser_expr.mly"
                       Cp_syntax.TE_expr ("/",_1,_3) ) : 'expr))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'expr) in
    let _3 = (peek_val parser_env 0 : 'expr) in
    Obj.repr((
# 60 "../../src/ml/cp_parser_expr.mly"
                     Cp_syntax.TE_expr ("=",_1,_3) ) : 'expr))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'expr) in
    let _3 = (peek_val parser_env 0 : 'expr) in
    Obj.repr((
# 61 "../../src/ml/cp_parser_expr.mly"
                     Cp_syntax.TE_expr ("<",_1,_3) ) : 'expr))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'expr) in
    let _3 = (peek_val parser_env 0 : 'expr) in
    Obj.repr((
# 62 "../../src/ml/cp_parser_expr.mly"
                     Cp_syntax.TE_expr (">",_1,_3) ) : 'expr))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'expr) in
    let _3 = (peek_val parser_env 0 : 'expr) in
    Obj.repr((
# 63 "../../src/ml/cp_parser_expr.mly"
                      Cp_syntax.TE_expr ("<=",_1,_3) ) : 'expr))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'expr) in
    let _3 = (peek_val parser_env 0 : 'expr) in
    Obj.repr((
# 64 "../../src/ml/cp_parser_expr.mly"
                      Cp_syntax.TE_expr (">=",_1,_3) ) : 'expr))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'expr) in
    let _3 = (peek_val parser_env 0 : 'expr) in
    Obj.repr((
# 65 "../../src/ml/cp_parser_expr.mly"
                      Cp_syntax.TE_expr ("<>",_1,_3) ) : 'expr))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'expr) in
    Obj.repr((
# 66 "../../src/ml/cp_parser_expr.mly"
                           _2 ) : 'expr))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'element) in
    Obj.repr((
# 67 "../../src/ml/cp_parser_expr.mly"
                _1 ) : 'expr))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'element) in
    Obj.repr((
# 71 "../../src/ml/cp_parser_expr.mly"
                  Cp_syntax.TE_unit (_1,"1E-9") ) : 'element))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'element) in
    Obj.repr((
# 72 "../../src/ml/cp_parser_expr.mly"
                  Cp_syntax.TE_unit (_1,"1E-6") ) : 'element))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : Cp_syntax.file_pos*string) in
    Obj.repr((
# 73 "../../src/ml/cp_parser_expr.mly"
            Cp_syntax.TE_ident _1 ) : 'element))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : Cp_syntax.file_pos*string) in
    Obj.repr((
# 74 "../../src/ml/cp_parser_expr.mly"
           Cp_syntax.TE_param  _1 ) : 'element))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : Cp_syntax.file_pos*string) in
    Obj.repr((
# 75 "../../src/ml/cp_parser_expr.mly"
             Cp_syntax.TE_string _1 ) : 'element))
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
   (yyparse yytables 1 lexfun lexbuf : Cp_syntax.expr_syntax)
