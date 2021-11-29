/*
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
**    $VERSION:     2.18
**
**    $INFO:
**
**  CONPRO parser
**
**    $ENDOFINFO
*/
%token <Cp_syntax.file_pos*string> STRING
%token <Cp_syntax.file_pos*char> CHAR
%token <Cp_syntax.file_pos*string> IDENT
%token VAR REG CONST SIGNAL ARRAY OBJECT COMP CHAN QUEUE
%token PROCESS MODULE  FUNCTION RETURN IMPORT EXPORT INCLUDE  CONNECT MAP MAPS MAPSREV
%token BLOCK_START BLOCK_END ENDFILE
%token WAIT FOR WHILE LOOP DO ALWAYS IF THEN ELSE MATCH WHEN OTHERS TRY RAISE EXCEPTION
%token SEP ARGSEP DOT IS TYPE PORT WITH 
%token OF IN TO DOWNTO CONCAT ID Z
%token MON DEBUG DOMAIN
%token LLIST RLIST LBRAK RBRAK LPAREN RPAREN
%token ASSIGN INIT OPEN DBLOCK UNIT 
%token TO_INT TO_BOOL TO_LOG TO_CHR SIZEOF
%token EOI
%token EQ NEQ LT GT LEQ GEQ MAX MIN
%token PLUS MINUS TIMES DIVI
%token EXP LOG
%left PLUS MINUS
%left TIMES DIVI
%left EXP LOG
%token LNOT LOR LXOR LAND BNOT BOR BXOR BAND
%token LSL LSR ASL ASR
%left LOR LXOR
%left LAND
%left BOR BXOR
%left BAND
%left EQ NEQ LT GT LEQ GEQ
%left DOT
%token COMMENT 
%token NSEC USEC MSEC SEC HZ KHZ MHZ GHZ
%start main
%type <Cp_syntax.syntax> main
%%
main:
    | main_instr EOI { $1 }
    | top_instr EOI { $1 }
    | ENDFILE { raise Cp_lexer.EOF }
;

main_instr:
    | OPEN expr { Cp_syntax.T_module $2 }
    | INCLUDE STRING  { Cp_syntax.T_include (Cp_syntax.T_string $2) }
    | PROCESS expr SEP block_env  { Cp_syntax.T_process ($2,$4) } 
    | PROCESS expr SEP IMPORT  { Cp_syntax.T_process ($2,Cp_syntax.T_empty) } 
    | MODULE expr SEP top_block_env { Cp_syntax.T_module_def ($2,$4) }
    | FUNCTION expr LPAREN arg_list RPAREN SEP block_env  { Cp_syntax.T_Fun_def ($2,[],$4,$7) } 
    | FUNCTION expr UNIT SEP block_env  { Cp_syntax.T_Fun_def ($2,[],[],$5) } 
    | FUNCTION expr LPAREN arg_list RPAREN RETURN LPAREN arg_list RPAREN SEP block_env  { Cp_syntax.T_Fun_def ($2,$8,$4,$11) } 
    | FUNCTION expr UNIT RETURN LPAREN arg_list RPAREN SEP block_env  { Cp_syntax.T_Fun_def ($2,$6,[],$9) } 
    | EXPORT element_list  { Cp_syntax.T_export $2 }
    | IMPORT element_list  { Cp_syntax.T_import $2 }
;

top_block_or_instr:
    | top_instr { $1 }
    | top_block_env { $1 }
;

top_instr:
    | mon_map  { Cp_syntax.T_topinstr ($1) }
    | func { Cp_syntax.T_topinstr ($1) }
    | top_decl  { $1 }
    | decl  { $1 }
    | FOR expr EQ expr TO expr DO top_block_or_instr { Cp_syntax.T_topinstr(Cp_syntax.T_forloop ($2,'+',$4,$6,$8)) }
    | FOR expr EQ expr DOWNTO expr DO top_block_or_instr { Cp_syntax.T_topinstr(Cp_syntax.T_forloop ($2,'-',$4,$6,$8)) }
    | IF expr THEN top_block_or_instr ELSE top_block_or_instr { Cp_syntax.T_topinstr(Cp_syntax.T_branch ($2,$4,$6)) }
    | IF expr THEN top_block_or_instr { Cp_syntax.T_topinstr(Cp_syntax.T_branch ($2,$4,Cp_syntax.T_empty)) }
    | main_instr { $1 }
;

top_block_env :
    | BLOCK_START top_block BLOCK_END { Cp_syntax.T_block ((Cp_syntax.T_list $2),None) }
    | BLOCK_START top_block BLOCK_END WITH expr { Cp_syntax.T_block ((Cp_syntax.T_list $2),Some $5) }
;

top_block :
    | top_instr EOI { [$1] }
    | top_instr EOI top_block { $1::$3 }
;

top_block_instr :
    | top_instr { Cp_syntax.T_block ((Cp_syntax.T_list [$1]),None) }
;

decl:
    | TYPE expr SEP LLIST type_list RLIST  { Cp_syntax.T_typedef ($2,(Cp_syntax.T_list $5),Cp_syntax.T_empty) }
    | TYPE expr SEP LLIST type_list RLIST WITH expr { Cp_syntax.T_typedef ($2,(Cp_syntax.T_list $5),$8) }
    | EXCEPTION element { Cp_syntax.T_exception $2 }
    | COMP element_list SEP expr { Cp_syntax.T_OT_comp ($2,$4,[]) }
    | COMP element_list SEP expr INIT LLIST element_list RLIST { Cp_syntax.T_OT_comp ($2,$4,$7) }
    | CONST element_list SEP expr INIT expr { Cp_syntax.T_OT_const ($2,$4,$6) }
    | SIGNAL element_list SEP expr { Cp_syntax.T_OT_sig ($2,$4,Cp_syntax.T_empty) }
    | SIGNAL element_list SEP expr INIT expr { Cp_syntax.T_OT_sig ($2,$4,$6) }
    | ARRAY element_list SEP OBJECT element {Cp_syntax.T_OT_array ($2,'o',$5,Cp_syntax.T_empty,Cp_syntax.T_empty,Cp_syntax.T_empty)}
    | ARRAY element_list SEP OBJECT element WITH expr {Cp_syntax.T_OT_array ($2,'o',$5,Cp_syntax.T_empty,Cp_syntax.T_empty,$7)}
    | ARRAY element_list SEP COMP element {Cp_syntax.T_OT_array ($2,'c',$5,Cp_syntax.T_empty,Cp_syntax.T_empty,Cp_syntax.T_empty)}
    | ARRAY element_list SEP COMP element WITH expr {Cp_syntax.T_OT_array ($2,'c',$5,Cp_syntax.T_empty,Cp_syntax.T_empty,$7)}
    | ARRAY element_list SEP element OF element { Cp_syntax.T_OT_array ($2,' ',$4,$6,Cp_syntax.T_empty,Cp_syntax.T_empty) }
    | ARRAY element_list SEP element OF element WITH expr { Cp_syntax.T_OT_array ($2,' ',$4,$6,Cp_syntax.T_empty,$8) }
    | ARRAY element_list SEP element OF block_env { Cp_syntax.T_OT_array ($2,'p',$4,$6,Cp_syntax.T_empty,Cp_syntax.T_empty) }
    | ARRAY element_list SEP element OF element IN element { Cp_syntax.T_OT_array ($2,' ',$4,$6,$8,Cp_syntax.T_empty) }
    | REG element_list SEP expr { Cp_syntax.T_OT_reg ($2,$4,Cp_syntax.T_empty,Cp_syntax.T_empty) }
    | REG element_list SEP expr WITH expr { Cp_syntax.T_OT_reg ($2,$4,Cp_syntax.T_empty,$6) }
    | REG element_list SEP expr INIT expr { Cp_syntax.T_OT_reg ($2,$4,$6,Cp_syntax.T_empty) }
    | VAR element_list SEP expr IN expr { Cp_syntax.T_OT_var ($2,$4,$6,Cp_syntax.T_empty,Cp_syntax.T_empty) }
    | VAR element_list SEP expr IN expr INIT expr { Cp_syntax.T_OT_var ($2,$4,$6,$8,Cp_syntax.T_empty) }
    | DBLOCK expr { Cp_syntax.T_data_block ($2,Cp_syntax.T_empty) }
    | DBLOCK expr WITH expr { Cp_syntax.T_data_block ($2,$4) }
    | OBJECT element_list SEP expr { Cp_syntax.T_OT_object ($2,$4,Cp_syntax.T_empty) }
    | OBJECT element_list SEP expr WITH expr { Cp_syntax.T_OT_object ($2,$4,$6) }
;

type_list:
    | type_expr EOI { [$1] }
    | type_expr EOI type_list { $1::$3 }
;

type_expr:
    | PORT element_list SEP expr expr { Cp_syntax.T_OT ($2,[$4;$5]) }    
    | element_list SEP expr { Cp_syntax.T_OT ($1,[$3]) }
    | IDENT { Cp_syntax.T_ident $1 }
;

arg_list:
    | arg_expr { [$1] }
    | arg_expr ARGSEP arg_list { $1::$3 }
;

arg_expr:
    | element_list SEP expr { Cp_syntax.T_OT_reg ($1,$3,Cp_syntax.T_empty,Cp_syntax.T_empty) }
    | IDENT { Cp_syntax.T_ident $1 }
;

top_decl:
    | CHAN element_list SEP expr { Cp_syntax.T_OT_chan ($2,$4,Cp_syntax.T_empty) }
    | CHAN element_list SEP expr WITH expr { Cp_syntax.T_OT_chan ($2,$4,$6) }
    | QUEUE element_list SEP expr { Cp_syntax.T_OT_que ($2,$4,Cp_syntax.T_empty) }
    | QUEUE element_list SEP expr WITH expr { Cp_syntax.T_OT_que ($2,$4,$6) }
    | DOMAIN element_list SEP STRING { Cp_syntax.T_domain ($2,$4) }
;


block_env :
    | BLOCK_START block BLOCK_END { Cp_syntax.T_block ((Cp_syntax.T_list $2),None) }
    | BLOCK_START block BLOCK_END WITH expr { Cp_syntax.T_block ((Cp_syntax.T_list $2),Some $5) }
;

block_env_case_list :
    | BLOCK_START case_list BLOCK_END { Cp_syntax.T_block ((Cp_syntax.T_list $2),None) }
    | BLOCK_START case_list BLOCK_END WITH expr { Cp_syntax.T_block ((Cp_syntax.T_list $2),Some $5) }
;

block :
    | block_env EOI { [$1] }
    | block_env EOI block { $1::$3 }
    | control EOI { [$1] }
    | control EOI block { $1::$3 }
    | decl EOI { [$1] }
    | decl EOI block { $1::$3 }
    | mon_map EOI { [$1] }
    | mon_map EOI block { $1::$3 }
    | instr EOI { [Cp_syntax.T_instr $1] }
    | instr EOI block { (Cp_syntax.T_instr $1)::$3 }
    | instr_list EOI { [Cp_syntax.T_block ((Cp_syntax.T_list $1),Some (Cp_syntax.t_block_params ["bind","true"]))] }
    | instr_list EOI block { (Cp_syntax.T_block ((Cp_syntax.T_list $1),Some (Cp_syntax.t_block_params ["bind","true"]))) :: $3 }
;

case:
    | WHEN expr_list SEP instr EOI { Cp_syntax.T_case ($2,Cp_syntax.T_instr $4) }
    | WHEN expr_list SEP block_env EOI{ Cp_syntax.T_case ($2,Cp_syntax.T_instr $4) }
;
case_list:
    | case { [$1] }
    | case case_list { $1 :: $2 }
;
block_instr :
    | instr { Cp_syntax.T_block ((Cp_syntax.T_list [$1]),None) }
;

func:
    | expr UNIT { Cp_syntax.T_Fun ($1,[],Cp_syntax.T_empty) }
    | expr LPAREN args RPAREN { Cp_syntax.T_Fun ($1,$3,Cp_syntax.T_empty) }
;

mon_map:
    | element MAPS element { Cp_syntax.T_map ($3,$1) }
    | element MAPSREV element { Cp_syntax.T_map ($1,$3) }
    | MON element_list { Cp_syntax.T_monitor ($2,false) }
    | MON element_list WITH DEBUG { Cp_syntax.T_monitor ($2,true) }
;

time:
  | element { Cp_syntax.T_time($1,Cp_syntax.Cycles) }
  | element timeunit { Cp_syntax.T_time($1,$2) }
;

instr_list:
  | instr ARGSEP instr { [$1;$3] }
  | instr ARGSEP instr_list { $1 :: $3 }
;

block_or_instr:
  | instr { $1 }
  | block_env { $1 }
  | instr_list { Cp_syntax.T_block ((Cp_syntax.T_list $1),Some (Cp_syntax.t_block_params ["bind","true"])) }
;

block_or_instr2:
  | instr { $1 }
  | BLOCK_START block BLOCK_END { Cp_syntax.T_block ((Cp_syntax.T_list $2),None) }
;

control:
    | WAIT FOR time { Cp_syntax.T_waitfor (Cp_syntax.T_empty,$3,Cp_syntax.T_empty,Cp_syntax.T_empty) }
    | WAIT FOR time WITH block_or_instr { Cp_syntax.T_waitfor (Cp_syntax.T_empty,$3,$5,Cp_syntax.T_empty) }
    | WAIT FOR time WITH block_or_instr ELSE block_or_instr { Cp_syntax.T_waitfor (Cp_syntax.T_empty,$3,$5,$7) }
    | WAIT FOR expr { Cp_syntax.T_waitfor ($3,Cp_syntax.T_empty,Cp_syntax.T_empty,Cp_syntax.T_empty) }
    | WAIT FOR expr WITH block_or_instr { Cp_syntax.T_waitfor ($3,Cp_syntax.T_empty,$5,Cp_syntax.T_empty) }
    | WAIT FOR expr WITH block_or_instr ELSE block_or_instr { Cp_syntax.T_waitfor ($3,Cp_syntax.T_empty,$5,$7) }
    | IF expr THEN block_or_instr ELSE block_or_instr { Cp_syntax.T_branch ($2,$4,$6) }
    | IF expr THEN block_or_instr { Cp_syntax.T_branch ($2,$4,Cp_syntax.T_empty) }
    | FOR expr EQ expr TO expr DO block_instr { Cp_syntax.T_forloop ($2,'+',$4,$6,$8) }
    | FOR expr EQ expr TO expr DO block_env { Cp_syntax.T_forloop ($2,'+',$4,$6,$8) }
    | FOR expr EQ expr DOWNTO expr DO block_instr { Cp_syntax.T_forloop ($2,'-',$4,$6,$8) }
    | FOR expr EQ expr DOWNTO expr DO block_env { Cp_syntax.T_forloop ($2,'-',$4,$6,$8) }
    | WHILE expr DO block_instr { Cp_syntax.T_loop (Cp_syntax.Loop_while,$2,$4) }
    | WHILE expr DO block_env { Cp_syntax.T_loop (Cp_syntax.Loop_while,$2,$4) }
    | ALWAYS DO block_env { Cp_syntax.T_loop (Cp_syntax.Loop,Cp_syntax.T_empty,$3) }
    | MATCH expr WITH block_env_case_list { Cp_syntax.T_select ($2,$4) }
    | TRY block_or_instr2 WITH block_env_case_list { Cp_syntax.T_try ($2,$4) }
    | RAISE element { Cp_syntax.T_raise $2 }
;

instr :
    | element_list SEP expr { Cp_syntax.T_OT ($1,[$3]) }
    | expr ASSIGN expr { Cp_syntax.T_assign ($1,$3) }
    | IDENT  { Cp_syntax.T_ident $1 }
    | control { $1 }
    | func { $1 }
;

args :
    | expr { [$1] }
    | expr ARGSEP args { $1 :: $3 }
;

spargs :
    | expr { [$1] }
    | expr spargs { $1 :: $2 }
;





expr:
    | func { $1 }
    | expr PLUS expr { Cp_syntax.T_OP_arith ("+",$1,$3) }
    | MINUS expr { Cp_syntax.T_OP_arith ("-",Cp_syntax.T_empty,$2) }
    | LNOT expr { Cp_syntax.T_OP_arith ("lnot",Cp_syntax.T_empty,$2) }
    | BNOT expr { Cp_syntax.T_OP_bool (Cp_syntax.Bool,"not",Cp_syntax.T_empty,$2) }
    | expr MINUS expr { Cp_syntax.T_OP_arith ("-",$1,$3) }
    | expr TIMES expr { Cp_syntax.T_OP_arith ("*",$1,$3) }
    | expr DIVI expr { Cp_syntax.T_OP_arith ("/",$1,$3) }
    | expr LAND expr { Cp_syntax.T_OP_arith ("land",$1,$3) }
    | expr LOR expr { Cp_syntax.T_OP_arith ("lor",$1,$3) }
    | expr LXOR expr { Cp_syntax.T_OP_arith ("lxor",$1,$3) }
    | expr EQ expr { Cp_syntax.T_OP_bool (Cp_syntax.Relational,"=",$1,$3) }
    | expr LT expr { Cp_syntax.T_OP_bool (Cp_syntax.Relational,"<",$1,$3) }
    | expr GT expr { Cp_syntax.T_OP_bool (Cp_syntax.Relational,">",$1,$3) }
    | expr LEQ expr { Cp_syntax.T_OP_bool (Cp_syntax.Relational,"<=",$1,$3) }
    | expr GEQ expr { Cp_syntax.T_OP_bool (Cp_syntax.Relational,">=",$1,$3) }
    | expr NEQ expr { Cp_syntax.T_OP_bool (Cp_syntax.Relational,"<>",$1,$3) }
    | expr BOR expr { Cp_syntax.T_OP_bool (Cp_syntax.Bool,"or",$1,$3) }
    | expr BAND expr { Cp_syntax.T_OP_bool (Cp_syntax.Bool,"and",$1,$3) }
    | expr BXOR expr { Cp_syntax.T_OP_bool (Cp_syntax.Bool,"xor",$1,$3) }
    | expr LSL expr { Cp_syntax.T_OP_arith ("lsl",$1,$3) }
    | expr LSR expr { Cp_syntax.T_OP_arith ("lsr",$1,$3) }
    | expr ASL expr { Cp_syntax.T_OP_arith ("asl",$1,$3) }
    | expr ASR expr { Cp_syntax.T_OP_arith ("asr",$1,$3) }
    | expr EXP expr { Cp_syntax.T_OP_arith ("^",$1,$3) }
    | expr LOG expr { Cp_syntax.T_OP_arith ("~",$1,$3) }
    | expr MAX expr { Cp_syntax.T_OP_arith ("max",$1,$3) }
    | expr MIN expr { Cp_syntax.T_OP_arith ("min",$1,$3) }
    | LPAREN expr RPAREN { $2 }
    | expr TO expr { Cp_syntax.T_interval ($1,$3) }
    | expr DOWNTO expr { Cp_syntax.T_interval ($3,$1) }
    | TO_INT LPAREN expr RPAREN { Cp_syntax.T_typeconv ('i',$3) }
    | TO_BOOL LPAREN expr RPAREN { Cp_syntax.T_typeconv ('b',$3) }
    | TO_LOG LPAREN expr RPAREN { Cp_syntax.T_typeconv ('l',$3) }
    | TO_CHR LPAREN expr RPAREN { Cp_syntax.T_typeconv ('c',$3) }
    | SIZEOF LPAREN expr RPAREN { Cp_syntax.T_sizeof ($3) }
    | expr CONCAT expr { Cp_syntax.T_concat ($1,$3) }
    | expr timeunit { Cp_syntax.T_time ($1,$2) }
    | expr frequnit { Cp_syntax.T_freq ($1,$2) }
    | element { $1 }
;

element :
    | IDENT  { Cp_syntax.T_ident $1 }
    | ID { Cp_syntax.T_id }
    | Z { Cp_syntax.T_z }
    | STRING  { Cp_syntax.T_string $1 }
    | CHAR  { Cp_syntax.T_character $1 }
    | CHAN { Cp_syntax.T_chan }
    | QUEUE { Cp_syntax.T_que }
    | REG { Cp_syntax.T_reg }
    | VAR { Cp_syntax.T_var }
    | SIGNAL { Cp_syntax.T_sig }
    | PROCESS { Cp_syntax.T_proc }
    | UNIT { Cp_syntax.T_unit }
    | LPAREN RPAREN { Cp_syntax.T_ident (Cp_syntax.nilpos,"") }
    | element LBRAK expr RBRAK { Cp_syntax.T_sub ($1,$3) }
    | element LBRAK element_list RBRAK { Cp_syntax.T_sub ($1,Cp_syntax.T_list $3) }
    | element LBRAK expr TO expr RBRAK { Cp_syntax.T_sub ($1,Cp_syntax.T_interval ($3,$5)) }
    | element LBRAK element DOWNTO element RBRAK { Cp_syntax.T_sub ($1,Cp_syntax.T_interval ($5,$3)) }
    | element DOT LBRAK expr RBRAK { Cp_syntax.T_selector ($1,$4) }
    | element DOT LBRAK element_list RBRAK { Cp_syntax.T_selector ($1,Cp_syntax.T_list $4) }
    | element DOT element { Cp_syntax.T_selector ($1,$3) }
    | TO_INT LPAREN expr RPAREN { Cp_syntax.T_typeconv ('i',$3) }
    | TO_BOOL LPAREN expr RPAREN { Cp_syntax.T_typeconv ('b',$3) }
    | TO_LOG LPAREN expr RPAREN { Cp_syntax.T_typeconv ('l',$3) }
    | TO_CHR LPAREN expr RPAREN { Cp_syntax.T_typeconv ('c',$3) }
    | LLIST element_list RLIST { Cp_syntax.T_objlist $2 }
;
element_list :
    | element { [$1] }
    | element ARGSEP element_list { $1 :: $3 }
;

expr_list :
    | expr  { [$1] }
    | OTHERS { [] }
    | expr ARGSEP expr_list { $1 :: $3 }
;

timeunit :
    | NSEC { Cp_syntax.Nsec }
    | USEC { Cp_syntax.Usec }
    | MSEC { Cp_syntax.Msec }
    | SEC { Cp_syntax.Sec }
;

frequnit :
    | GHZ { Cp_syntax.Ghz }
    | MHZ { Cp_syntax.Mhz }
    | KHZ { Cp_syntax.Khz }
    | HZ { Cp_syntax.Hz }
;

