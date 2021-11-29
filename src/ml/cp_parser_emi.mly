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
**    $INITIAL:     (C) 2006-2010 BSSLAB
**    $CREATED:     7.7.2008
**    $VERSION:     2.28
**
**    $INFO:
**
**  CONPRO parser for the External Module Interface
**
**    $ENDOFINFO
*/

%token <Cp_syntax.file_pos*string> STRING
%token <Cp_syntax.file_pos*string> IDENT
%token <char*Cp_syntax.file_pos*string> VALUE
%token <Cp_syntax.file_pos*string> PARAM
%token S_VERSION S_PARAMETER S_METHODS S_SIGNALS S_INTERFACE S_MAPPING S_ACCESS 
%token S_PROCESS S_DATA S_CONTROL S_IMPORT S_SET S_TOP S_CHECK
%token LHS RHS LRHS VAR 
%token SIGNAL CONSTANT LOGIC NATURAL VARIABLE BOOL SHARED
%token FOREACH FOR DO IN IF THEN ELSE ELSIF BEGIN END SIGNAL WHEN WAIT FOR UNTIL
%token SEQUENCE TYPE CASE IS ARRAY 
%token EOI SEP1 SEP2 LPAREN RPAREN LBRAK RBRAK LCURL RCURL ACC NULL OTHERS FOLLOW
%token EQ NEQ LT GT ASSIGN ASSIGN2 GEQ
%token PLUS MINUS TIMES DIVI EXP2
%token LOGIC INT BOOL CHAR STRINGS VAR SIGNED
%left PLUS MINUS
%left TIMES DIVI EXP2
%token CONC AND OR XOR NOT
%left OR AND NOT
%left EQ NEQ LT GT LEQ GEQ 
%left CONC
%token COMMENT TO DOWNTO IN OUT WITH IS OF
%token NSEC USEC MSEC SEC HZ KHZ MHZ GHZ
%start main
%type <Cp_syntax.emi_syntax> main
%%

main:
    | section EOI { $1 }
;

section:
    | S_VERSION STRING { Cp_syntax.TM_version $2 }
    | S_PARAMETER BEGIN params END { Cp_syntax.TM_parameter $3 } 
    | S_METHODS BEGIN methods END { Cp_syntax.TM_methods $3 } 
    | S_METHODS LPAREN expr RPAREN BEGIN methods END { Cp_syntax.TM_conditional ($3,Cp_syntax.TM_methods $6) } 
    | S_CHECK BEGIN checks END { Cp_syntax.TM_check $3 } 
    | S_CHECK LPAREN expr RPAREN BEGIN checks END { Cp_syntax.TM_conditional ($3,Cp_syntax.TM_check $6) } 
    | S_INTERFACE BEGIN interfaces END { Cp_syntax.TM_interface $3 } 
    | S_INTERFACE LPAREN expr RPAREN BEGIN interfaces END { Cp_syntax.TM_conditional ($3,Cp_syntax.TM_interface $6) } 
    | S_INTERFACE LPAREN expr RPAREN BEGIN NULL EOI END { Cp_syntax.TM_conditional ($3,Cp_syntax.TM_interface []) } 
    | S_SIGNALS BEGIN signals END { Cp_syntax.TM_signals $3 } 
    | S_SIGNALS LPAREN expr RPAREN BEGIN signals END { Cp_syntax.TM_conditional ($3,Cp_syntax.TM_signals $6) } 
    | S_MAPPING BEGIN mappings END { Cp_syntax.TM_mappings $3 } 
    | S_MAPPING LPAREN expr RPAREN BEGIN mappings END { Cp_syntax.TM_conditional ($3,Cp_syntax.TM_mappings $6) } 
    | S_MAPPING LPAREN expr RPAREN BEGIN NULL EOI END { Cp_syntax.TM_conditional ($3,Cp_syntax.TM_mappings []) } 
    | S_TOP BEGIN instructions END { Cp_syntax.TM_top $3 } 
    | S_TOP LPAREN expr RPAREN BEGIN instructions END { Cp_syntax.TM_conditional ($3,Cp_syntax.TM_top $6) } 
    | name SEP1 S_PROCESS BEGIN instructions END { Cp_syntax.TM_process ($1,$5) } 
    | name SEP1 S_PROCESS LPAREN expr RPAREN BEGIN instructions END { Cp_syntax.TM_conditional ($5,Cp_syntax.TM_process ($1,$8)) } 
    | name SEP1 S_ACCESS BEGIN access END { Cp_syntax.TM_access ($1, $5) } 
    | name SEP1 S_ACCESS LPAREN expr RPAREN BEGIN access END { Cp_syntax.TM_conditional ($5, Cp_syntax.TM_access ($1, $8)) } 
;

params:
  | param_def EOI { [$1] }
  | param_def EOI params { $1 :: $3 } 
;

param_def:
  | param { Cp_syntax.TM_param_def ($1,[]) }
  | param WITH attr_list { Cp_syntax.TM_param_def ($1,$3) }
;


param:
  | name { $1 }
  | name ASSIGN expr { Cp_syntax.TM_assign ('s',$1,$3) } 
  | name LBRAK range RBRAK ASSIGN expr { Cp_syntax.TM_assign ('s',Cp_syntax.TM_sub ($1,$3),$6) } 
  | name LBRAK arg_list RBRAK ASSIGN expr { Cp_syntax.TM_assign ('s',Cp_syntax.TM_sub ($1,Cp_syntax.TM_list $3), $6) } 
  | name LBRAK range RBRAK { Cp_syntax.TM_sub ($1,$3)  }
  | name LBRAK arg_list RBRAK { Cp_syntax.TM_sub ($1,Cp_syntax.TM_list $3)  }
  | name LBRAK range SEP1 data_type RBRAK { Cp_syntax.TM_type ($5,Cp_syntax.TM_sub ($1,$3))  }
  | name LBRAK range SEP1 data_type RBRAK ASSIGN expr { Cp_syntax.TM_assign ('s',Cp_syntax.TM_type ($5,Cp_syntax.TM_sub ($1,$3)),$8) } 
  | name LBRAK arg_list SEP1 data_type RBRAK { Cp_syntax.TM_type ($5,Cp_syntax.TM_sub ($1,Cp_syntax.TM_list $3))  }
  | name LBRAK arg_list SEP1 data_type RBRAK ASSIGN expr { Cp_syntax.TM_assign ('s',Cp_syntax.TM_type ($5,Cp_syntax.TM_sub($1,Cp_syntax.TM_list $3)), $8) } 
;

data_type:
  | LOGIC { "logic" }
  | SIGNED { "int" }
  | INT { "natural" }
  | BOOL { "bool" }
  | CHAR { "char" }
  | STRINGS { "string" }
  | VAR { "var" }
;

attr_list:
  | IDENT { [Cp_syntax.TM_ident $1] }
  | IDENT AND attr_list { (Cp_syntax.TM_ident $1) :: $3 }
;
methods:
  | method_decl EOI { [$1] }
  | method_decl EOI methods { $1 :: $3 } 
;

method_decl:
  | func  { $1 }
;

checks:
  | check EOI { [$1] }
  | check EOI checks { $1 :: $3 } 
;


check:
  | expr { $1 }
;

signals:
  | signal_decl EOI { [$1] }
  | signal_decl EOI signals { $1 :: $3 } 
;

sig_dir:
  | IN { Cp_syntax.TM_type ("in",Cp_syntax.TM_empty) }
  | OUT { Cp_syntax.TM_type ("out",Cp_syntax.TM_empty) }
;

signal_decl:
  | VAR IDENT  { Cp_syntax.TM_signal ('s',Cp_syntax.TM_var $2,Cp_syntax.TM_empty,Cp_syntax.TM_empty) }
  | FOREACH VAR IDENT IN foreach DO BEGIN signals END { Cp_syntax.TM_foreach (Cp_syntax.TM_var $3,$5,$8) }
  | CONSTANT name { Cp_syntax.TM_constant ($2,Cp_syntax.TM_empty,Cp_syntax.TM_empty) }
  | CONSTANT name SEP1 arg_type ASSIGN2 expr { Cp_syntax.TM_constant ($2,$4,$6) }
  | SIGNAL name SEP1 arg_type { Cp_syntax.TM_signal ('s',$2,Cp_syntax.TM_empty,$4) }
  | SIGNAL name SEP1 sig_dir arg_type { Cp_syntax.TM_signal ('s',$2,$4,$5) }
  | VARIABLE name SEP1 arg_type { Cp_syntax.TM_signal ('v',$2,Cp_syntax.TM_empty,$4) }
  | SHARED VARIABLE name SEP1 arg_type { Cp_syntax.TM_signal ('V',$3,Cp_syntax.TM_empty,$5) }
  | TYPE name IS LCURL name_list RCURL { Cp_syntax.TM_type_decl ('e',$2,$5,Cp_syntax.TM_empty) }
  | TYPE name IS ARRAY LBRAK range RBRAK OF arg_type { Cp_syntax.TM_type_decl ('a',$2,[$6],$9) }
  | TYPE name  { Cp_syntax.TM_type_decl ('e',$2,[],Cp_syntax.TM_empty) }
;

interfaces:
  | interface_decl EOI { [$1] }
  | interface_decl EOI interfaces { $1 :: $3 } 
;

interface_decl:
  | FOREACH VAR IDENT IN foreach DO BEGIN interfaces END { Cp_syntax.TM_foreach (Cp_syntax.TM_var $3,$5,$8) }
  | SIGNAL name SEP1 sig_dir arg_type { Cp_syntax.TM_signal ('s',$2,$4,$5) }
;

mappings:
  | mapping_decl EOI { [$1] }
  | mapping_decl EOI mappings { $1 :: $3 } 
;

mapping_decl:
  | FOREACH VAR IDENT IN foreach DO BEGIN mappings END { Cp_syntax.TM_foreach (Cp_syntax.TM_var $3,$5,$8) }
  | name FOLLOW name { Cp_syntax.TM_mapping ($1,$3) }
;

access:
  | access_decl EOI { [$1] }
  | access_decl EOI access { $1 :: $3 } 
;

access_decl:
  | S_DATA BEGIN data_exprs END { Cp_syntax.TM_data $3}
  | S_CONTROL BEGIN control_exprs END { Cp_syntax.TM_control $3}
  | S_IMPORT BEGIN methods END { Cp_syntax.TM_import $3}
  | S_SET BEGIN data_exprs END { Cp_syntax.TM_set $3}
;

foreach:
  | VAR IDENT { Cp_syntax.TM_var $2 } 
  | foreach AND foreach { Cp_syntax.TM_expr ("and",$1,$3) }
  | foreach OR foreach { Cp_syntax.TM_expr ("or",$1,$3) }
  | foreach XOR foreach { Cp_syntax.TM_expr ("xor",$1,$3) }
;

data_exprs:
  | data_expr EOI { [$1] }
  | data_expr EOI data_exprs { $1 :: $3 } 
;
control_exprs:
  | control_expr EOI { [$1] }
  | control_expr EOI control_exprs { $1 :: $3 } 
;

data_expr:
 | element ASSIGN expr WHEN expr ELSE expr { Cp_syntax.TM_cond_assign ($1,$3,$5,$7) }
 | element ASSIGN expr { Cp_syntax.TM_assign ('s',$1,$3) } 
 | element ASSIGN2 expr { Cp_syntax.TM_assign ('v',$1,$3) } 
 | NULL { Cp_syntax.TM_empty }
 | func { $1 }
;

ifothers:
  | IF OTHERS THEN block_instr { [Cp_syntax.TM_if (Cp_syntax.TM_empty,$4,[])]}
;
ifseq:
  | IF expr THEN block_instr ifseq { (Cp_syntax.TM_if ($2,$4,[])) :: $5}
  | ELSIF expr THEN block_instr ifseq { (Cp_syntax.TM_if ($2,$4,[])) :: $5 }
  | IF expr THEN block_instr ifothers { (Cp_syntax.TM_if ($2,$4,[])) :: $5}
  | ELSIF expr THEN block_instr ifothers { (Cp_syntax.TM_if ($2,$4,[])) :: $5 }
  | ELSIF expr THEN block_instr { [Cp_syntax.TM_if ($2,$4,[])]}
  | ELSIF expr THEN block_instr ELSE block_instr { [Cp_syntax.TM_if ($2,$4,$6)]}
  | ELSE IF expr THEN block_instr ifseq { (Cp_syntax.TM_if ($3,$5,[])) :: $6 }
  | ELSE IF expr THEN block_instr { [Cp_syntax.TM_if ($3,$5,[])]}
  | ELSE IF expr THEN block_instr ELSE block_instr { [Cp_syntax.TM_if ($3,$5,$7)]}
;

control_expr:
  | WAIT FOR expr { Cp_syntax.TM_wait ("for",$3) }
  | WAIT UNTIL expr { Cp_syntax.TM_wait ("until",$3) }
  | IF expr THEN block_instr { Cp_syntax.TM_if ($2,$4,[])}
  | IF expr THEN block_instr ELSE block_instr { Cp_syntax.TM_if ($2,$4,$6)}
  | IF OTHERS THEN block_instr { Cp_syntax.TM_if (Cp_syntax.TM_empty,$4,[])}
  | ifseq { Cp_syntax.TM_seq $1 } 
  | CASE expr IS BEGIN when_list END { Cp_syntax.TM_case ($2,$5) }
  | FOR expr EQ range DO block_instr { Cp_syntax.TM_for ($2,$4,$6) }
  | WITH expr DO block_instr { Cp_syntax.TM_dowith ($2,$4) }
  | NULL { Cp_syntax.TM_empty }
;

block_instr:
  | instruction { [$1] }
  | BEGIN instructions END { $2 }
;

when_list:
  | when_expr { [$1] }
  | when_expr when_list { $1 :: $2 }
;
when_expr:
  | WHEN OTHERS FOLLOW block_instr EOI{ Cp_syntax.TM_when (  Cp_syntax.TM_value ('O',Cp_syntax.nilpos,"others") ,$4) } 
  | WHEN expr FOLLOW block_instr EOI{ Cp_syntax.TM_when ($2,$4) } 
;
name:
  | IDENT { Cp_syntax.TM_ident $1 }
  | VAR IDENT { Cp_syntax.TM_var $2 } 
  | var_expr { Cp_syntax.tm_name_split $1 }
;

var_expr:
  | IDENT { Cp_syntax.TM_ident $1 } 
  | VAR IDENT { Cp_syntax.TM_var $2 } 
  | VAR IDENT var_expr { Cp_syntax.tm_name_concat [Cp_syntax.TM_var $2;$3] }
  | IDENT var_expr { Cp_syntax.tm_name_concat [Cp_syntax.TM_ident $1;$2] }
;

name_list:
  | name { [$1] }
  | name SEP2 name_list { $1 :: $3 }
;
func:
  | name LPAREN arg_list RPAREN { Cp_syntax.TM_func ($1,$3) }
  | name LPAREN RPAREN { Cp_syntax.TM_func ($1,[]) }
;



arg_list:
  | arg { [$1] }
  | arg SEP2 arg_list { $1 :: $3 }
;

arg:
  | expr { $1 }
  | OR { Cp_syntax.TM_string (Cp_syntax.nilpos,"or") }
  | LHS SEP1 arg_type { Cp_syntax.TM_arg ("lhs",$3) }
  | RHS SEP1 arg_type { Cp_syntax.TM_arg ("rhs",$3) }
  | LRHS SEP1 arg_type { Cp_syntax.TM_arg ("lrhs",$3) }
  | LHS { Cp_syntax.TM_arg ("lhs",Cp_syntax.TM_empty) }
  | RHS { Cp_syntax.TM_arg ("rhs",Cp_syntax.TM_empty) }
  | LRHS { Cp_syntax.TM_arg ("lrhs",Cp_syntax.TM_empty) }
;

arg_type:
  | LOGIC  { Cp_syntax.TM_type ("logic", Cp_syntax.TM_empty) }
  | BOOL { Cp_syntax.TM_type ("bool", Cp_syntax.TM_empty) }
  | INT  LBRAK expr RBRAK { Cp_syntax.TM_type ("int", $3) }
  | SIGNED  LBRAK expr RBRAK { Cp_syntax.TM_type ("int", $3) }
  | LOGIC  LBRAK expr RBRAK { Cp_syntax.TM_type ("logic", $3) }
  | LOGIC  LBRAK range RBRAK { Cp_syntax.TM_type ("logic", $3) }
  | NATURAL  { Cp_syntax.TM_type ("natural", Cp_syntax.TM_empty) }
  | STRINGS  { Cp_syntax.TM_type ("string", Cp_syntax.TM_empty) }
  | name { Cp_syntax.TM_type ("type",$1) }  
;    


instructions:
  | instruction EOI { [$1] }
  | instruction EOI instructions { $1 :: $3 } 
;

instruction:
  | VARIABLE name SEP1 arg_type { Cp_syntax.TM_signal ('v',$2,Cp_syntax.TM_empty,$4) }
  | data_expr { $1 }
  | control_expr { $1 }
  | SEQUENCE BEGIN instructions END { Cp_syntax.TM_seq $3 }
  | FOREACH VAR IDENT IN foreach DO BEGIN instructions END { Cp_syntax.TM_foreach (Cp_syntax.TM_var $3,$5,$8) }
  | FOREACH VAR IDENT IN foreach DO instruction { Cp_syntax.TM_foreach (Cp_syntax.TM_var $3,$5,[$7]) }
;



expr:
    | expr PLUS expr { Cp_syntax.TM_expr ("+",$1,$3) }
    | expr EXP2 expr { Cp_syntax.TM_expr ("^",$1,$3) }
    | MINUS expr { Cp_syntax.TM_expr ("-",Cp_syntax.TM_empty,$2) }
    | NOT expr { Cp_syntax.TM_expr ("not",Cp_syntax.TM_empty,$2) }
    | expr MINUS expr { Cp_syntax.TM_expr ("-",$1,$3) }
    | expr TIMES expr { Cp_syntax.TM_expr ("*",$1,$3) }
    | expr DIVI expr { Cp_syntax.TM_expr ("/",$1,$3) }
    | expr EQ expr { Cp_syntax.TM_expr ("=",$1,$3) }
    | expr LT expr { Cp_syntax.TM_expr ("<",$1,$3) }
    | expr GT expr { Cp_syntax.TM_expr (">",$1,$3) }
    | expr ASSIGN expr { Cp_syntax.TM_expr ("<=",$1,$3) }
    | expr GEQ expr { Cp_syntax.TM_expr (">=",$1,$3) }
    | expr NEQ expr { Cp_syntax.TM_expr ("/=",$1,$3) }
    | expr AND expr { Cp_syntax.TM_expr ("and",$1,$3) }
    | expr OR expr { Cp_syntax.TM_expr ("or",$1,$3) }
    | expr XOR expr { Cp_syntax.TM_expr ("xor",$1,$3) }
    | expr CONC expr { Cp_syntax.TM_expr ("&",$1,$3) }
    | LPAREN expr RPAREN { $2 }
    | element { $1 }
    | func { $1 }
;


range:
  | expr DOWNTO expr { Cp_syntax.TM_range ('-',$1,$3) }
  | expr TO expr { Cp_syntax.TM_range ('+',$1,$3) }
  | expr { Cp_syntax.TM_range ('=',$1,$1) }
;

element :
 | STRING  { Cp_syntax.TM_string ($1) }
 | VALUE { Cp_syntax.TM_value ($1) }
 | VALUE timeunit { let k,p,v=$1 in Cp_syntax.TM_value (k,p,Int64.to_string (Int64.mul (Int64.of_string v) $2)) }
 | VALUE frequnit { let k,p,v=$1 in Cp_syntax.TM_value (k,p,Int64.to_string (Int64.mul (Int64.of_string v) $2)) }
 | OTHERS FOLLOW VALUE { Cp_syntax.TM_value ('O',(let kind,pos,str = $3 in pos),(let kind,pos,str=$3 in str)) } 
 | name { $1 }
 | name LBRAK range RBRAK { Cp_syntax.TM_sub ($1,$3)  }
 | name ACC name { Cp_syntax.TM_sel ($1,$3) }
;

timeunit :
    | NSEC { Int64.of_string "1" }
    | USEC { Int64.of_string "1000" }
    | MSEC { Int64.of_string "1000000" }
    | SEC { Int64.of_string "1000000000" }
;

frequnit :
    | HZ { Int64.of_string "1" }
    | KHZ { Int64.of_string "1000" }
    | MHZ { Int64.of_string "1000000" }
    | GHZ { Int64.of_string "1000000000" }
;
