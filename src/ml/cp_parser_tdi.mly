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
**    $INITIAL:     (C) 2008 BSSLAB
**    $CREATED:     17.10.2008
**    $VERSION:     2.05
**
**    $INFO:
**
**  CONPRO parser for the Tool Description Interface
**
**    $ENDOFINFO
*/

%token <Cp_syntax.file_pos*string> STRING
%token <Cp_syntax.file_pos*string> IDENT
%token <char*Cp_syntax.file_pos*string> VALUE
%token <Cp_syntax.file_pos*string> PARAM
%token S_VERSION S_PARAMETER S_FUN S_TARGETS S_TARGET 
%token FOREACH FOR DO WHEN IF THEN ELSE BEGIN END MATCH WITH WHEN IN
%token EOI VAR SEP1 SEP2 DOT LPAREN RPAREN LBRAK RBRAK LCURL RCURL FOLLOW
%token EQ NEQ LT GT ASSIGN GEQ
%token PLUS MINUS TIMES DIVI
%left PLUS MINUS
%left TIMES DIVI
%token AND OR XOR
%left OR AND
%left EQ NEQ LT GT LEQ GEQ 
%token COMMENT TO DOWNTO IN 
%start main
%type <Cp_syntax.tdi_syntax> main
%%

main:
    | section EOI { $1 }
;

section:
    | S_VERSION STRING { Cp_syntax.TT_version $2 }
    | S_PARAMETER BEGIN params END { Cp_syntax.TT_parameter $3 } 
    | S_TARGETS BEGIN targets END { Cp_syntax.TT_targets $3 } 
    | name SEP1 S_FUN BEGIN instructions END { Cp_syntax.TT_fun ($1,$5) } 
    | name SEP1 S_FUN LPAREN expr RPAREN BEGIN instructions END { Cp_syntax.TT_conditional ($5,Cp_syntax.TT_fun ($1,$8)) } 
    | S_TARGETS BEGIN targets END { Cp_syntax.TT_targets $3 } 
    | S_TARGETS LPAREN expr RPAREN BEGIN targets END { Cp_syntax.TT_conditional ($3,Cp_syntax.TT_targets $6) } 
;

params:
  | instruction EOI { [$1] }
  | instruction EOI params { $1 :: $3 } 
;
targets:
  | target EOI { [$1] }
  | target EOI targets { $1 :: $3 } 
;

target:
    | name SEP1 S_TARGET BEGIN instructions END { Cp_syntax.TT_target ($1, $5) } 
    | name SEP1 S_TARGET LPAREN expr RPAREN BEGIN instructions END { Cp_syntax.TT_conditional ($5,Cp_syntax.TT_target ($1,$8)) } 
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
 | element ASSIGN expr { Cp_syntax.TT_assign ($1,$3) } 
;



control_expr:
  | IF expr THEN block_instr { Cp_syntax.TT_if ($2,$4,[])}
  | IF expr THEN block_instr ELSE block_instr { Cp_syntax.TT_if ($2,$4,$6)}
  | MATCH expr WITH BEGIN when_list END { Cp_syntax.TT_case ($2,$5) }
  | FOR expr EQ range DO block_instr { Cp_syntax.TT_for ($2,$4,$6) }
  | FOREACH VAR IDENT IN VAR IDENT DO block_instr { Cp_syntax.TT_foreach (Cp_syntax.TT_var $3,Cp_syntax.TT_var $6,$8) }
  | FOREACH VAR IDENT IN name LPAREN VAR IDENT RPAREN DO block_instr { Cp_syntax.TT_foreach (Cp_syntax.TT_var $3,
                                                                                             Cp_syntax.TT_func($5,[Cp_syntax.TT_var $8]),
                                                                                             $11) }
  | func { $1 }
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
  | WHEN expr SEP1 instructions  { Cp_syntax.TT_when ($2,$4) } 
;

var_name:
  | VAR IDENT { Cp_syntax.TT_var $2 } 
;

name_list:
  | name { [$1] }
  | name SEP2 name_list { $1 :: $3 }
;
name:
  | IDENT { Cp_syntax.TT_ident $1 }
  | VAR IDENT { Cp_syntax.TT_var $2 } 
;


func:
  | name LPAREN arg_list RPAREN { Cp_syntax.TT_func ($1,$3) }
  | name LPAREN RPAREN { Cp_syntax.TT_func ($1,[]) }
;



arg_list:
  | arg { [$1] }
  | arg SEP2 arg_list { $1 :: $3 }
;

arg:
  | expr { $1 }
;    


instructions:
  | instruction EOI { [$1] }
  | instruction EOI instructions { $1 :: $3 } 
;

instruction:
  | data_expr { $1 }
  | control_expr { $1 }
  | FOREACH VAR IDENT IN VAR IDENT DO block_instr { Cp_syntax.TT_foreach (Cp_syntax.TT_var $3,Cp_syntax.TT_var $6,$8) }
;



expr:
    | expr PLUS expr { Cp_syntax.TT_expr ("+",$1,$3) }
    | MINUS expr { Cp_syntax.TT_expr ("-",Cp_syntax.TT_empty,$2) }
    | expr MINUS expr { Cp_syntax.TT_expr ("-",$1,$3) }
    | expr TIMES expr { Cp_syntax.TT_expr ("*",$1,$3) }
    | expr DIVI expr { Cp_syntax.TT_expr ("/",$1,$3) }
    | expr EQ expr { Cp_syntax.TT_expr ("=",$1,$3) }
    | expr LT expr { Cp_syntax.TT_expr ("<",$1,$3) }
    | expr GT expr { Cp_syntax.TT_expr (">",$1,$3) }
    | expr ASSIGN expr { Cp_syntax.TT_expr ("<=",$1,$3) }
    | expr GEQ expr { Cp_syntax.TT_expr (">=",$1,$3) }
    | expr NEQ expr { Cp_syntax.TT_expr ("<>",$1,$3) }
    | expr AND expr { Cp_syntax.TT_expr ("and",$1,$3) }
    | expr OR expr { Cp_syntax.TT_expr ("or",$1,$3) }
    | expr XOR expr { Cp_syntax.TT_expr ("xor",$1,$3) }
    | LPAREN expr RPAREN { $2 }
    | element { $1 }
    | func { $1 }
;
range:
  | expr DOWNTO expr { Cp_syntax.TT_range ('-',$1,$3) }
  | expr TO expr { Cp_syntax.TT_range ('+',$1,$3) }
  | expr { Cp_syntax.TT_range ('=',$1,$1) }
;


element :
 | STRING  { Cp_syntax.TT_value ('s',$1) }
 | VALUE { let t,f,s = $1 in  Cp_syntax.TT_value (t,(f,s))}
 | name { $1 }
 | name DOT LBRAK range RBRAK { Cp_syntax.TT_sub ($1,$4)  }
 | name DOT name { Cp_syntax.TT_sel ($1,$3) }
;
