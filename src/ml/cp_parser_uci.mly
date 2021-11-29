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
**    $CREATED:     7.11.2008
**    $VERSION:     2.08
**
**    $INFO:
**
**  CONPRO parser for the MicroCode Interface
**
**    $ENDOFINFO
*/

%token <Cp_syntax.file_pos*string> STRING
%token <Cp_syntax.file_pos*char> CHAR
%token <Cp_syntax.file_pos*string> PARAM
%token <Cp_syntax.file_pos*string> TYPE_SPEC
%token <Cp_syntax.file_pos*string> IDENT
%token <char*Cp_syntax.file_pos*string> VALUE
%token <Cp_syntax.file_pos*string> PARAM
%token S_PARAMETER S_MODULES S_EXPORT S_IMPORT S_TYPES S_DATA S_CODE S_TEMP
%token VAR REG SIG VAR OBJ QUE CHA BLK FUNC CON VAL CELLS ARR MODULE METHOD TYPE
%token T_LOGIC T_INT T_CHAR T_BOOL T_NAT
%token EQ NEQ LT GT LEQ GEQ 
%token PLUS MINUS TIMES DIVI
%left PLUS MINUS
%left TIMES DIVI
%token AND OR XOR NOT
%left OR AND
%token LBRAK RBRAK LPAREN RPAREN LCURL RCURL SEP1 SEP2 LCURL RCURL
%token COMMENT TO DOWNTO IN WITH OF ASS DOT PARENT
%token BEGIN END IMMED ALU TEMP
%token LABEL MOVE EXPR FUN BIND JUMP FALSEJUMP NOP
%start main
%type <Cp_syntax.uci_syntax> main
%%

main:
    | section { $1 }
;

section:
    | S_PARAMETER SEP1 BEGIN env_params END { Cp_syntax.UT_s_parameter $4 } 
    | S_MODULES SEP1 BEGIN uc_objects END { Cp_syntax.UT_s_modules $4 } 
    | S_EXPORT SEP1 BEGIN uc_objects END { Cp_syntax.UT_s_export $4 } 
    | S_IMPORT SEP1 BEGIN uc_objects END { Cp_syntax.UT_s_import $4 } 
    | S_TYPES SEP1 BEGIN uc_objects END { Cp_syntax.UT_s_types $4 } 
    | S_DATA SEP1 BEGIN uc_objects END { Cp_syntax.UT_s_data $4 } 
    | S_TEMP SEP1 BEGIN uc_objects END { Cp_syntax.UT_s_temp $4 } 
    | S_CODE SEP1 BEGIN instructions END { Cp_syntax.UT_s_code $4 } 
;

uc_objects:
  | uc_object { [$1] }
  | uc_object uc_objects { $1 :: $2 } 
;

env_params:
  | env_param  { [$1] }
  | env_param env_params { $1 :: $2 } 
;

env_param:
  | env_param_name EQ env_param_value { Cp_syntax.UT_param_def ($1,$3) }
;

env_param_name:
  | VAR IDENT { Cp_syntax.UT_ident $2 }
;

env_param_value:
  | VALUE { Cp_syntax.UT_value $1 }
;


uc_object:
  | REG name SEP1 data_type { Cp_syntax.UT_obj ('r',$2,$4,[],Cp_syntax.UT_empty) }
  | REG name SEP1 data_type WITH params { Cp_syntax.UT_obj ('r',$2,$4,$6,Cp_syntax.UT_empty) }
  | SIG name SEP1 data_type { Cp_syntax.UT_obj ('s',$2,$4,[],Cp_syntax.UT_empty) }
  | VAR name SEP1 data_type { Cp_syntax.UT_obj ('v',$2,$4,[],Cp_syntax.UT_empty) }
  | VAR name SEP1 data_type IN name { Cp_syntax.UT_obj ('v',$2,$4,[$6],Cp_syntax.UT_empty) }
  | VAR name SEP1 data_type IN name DOT LBRAK VALUE RBRAK { Cp_syntax.UT_obj ('v',$2,$4,[$6],Cp_syntax.UT_value $9) }
  | VAR name SEP1 data_type IN name WITH params { Cp_syntax.UT_obj ('v',$2,$4,[$6]@$8,Cp_syntax.UT_empty) }
  | VAR name SEP1 data_type IN name DOT LBRAK VALUE RBRAK WITH params  { Cp_syntax.UT_obj ('v',$2,$4,[$6]@$12,Cp_syntax.UT_value $9) }
  | OBJ name SEP1 data_type { Cp_syntax.UT_obj ('o',$2,$4,[],Cp_syntax.UT_empty) }
  | OBJ name SEP1 data_type WITH params { Cp_syntax.UT_obj ('o',$2,$4,$6,Cp_syntax.UT_empty) }
  | QUE name SEP1 data_type { Cp_syntax.UT_obj ('q',$2,$4,[],Cp_syntax.UT_empty) }
  | QUE name SEP1 data_type WITH params { Cp_syntax.UT_obj ('q',$2,$4,$6,Cp_syntax.UT_empty) }
  | CHA name SEP1 data_type { Cp_syntax.UT_obj ('c',$2,$4,[],Cp_syntax.UT_empty) }
  | CHA name SEP1 data_type WITH params { Cp_syntax.UT_obj ('c',$2,$4,$6,Cp_syntax.UT_empty) }
  | FUNC name LPAREN data_type_list RPAREN { Cp_syntax.UT_fundef ($2,$4) }
  | BLK name SEP1 data_type OF data_type { Cp_syntax.UT_obj ('b',$2,$4,[$6],Cp_syntax.UT_empty) }
  | ARR name SEP1 data_type OF data_type { Cp_syntax.UT_obj ('b',$2,$4,[$6],Cp_syntax.UT_empty) }
  | CON name SEP1 data_type ASS operand { Cp_syntax.UT_obj ('1',$2,$4,[],$6) }
  | VAL name SEP1 operand { Cp_syntax.UT_obj ('2',$2,Cp_syntax.UT_empty,[],$4) }
  | MODULE name SEP1 data_type { Cp_syntax.UT_obj ('M',$2,$4,[],Cp_syntax.UT_empty) }
  | TYPE name SEP1 LCURL symbol_list RCURL { Cp_syntax.UT_obj ('Y',$2,Cp_syntax.UT_empty,$5,Cp_syntax.UT_empty) }
  | TYPE name SEP1 LCURL struct_list RCURL { Cp_syntax.UT_obj ('S',$2,Cp_syntax.UT_empty,$5,Cp_syntax.UT_empty) }
  | TYPE selector SEP1 LCURL method_list RCURL { Cp_syntax.UT_obj ('M',$2,Cp_syntax.UT_empty,$5,Cp_syntax.UT_empty) }
;

symbol_list:
  | symbol_el { [$1] }
  | symbol_el symbol_list { $1 :: $2 }
;

struct_list:
  | struct_el { [$1] }
  | struct_el struct_list { $1 :: $2 }
;

method_list:
  | method_el { [$1] }
  | method_el method_list { $1 :: $2 }
;

symbol_el:
  | name { Cp_syntax.UT_obj ('E',$1,Cp_syntax.UT_empty,[],Cp_syntax.UT_empty) }
;

struct_el:
  | name SEP1 data_type { Cp_syntax.UT_obj ('E',$1,$3,[],Cp_syntax.UT_empty) }
;

method_el:
  | METHOD name LPAREN data_type_list RPAREN { Cp_syntax.UT_obj ('E',$2,Cp_syntax.UT_empty,$4,Cp_syntax.UT_empty) }
  | METHOD name LPAREN  RPAREN { Cp_syntax.UT_obj ('E',$2,Cp_syntax.UT_empty,[],Cp_syntax.UT_empty) }
;

data_type_list:
  | data_type { [$1] }
  | data_type SEP2 data_type_list { $1 :: $3 }
;

data_type:
  | TYPE_SPEC { Cp_syntax.UT_type (Cp_syntax.char_of_type $1,Cp_syntax.UT_empty) }
  | TYPE_SPEC LBRAK VALUE RBRAK { Cp_syntax.UT_type (Cp_syntax.char_of_type $1,Cp_syntax.UT_value $3) }
  | REG LBRAK diml RBRAK { Cp_syntax.UT_type ('R',Cp_syntax.UT_list $3) }
  | VAR LBRAK diml RBRAK { Cp_syntax.UT_type ('V',Cp_syntax.UT_list $3) }
  | SIG LBRAK diml RBRAK { Cp_syntax.UT_type ('S',Cp_syntax.UT_list $3) }
  | CELLS LBRAK VALUE RBRAK { Cp_syntax.UT_type ('C',Cp_syntax.UT_value $3) }
  | QUE LBRAK VALUE RBRAK { Cp_syntax.UT_type ('Q',Cp_syntax.UT_value $3) }
  | CHA LBRAK VALUE RBRAK { Cp_syntax.UT_type ('C',Cp_syntax.UT_value $3) }
  | PARAM { Cp_syntax.UT_type ('P',Cp_syntax.UT_param $1) }
  | IDENT { Cp_syntax.UT_type ('O',Cp_syntax.UT_ident $1) }
  | IDENT DOT IDENT { Cp_syntax.UT_type ('O',Cp_syntax.UT_list [Cp_syntax.UT_ident $1;Cp_syntax.UT_ident $3]) }
  | data_type SEP1 PARAM { Cp_syntax.UT_param_def ($1,Cp_syntax.UT_param $3) }
;

diml:
  | dim { [$1] }
  | dim SEP2 diml { $1 :: $3 }
;

dim:
  | VALUE { Cp_syntax.UT_value $1 }
;

params:
  | param { [$1] }
  | param AND params { $1 :: $3 } 
;
param:
  | param_name EQ param_value { Cp_syntax.UT_param_def ($1,$3) }
;
param_name:
  | PARAM { Cp_syntax.UT_param $1 }
  | VAR IDENT { Cp_syntax.UT_param $2 }
;

param_value:
  | IDENT { Cp_syntax.UT_ident $1 }
  | VALUE { Cp_syntax.UT_value $1 }
  | STRING { Cp_syntax.UT_string $1 }
  | CHAR { Cp_syntax.UT_char $1 }
  | TYPE_SPEC { Cp_syntax.UT_type (Cp_syntax.char_of_type $1,Cp_syntax.UT_empty) }
  | TYPE_SPEC LBRAK VALUE RBRAK { Cp_syntax.UT_type (Cp_syntax.char_of_type $1,Cp_syntax.UT_value $3) }
  | data_type { $1 }
;

operand_list:
  | operand { [$1] }
  | operand SEP2 operand_list { $1 :: $3 }
;

operand:
  | VALUE { Cp_syntax.UT_op (Cp_syntax.UT_value $1,[]) }
  | VALUE SEP1 params { Cp_syntax.UT_op (Cp_syntax.UT_value $1,$3) }
  | IDENT { Cp_syntax.UT_op (Cp_syntax.UT_ident $1,[]) }
  | IDENT SEP1 params { Cp_syntax.UT_op (Cp_syntax.UT_ident $1,$3) }
  | VAR IMMED DOT LBRAK VALUE RBRAK { Cp_syntax.UT_immed (Cp_syntax.UT_value $5,[]) }
  | VAR TEMP DOT LBRAK name RBRAK { Cp_syntax.UT_temp ($5,[]) }
  | VAR TEMP DOT LBRAK selector RBRAK { Cp_syntax.UT_temp ($5,[]) }
  | VAR IMMED DOT LBRAK VALUE RBRAK SEP1 params { Cp_syntax.UT_immed (Cp_syntax.UT_value $5,$8) }
  | VAR TEMP DOT LBRAK name RBRAK SEP1 params { Cp_syntax.UT_temp ($5,$8) }
  | VAR TEMP DOT LBRAK selector RBRAK SEP1 params { Cp_syntax.UT_temp ($5,$8) }
  | VAR ALU DOT LBRAK VALUE RBRAK { Cp_syntax.UT_alu (Cp_syntax.UT_value $5,[]) }
  | VAR ALU DOT LBRAK VALUE RBRAK SEP1 params { Cp_syntax.UT_alu (Cp_syntax.UT_value $5,$8) }
  | selector { Cp_syntax.UT_op ($1,[]) }
  | selector SEP1 params { Cp_syntax.UT_op ($1,$3) }
;

range:
  | VALUE TO VALUE { Cp_syntax.UT_range ('+',Cp_syntax.UT_value $1,Cp_syntax.UT_value $3) }
  | VALUE DOWNTO VALUE { Cp_syntax.UT_range ('-',Cp_syntax.UT_value $1,Cp_syntax.UT_value $3) }
;
value_list:
  | range { [$1] }
  | operand { [$1] }
  | VALUE { [Cp_syntax.UT_value $1] }
  | VALUE SEP2 value_list { (Cp_syntax.UT_value $1) :: $3 } 
  | operand SEP2 value_list { $1 :: $3 } 
;

selector:
  selector_list { Cp_syntax.UT_selector $1 }
;

selector_type:
  | name { $1 }
  | LBRAK value_list RBRAK { Cp_syntax.UT_list $2 }
;

selector_list:
  | selector_type { [$1] }
  | selector_type DOT selector_list { $1 :: $3 }
;


instructions:
  | instruction  { [$1] }
  | instruction instructions { $1 :: $2 }
;

instruction:
  | NOP { Cp_syntax.UT_nop }
  | label SEP1 { Cp_syntax.UT_label $1 }
  | BIND LPAREN VALUE RPAREN  { Cp_syntax.UT_bind (Cp_syntax.UT_value $3) }
  | JUMP LPAREN label RPAREN { Cp_syntax.UT_jump $3  }
  | FALSEJUMP LPAREN operand SEP2 label RPAREN { Cp_syntax.UT_falsejump ($3,$5) }
  | MOVE LPAREN operand SEP2 operand RPAREN { Cp_syntax.UT_move ($3,$5,[]) }
  | MOVE LPAREN operand SEP2 operand RPAREN WITH params { Cp_syntax.UT_move ($3,$5,$8) }
  | EXPR LPAREN operand SEP2 operand SEP2 operation SEP2 operand RPAREN { Cp_syntax.UT_expr ($3,$5,$7,$9,[]) }
  | EXPR LPAREN operand SEP2 operand SEP2 operation SEP2 operand RPAREN WITH params { Cp_syntax.UT_expr ($3,$5,$7,$9,$12) }
  | FUN selector LPAREN operand_list RPAREN { Cp_syntax.UT_fun ($2,$4) }
;

name:
  | IDENT { Cp_syntax.UT_ident $1 }
;

label:
  | IDENT { Cp_syntax.UT_ident $1 }
;

operation:
  | PLUS { Cp_syntax.OP_add }
  | MINUS { Cp_syntax.OP_sub }
  | TIMES { Cp_syntax.OP_mul }
  | DIVI { Cp_syntax.OP_div }
  | AND { Cp_syntax.OP_land }
  | OR { Cp_syntax.OP_lor }
  | XOR { Cp_syntax.OP_lxor }
  | NOT { Cp_syntax.OP_lnot }
  | EQ { Cp_syntax.OP_eq }
  | NEQ { Cp_syntax.OP_neq }
  | LT { Cp_syntax.OP_lt }
  | GT { Cp_syntax.OP_gt }
  | LEQ { Cp_syntax.OP_le }
  | GEQ { Cp_syntax.OP_ge }
;
