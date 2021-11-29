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
**    $INITIAL:     (C) 2006-2009 BSSLAB
**    $CREATED:     18.2.2009
**    $VERSION:     2.04
**
**    $INFO:
**
**  CONPRO parser for synth_tools
**
**    $ENDOFINFO
*/

%token <Cp_syntax.file_pos*string> STRING
%token <Cp_syntax.file_pos*string> VALUE
%token <Cp_syntax.file_pos*string> IDENT
%token COMMENT VERSION
%token EOI SEP LBRAK RBRAK LCURL RCURL ADD
%token SYN_TOOL SYN_TOP SYN_VER SYN_VHDL_MAP SYN_SYNTH_SET SYN_TECH_SET SYN_VHDL_LIB
%token EQ 
%left EQ
%start main
%type <Cp_syntax.synth_tools_syntax> main
%%

main:
 | VERSION STRING EOI { Cp_syntax.TS_version $2 }
 | LBRAK structs RBRAK EOI { Cp_syntax.TS_structs $2 }
;



structs:
  | struct1 EOI { [ $1 ] }
  | struct1 EOI structs { $1 :: $3 }
;

struct1:
  | LCURL struct_entries RCURL { Cp_syntax.TS_struct $2 }
;

struct_entries:
  | struct_entry EOI { [ $1 ] }
  | struct_entry EOI struct_entries { $1 :: $3 }
;

struct_entry:
  | SYN_TOOL EQ STRING { Cp_syntax.TS_param ("syn_tool", $3) }
  | SYN_TOP EQ STRING  { Cp_syntax.TS_param("syn_top", $3) }
  | SYN_VER EQ VALUE  { Cp_syntax.TS_param ("syn_ver", $3) }
  | SYN_VHDL_MAP EQ LBRAK list2 RBRAK  { Cp_syntax.TS_paraml2 ("syn_vhdl_map", $4) }
  | SYN_VHDL_MAP EQ LBRAK RBRAK  { Cp_syntax.TS_paraml2 ("syn_vhdl_map", []) }
  | SYN_VHDL_LIB EQ LBRAK list1 RBRAK  { Cp_syntax.TS_paraml ("syn_vhdl_lib", $4) }
  | SYN_VHDL_LIB EQ LBRAK RBRAK  { Cp_syntax.TS_paraml ("syn_vhdl_lib", []) }
  | SYN_SYNTH_SET EQ LBRAK list21 RBRAK  { Cp_syntax.TS_paraml21 ("syn_synth_set", $4) }
  | SYN_SYNTH_SET EQ LBRAK RBRAK  { Cp_syntax.TS_paraml21 ("syn_synth_set", []) }
  | SYN_TECH_SET EQ LBRAK list21 RBRAK  { Cp_syntax.TS_paraml21 ("syn_tech_set", $4) }
  | SYN_TECH_SET EQ LBRAK RBRAK  { Cp_syntax.TS_paraml21 ("syn_tech_set", []) }
;

list21:
  | str SEP LBRAK list2 RBRAK EOI { [$1,$4] }
  | str SEP LBRAK list2 RBRAK EOI list21 { ($1,$4) :: $7 }
;
list2:
  | str SEP str EOI { [ $1,$3] }
  | str SEP str EOI list2 { ($1,$3) :: $5 }
;
list1:
  | str EOI { [ $1] }
  | str EOI list1 { $1 :: $3 }
;

str:
  | STRING { $1 }
  | STRING ADD str { let p1,s1 = $1 in let p2,s2= $3 in (p1,s1^s2) }
;
