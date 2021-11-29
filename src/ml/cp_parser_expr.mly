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
**    $INITIAL:     (C) 2007 BSSLAB
**    $CREATED:     12.12.2007
**    $VERSION:     1.02
**
**    $INFO:
**
**  CONPRO parser for expr_syntax
**
**    $ENDOFINFO
*/

%token <Cp_syntax.file_pos*string> STRING
%token <Cp_syntax.file_pos*string> IDENT
%token <Cp_syntax.file_pos*string> PARAM
%token DATAWIDTH CYCLETIME
%token U_NS U_US
%token EOI SEP LPAREN RPAREN
%token EQ NEQ LT GT LEQ GEQ
%token PLUS MINUS TIMES DIVI
%left PLUS MINUS
%left TIMES DIVI
%left EQ NEQ LT GT LEQ GEQ
%left DOT
%start main
%type <Cp_syntax.expr_syntax> main
%%

main:
    | expr EOI { $1 }
;





expr:
    | expr PLUS expr { Cp_syntax.TE_expr ("+",$1,$3) }
    | MINUS expr { Cp_syntax.TE_expr ("-",Cp_syntax.TE_empty,$2) }
    | expr MINUS expr { Cp_syntax.TE_expr ("-",$1,$3) }
    | expr TIMES expr { Cp_syntax.TE_expr ("*",$1,$3) }
    | expr DIVI expr { Cp_syntax.TE_expr ("/",$1,$3) }
    | expr EQ expr { Cp_syntax.TE_expr ("=",$1,$3) }
    | expr LT expr { Cp_syntax.TE_expr ("<",$1,$3) }
    | expr GT expr { Cp_syntax.TE_expr (">",$1,$3) }
    | expr LEQ expr { Cp_syntax.TE_expr ("<=",$1,$3) }
    | expr GEQ expr { Cp_syntax.TE_expr (">=",$1,$3) }
    | expr NEQ expr { Cp_syntax.TE_expr ("<>",$1,$3) }
    | LPAREN expr RPAREN { $2 }
    | element { $1 }
;

element :
 | element U_NS { Cp_syntax.TE_unit ($1,"1E-9") }   
 | element U_US { Cp_syntax.TE_unit ($1,"1E-6") }   
 | IDENT  { Cp_syntax.TE_ident $1 }
 | PARAM { Cp_syntax.TE_param  $1 }
 | STRING  { Cp_syntax.TE_string $1 }

;
