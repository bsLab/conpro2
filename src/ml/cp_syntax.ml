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
**    $INITIAL:     (C) 2006-2009 BSSLAB
**    $CREATED:     1.3.2006
**    $VERSION:     2.26
**
**    $INFO:
**	  Syntax graph 
**    $ENDINFO
**
*)

(*
** CONPRO CP syntax
*)

type bool_kind =
    | Relational
    | Bool
type loop_kind =
    | Loop_while
    | Loop
type timeunit = 
    | Nsec
    | Usec
    | Msec
    | Sec
    | Cycles
type frequnit=
    | Ghz
    | Mhz
    | Khz
    | Hz

(*
** Arithmetic, logical and boolean operators; data path specifiers
*)
type operator = 
    (*
    ** Arithmetic operators
    *)
    | OP_add
    | OP_sub
    | OP_mul
    | OP_div
    | OP_asl
    | OP_asr
    (*
    ** Logical operators
    *)
    | OP_land
    | OP_lor
    | OP_lxor
    | OP_lnot
    | OP_lsl
    | OP_lsr
    (*
    ** Relational operators
    *)
    | OP_eq
    | OP_neq
    | OP_lt
    | OP_gt
    | OP_ge
    | OP_le
    (*
    ** Boolean operators
    *)
    | OP_band
    | OP_bor
    | OP_bxor
    | OP_bnot
    
    | OP_max
    | OP_min
    
    (*
    ** Specifiers
    *)
    | OP_nop
    | OP_alu

type operator_group =
    (*
    ** Groups
    *)
    | OP_arith
    | OP_logic
    | OP_relat
    | OP_bool



type file_pos = {
    f_name : string;
    f_cpos : int;
}

let file_name = ref ""
let nilpos = {f_name="";f_cpos=0}

type syntax = 
    | T_empty
    | T_ident of (file_pos*string)
    | T_id
    | T_z
    | T_unit
    | T_character of (file_pos*char)
    | T_string of (file_pos*string)
    | T_objlist of syntax list


    (*
    ** Open module
    *)
    | T_module of syntax
    (*
    ** Define module
    *)
    | T_module_def of syntax*syntax

    | T_domain of (syntax list*(file_pos * string))

    (*
    ** Module Core: Data object creation
    *)

    | T_OT of (syntax list * syntax list)
    (*
    ** <oname> * <DT> * [ <init> ]
    *)
    | T_OT_const of (syntax list * syntax * syntax)
    (*
    ** <oname> * <ST> * <DT> 
    *)
    | T_OT_sig of (syntax list * syntax * syntax)
    (*
    ** <oname> * <DT> * [ <init> ] * [ <with params> ]
    *)
    | T_OT_reg of (syntax list * syntax * syntax * syntax)
    (*
    ** <oname> * <DT> * <group> * [ <init> ] * [<with params>]
    *)
    | T_OT_var of (syntax list * syntax * syntax * syntax * syntax)
    (*
    ** <oname> * <DT> * [ <with params> ]
    *)
    | T_OT_chan of (syntax list * syntax * syntax)
    (*
    ** <oname> * <DT> * [ <with params> ]
    *)
    | T_OT_que of (syntax list * syntax * syntax)

    (*
    ** <aname> * <kind:' ','o','c','p'> * 
    **           (<OT> <range>) * <DT> [ <block> ] [<with params>]
    *)
    | T_OT_array of (syntax list * char * syntax * syntax *syntax * syntax)

    (*
    ** External components <cname> * <type> * <init>
    *)
    | T_OT_comp of (syntax list * syntax * syntax list)

    (*
    ** <oname> * <type> * <params>
    *)
    | T_OT_object of (syntax list * syntax * syntax)

    | T_concat of (syntax * syntax)

    (*
    ** Block <name> * <optional parameter list>
    *)
    | T_data_block of (syntax * syntax)


    (*
    ** sizeof function
    *)
    | T_sizeof of syntax
    
    (*
    ** Function calls: 
    ** 1. Abstract Object methods
    ** 2. User defined static functions
    **
    ** <name> [<args>] <ret?>
    *)
    | T_Fun of (syntax * (syntax list) * syntax)

    (*
    ** Function definition
    ** <fname> <frets> <fargs> <fbody>
    *)
    | T_Fun_def of (syntax * (syntax list) * (syntax list) * syntax)
    
    (*
    ** Type conversion
    *)

    | T_typeconv of (char * syntax)

    (*
    ** Module Core: Assignment and expressions
    *)

    (*
    ** <lhs> <- <rhs>;
    *)
    | T_assign of (syntax * syntax)
    (*
    ** Signal mapping to registers or other signals
    *)
    | T_map of (syntax * syntax)
    (*
    ** Arithmetic operations
    *)
    | T_OP_arith of (string * syntax * syntax)

    (*
    ** Boolean operations
    *)
    | T_OP_bool of (bool_kind * string * syntax * syntax)

    (*
    ** Module Core: Flow control
    *)
    
    (*
    ** wait for <expr> [with <false> else <true>];
    ** delay|apply <cycles> only apply:[with <false> else <true>];
    ** <expr> <T_timeunit|T_Empty> <expr_f> <expr_t>
    *)

    | T_waitfor of (syntax*syntax*syntax*syntax)
    (*
    ** <val> <timeunit> 
    *)
    | T_time of (syntax*timeunit)
    (*
    ** <val> <frequnit> 
    *)
    | T_freq of (syntax*frequnit)

    (*
    ** <expr> <block1> <block2>
    *)
    | T_branch of (syntax * syntax * syntax)

    (*
    ** <expr> <dir +/-> <limit1> <limit2> <block>
    *)
    | T_forloop of (syntax * char * syntax * syntax * syntax)

    (*
    ** <expr> <block>
    *)
    | T_loop of (loop_kind * syntax * syntax)
    
    (*
    ** match <select_expr> with
    ** | <case1>
    ** | <case2> ...
    **
    ** <expr> <case list>
    *)
    | T_select of (syntax * syntax)
    (*
    ** <expr list> <instr/block>
    *)
    | T_case of (syntax list * syntax) 
    (*
    ** The world inside: the process
    **
    ** process <name>:
    ** begin
    **  ...
    ** end;
    **
    *)
    | T_process of (syntax * syntax)
    | T_connect of syntax
    | T_export of syntax list
    | T_import of syntax list
    | T_include of syntax
    | T_block of (syntax * syntax option)
    | T_bind of syntax
    
    | T_topinstr of syntax
    | T_instr of syntax


    | T_list of (syntax list)
    (*
    **   
    **  Subindex, subrange and index specifier
    **  v[<ind>];
    *)
    | T_sub of (syntax*syntax)
    | T_interval of (syntax*syntax)
    | T_selector of (syntax*syntax)
    
    (*
    ** Array object type
    *)
    | T_reg
    | T_var
    | T_sig
    | T_chan
    | T_que
    | T_proc
    
    (*
    ** Export monitor signals for given data object to main module port.
    ** With debug=true internal object signals like WE are exported, too.
    *)
    | T_monitor of (syntax list * bool)

    (*
    ** Type definition <name> <element list> <optional flag list>
    *)
    | T_typedef of (syntax * syntax * syntax)

    (*
    ** Define a new exception type
    *)
    | T_exception of syntax
    (*
    ** Raise a particular exception
    *)
    | T_raise of syntax
    (*
    ** Catch exceptions
    ** try <instr> with <T_case list in block>
    *)
    | T_try of (syntax * syntax)
    
(* lexbuf.Lexing.lex_curr_pos *)


let rec t_block_params pl =
  match pl with
  | [n,v] -> T_OP_bool (Relational,"=",T_string(nilpos,n),T_string(nilpos,v))
  | _ -> progerr "t_block_params with # > 1"
  
(*
** CONPRO UC MicroCode syntax
*)

type uc_syntax =
  | TU_empty
  | TU_ident of (file_pos*string)
  
(*
************************************************************************
** CONPRO expression syntax used for example
** in resource functions.
************************************************************************
*)
type expr_syntax =
  | TE_empty
  | TE_ident of (file_pos*string)
  | TE_param of (file_pos*string)
  | TE_string of (file_pos*string)
  | TE_expr of (string * expr_syntax * expr_syntax)
  | TE_unit of (expr_syntax * string)  


(*
************************************************************************
** CONPRO external module interface syntax 
************************************************************************
*)
type emi_syntax =
  | TM_empty
  | TM_ident of (file_pos*string)
  (*
  ** b: single binary number 'X'
  ** v: vector "XXX"
  ** o: others => 'X'
  *)
  | TM_value of (char*file_pos*string)
  | TM_string of (file_pos*string)
  (*
  ** a TO|DOWNTO b
  *)
  | TM_range of (char * emi_syntax * emi_syntax)
  (*
  ** obj(range)
  *)
  | TM_sub of (emi_syntax*emi_syntax)
 
  | TM_expr of (string * emi_syntax * emi_syntax)
  | TM_block of (emi_syntax list)
  | TM_list of (emi_syntax list)
  (*
  ** #lhs:<type>
  ** #rhs:<type>
  *)
  | TM_arg of (string * emi_syntax)
  | TM_type of (string * emi_syntax)
  (*
  ** F(arg_list)
  *)
  | TM_func of (emi_syntax * emi_syntax list)
  
  
   
  (*
  ** All variables $...
  *)
  | TM_var of (file_pos*string)
  
  
  (*
  ** VHDL x'f
  *)
  | TM_sel of (emi_syntax*emi_syntax)
  
  (*
  ** foreach <var> [in <var>] begin <statements> end;
  *)
  | TM_foreach of (emi_syntax * emi_syntax * emi_syntax list)
  
  (*
  ** for i = a to b do begin end
  *)
  | TM_for of (emi_syntax * emi_syntax * emi_syntax list)

  
  (*
  ** with $p = nth($P,1) do ...
  *)
  | TM_dowith of (emi_syntax * emi_syntax list)
  
  (*
  ** Paramter defintion with optional attributes
  *)
  | TM_param_def of (emi_syntax * emi_syntax list)
  
  (*
  ** Sections
  *)
  
  (*
  ** Conditional section
  *)
  | TM_conditional of (emi_syntax*emi_syntax)
  
  | TM_version of (file_pos*string)
  | TM_parameter of emi_syntax list
  | TM_methods of emi_syntax list 
  | TM_check of emi_syntax list 
  | TM_signals of emi_syntax list
  | TM_mappings of emi_syntax list
  | TM_interface of emi_syntax list
  (*
  ** <label>: #access begin <cont> end;
  *)
  | TM_access of (emi_syntax*emi_syntax list)
  
  (*
  ** label: #process
  *)
  | TM_process of (emi_syntax * emi_syntax list)
  
  (*
  ** #top
  *)
  | TM_top of (emi_syntax list)
  (*
  ** #data
  *)
  | TM_data of emi_syntax list
  (*
  ** #import
  *)
  | TM_import of emi_syntax list
  
  (*
  ** #set
  *)
  | TM_set of emi_syntax list
  
  (*
  ** name ASSIGN vhdl_expr WHEN vhdl_expr ELSE vhdl_expr
  *)
  | TM_cond_assign of (emi_syntax*emi_syntax*emi_syntax*emi_syntax)
  
  (*
  ** #control
  *)
  | TM_control of emi_syntax list
  
  (*
  ** VHDL
  *)
  (* 
  ** signal 's'/variable 'v' <id>: <dir> <type> 
  *)
  | TM_signal of (char*emi_syntax*emi_syntax*emi_syntax)
  (* 
  ** constant  <id>: <type> := <val> 
  *)
  | TM_constant of (emi_syntax*emi_syntax*emi_syntax)
  (*
  ** type declaration (enumeration 'e',array 'a')
  ** type name is (,...) [array(range)  of ...] 
  *)
  | TM_type_decl of (char*emi_syntax*emi_syntax list*emi_syntax)
  
  (* 
  ** <x> => <y> 
  *)
  | TM_mapping of (emi_syntax*emi_syntax)
  
  
  (*
  ** sequence
  ** begin
  **   if c1 ...
  **   if c2 ...
  **   foreach $p begin if c3 ...
  **   begin  ... end;
  ** end;
  **  => VHDL: if c1 elsif c2 elsif c3.p1 ... else ...
  *) 
  | TM_seq of (emi_syntax list)
  
  (*
  ** A <= B;  // A: signal/variable
  *)
  | TM_assign of (char*emi_syntax*emi_syntax)
  (*
  ** WAIT FOR/UNTIL vhdl_expr
  *)
  | TM_wait of (string*emi_syntax)
  | TM_if of (emi_syntax*emi_syntax list*emi_syntax list)
  | TM_case of (emi_syntax * emi_syntax list)
  | TM_when of (emi_syntax * emi_syntax list)
  
  
let rec tm_name_concat eml =
  let fp = 
    match eml with
    | (TM_ident (fp,_)) :: _ -> fp;
    | (TM_var (fp,_)) :: _ -> fp;
      | _ -> progerr "tm_name_concat"; in
  let rec conc eml = 
    match eml with
    | hd :: tl ->
    begin
      match hd with
      | TM_ident (fp,str) -> str ^ (conc tl);
      | TM_var (fp,str) -> "$" ^ str ^ (conc tl);
      | _ -> progerr "tm_name_concat";
    end;
    | [] -> "" in
  TM_ident (fp,conc eml)

let tm_name_split em =
  match em with
  | TM_ident (pos,str) ->
  begin
    let rec map strl =
      match strl with
      | [str] ->
      begin
        let strlen = String.length str in
        let em' = 
          if strlen > 0 && str.[0] = '$' then
            TM_var (pos,String.sub str 1 (strlen-1))
          else
            TM_ident (pos,str) in
        em'
      end;
        
      | str :: tl ->
      begin
        let strlen = String.length str in
        let em' = 
          if strlen > 0 && str.[0] = '$' then
            TM_var (pos,String.sub str 1 (strlen-1))
          else
            TM_ident (pos,str) in
        TM_expr ("_",em',map tl);
      end;
      | [] -> progerr "tm_name_split:list" in
    map (Str.split (Str.regexp "_") str);
  end
  | _ -> em

(*
************************************************************************
** CONPRO tool description interface syntax 
************************************************************************
*)
type tdi_syntax =
  | TT_empty
  | TT_ident of (file_pos*string)
  | TT_string of (file_pos*string)
  (*
  ** a TO|DOWNTO b
  *)
  | TT_range of (char * tdi_syntax * tdi_syntax)
  (*
  ** obj(range)
  *)
  | TT_sub of (tdi_syntax*tdi_syntax)
 
  | TT_expr of (string * tdi_syntax * tdi_syntax)
  | TT_block of (tdi_syntax list)
  | TT_list of (tdi_syntax list)
  (*
  *)
  | TT_arg of (string * tdi_syntax)
  (*
  ** F(arg_list)
  *)
  | TT_func of (tdi_syntax * tdi_syntax list)
  
  (*
  ** All variables $...
  *)
  | TT_var of (file_pos*string)
  
  (*
  ** Array/Structure selector x.[i]
  *)
  | TT_sel of (tdi_syntax*tdi_syntax)
  
  (*
  ** foreach <var> in <var> begin <statements> end;
  *)
  | TT_foreach of (tdi_syntax * tdi_syntax * tdi_syntax list)
  
  (*
  ** for i = a to b do begin end
  *)
  | TT_for of (tdi_syntax * tdi_syntax * tdi_syntax list)
  
  (*
  ** Sections
  *)
  
  (*
  ** Conditional section
  *)
  | TT_conditional of (tdi_syntax*tdi_syntax)
  
  | TT_version of (file_pos*string)
  | TT_parameter of tdi_syntax list
  | TT_fun of (tdi_syntax * tdi_syntax list) 
  | TT_targets of tdi_syntax list
  | TT_target of (tdi_syntax * tdi_syntax list)
  
  (*
  ** b: single binary number 'X'
  ** v: vector "XXX"
  ** o: others => 'X'
  ** s: string
  *)
  | TT_value of (char*(file_pos*string))
  
  
  (*
  ** A <= B;  
  *)
  | TT_assign of (tdi_syntax*tdi_syntax)


  (*
  ** control statements
  *)
  | TT_if of (tdi_syntax*tdi_syntax list*tdi_syntax list)
  | TT_case of (tdi_syntax * tdi_syntax list)
  | TT_when of (tdi_syntax * tdi_syntax list)
 

(*
************************************************************************
** CONPRO MicroCode interface syntax 
************************************************************************
*)
type uci_syntax =
  | UT_empty
  | UT_string of (file_pos*string)
  | UT_char of (file_pos*char)
  | UT_ident of (file_pos*string)
  | UT_value of (char*file_pos*string)
  | UT_param of (file_pos*string)
  
  (*
  ** Sections
  *)
  | UT_s_parameter of (uci_syntax list)
  | UT_s_modules of (uci_syntax list)
  | UT_s_export of (uci_syntax list)
  | UT_s_import of (uci_syntax list)
  | UT_s_types of (uci_syntax list)
  | UT_s_data of (uci_syntax list)
  | UT_s_temp of (uci_syntax list)
  | UT_s_code of (uci_syntax list)
  
  (*
  ** Parameter definition <name>=<expr>
  *)
  | UT_param_def of (uci_syntax * uci_syntax)
  
  (*
  ** Object definitions: <kind>,<name>,<datatype>,[<params>,<initval>]
  *)
  | UT_obj of (char * uci_syntax * uci_syntax * uci_syntax list * uci_syntax)
  (*
  ** Function definition <name>*<data_type> list
  *)
  | UT_fundef of (uci_syntax * uci_syntax list)
  
  (*
  ** Data types
  *)
  | UT_type of (char * uci_syntax)
  
  | UT_list of (uci_syntax list)

  (*
  ** Operands
  *)
  
  (*
  ** <name>[:parameter]
  *)
  | UT_op of (uci_syntax*uci_syntax list)
  | UT_immed of (uci_syntax*uci_syntax list)
  | UT_temp of (uci_syntax*uci_syntax list)
  | UT_alu of (uci_syntax*uci_syntax list)
  | UT_selector of (uci_syntax list)
  | UT_range of (char * uci_syntax * uci_syntax)
  

  (*
  ** Instructions
  *)
  | UT_label of uci_syntax
  | UT_move of (uci_syntax * uci_syntax * uci_syntax list)
  | UT_expr of (uci_syntax * uci_syntax * operator * uci_syntax * uci_syntax list)
  | UT_fun of (uci_syntax * uci_syntax list)
  | UT_jump of (uci_syntax)
  | UT_falsejump of (uci_syntax * uci_syntax)
  | UT_bind of uci_syntax
  | UT_nop 
   
let char_of_type ps =
  match ps with
  | (pos,"L") -> 'l';
  | (pos,"I") -> 'i';
  | (pos,"N") -> 'n';
  | (pos,"B") -> 'b';
  | (pos,"C") -> 'c';
  | _ -> progerr "char_of_type"
  
(*
** Tools definition file syntax
*)

type synth_tools_syntax =
  | TS_version of (file_pos * string)
  | TS_structs of (synth_tools_syntax list)
  | TS_struct of (synth_tools_syntax list)
  | TS_param of (string * (file_pos * string))
  | TS_paraml of (string * (file_pos * string) list)
  | TS_paraml2 of (string * ((file_pos * string)*(file_pos * string)) list)
  | TS_paraml21 of (string * ((file_pos * string)*(((file_pos * string)*(file_pos * string)) list)) list)
