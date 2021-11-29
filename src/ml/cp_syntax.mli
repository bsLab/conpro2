type bool_kind = Relational | Bool
and loop_kind = Loop_while | Loop
and timeunit = Nsec | Usec | Msec | Sec | Cycles
and frequnit = Ghz | Mhz | Khz | Hz
and operator =
    OP_add
  | OP_sub
  | OP_mul
  | OP_div
  | OP_asl
  | OP_asr
  | OP_land
  | OP_lor
  | OP_lxor
  | OP_lnot
  | OP_lsl
  | OP_lsr
  | OP_eq
  | OP_neq
  | OP_lt
  | OP_gt
  | OP_ge
  | OP_le
  | OP_band
  | OP_bor
  | OP_bxor
  | OP_bnot
  | OP_max
  | OP_min
  | OP_nop
  | OP_alu
and operator_group = OP_arith | OP_logic | OP_relat | OP_bool
and file_pos = { f_name : string; f_cpos : int; } 
val file_name : string ref
val nilpos : file_pos
type syntax =
    T_empty
  | T_ident of (file_pos * string)
  | T_id
  | T_z
  | T_unit
  | T_character of (file_pos * char)
  | T_string of (file_pos * string)
  | T_objlist of syntax list
  | T_module of syntax
  | T_module_def of syntax * syntax
  | T_domain of (syntax list * (file_pos * string))
  | T_OT of (syntax list * syntax list)
  | T_OT_const of (syntax list * syntax * syntax)
  | T_OT_sig of (syntax list * syntax * syntax)
  | T_OT_reg of (syntax list * syntax * syntax * syntax)
  | T_OT_var of (syntax list * syntax * syntax * syntax * syntax)
  | T_OT_chan of (syntax list * syntax * syntax)
  | T_OT_que of (syntax list * syntax * syntax)
  | T_OT_array of (syntax list * char * syntax * syntax * syntax * syntax)
  | T_OT_comp of (syntax list * syntax * syntax list)
  | T_OT_object of (syntax list * syntax * syntax)
  | T_concat of (syntax * syntax)
  | T_data_block of (syntax * syntax)
  | T_sizeof of syntax
  | T_Fun of (syntax * syntax list * syntax)
  | T_Fun_def of (syntax * syntax list * syntax list * syntax)
  | T_typeconv of (char * syntax)
  | T_assign of (syntax * syntax)
  | T_map of (syntax * syntax)
  | T_OP_arith of (string * syntax * syntax)
  | T_OP_bool of (bool_kind * string * syntax * syntax)
  | T_waitfor of (syntax * syntax * syntax * syntax)
  | T_time of (syntax * timeunit)
  | T_freq of (syntax * frequnit)
  | T_branch of (syntax * syntax * syntax)
  | T_forloop of (syntax * char * syntax * syntax * syntax)
  | T_loop of (loop_kind * syntax * syntax)
  | T_select of (syntax * syntax)
  | T_case of (syntax list * syntax)
  | T_process of (syntax * syntax)
  | T_connect of syntax
  | T_export of syntax list
  | T_import of syntax list
  | T_include of syntax
  | T_block of (syntax * syntax option)
  | T_bind of syntax
  | T_topinstr of syntax
  | T_instr of syntax
  | T_list of syntax list
  | T_sub of (syntax * syntax)
  | T_interval of (syntax * syntax)
  | T_selector of (syntax * syntax)
  | T_reg
  | T_var
  | T_sig
  | T_chan
  | T_que
  | T_proc
  | T_monitor of (syntax list * bool)
  | T_typedef of (syntax * syntax * syntax)
  | T_exception of syntax
  | T_raise of syntax
  | T_try of (syntax * syntax)
val t_block_params : (string * string) list -> syntax
type uc_syntax = TU_empty | TU_ident of (file_pos * string)
and expr_syntax =
    TE_empty
  | TE_ident of (file_pos * string)
  | TE_param of (file_pos * string)
  | TE_string of (file_pos * string)
  | TE_expr of (string * expr_syntax * expr_syntax)
  | TE_unit of (expr_syntax * string)
and emi_syntax =
    TM_empty
  | TM_ident of (file_pos * string)
  | TM_value of (char * file_pos * string)
  | TM_string of (file_pos * string)
  | TM_range of (char * emi_syntax * emi_syntax)
  | TM_sub of (emi_syntax * emi_syntax)
  | TM_expr of (string * emi_syntax * emi_syntax)
  | TM_block of emi_syntax list
  | TM_list of emi_syntax list
  | TM_arg of (string * emi_syntax)
  | TM_type of (string * emi_syntax)
  | TM_func of (emi_syntax * emi_syntax list)
  | TM_var of (file_pos * string)
  | TM_sel of (emi_syntax * emi_syntax)
  | TM_foreach of (emi_syntax * emi_syntax * emi_syntax list)
  | TM_for of (emi_syntax * emi_syntax * emi_syntax list)
  | TM_dowith of (emi_syntax * emi_syntax list)
  | TM_param_def of (emi_syntax * emi_syntax list)
  | TM_conditional of (emi_syntax * emi_syntax)
  | TM_version of (file_pos * string)
  | TM_parameter of emi_syntax list
  | TM_methods of emi_syntax list
  | TM_check of emi_syntax list
  | TM_signals of emi_syntax list
  | TM_mappings of emi_syntax list
  | TM_interface of emi_syntax list
  | TM_access of (emi_syntax * emi_syntax list)
  | TM_process of (emi_syntax * emi_syntax list)
  | TM_top of emi_syntax list
  | TM_data of emi_syntax list
  | TM_import of emi_syntax list
  | TM_set of emi_syntax list
  | TM_cond_assign of (emi_syntax * emi_syntax * emi_syntax * emi_syntax)
  | TM_control of emi_syntax list
  | TM_signal of (char * emi_syntax * emi_syntax * emi_syntax)
  | TM_constant of (emi_syntax*emi_syntax*emi_syntax)
  | TM_type_decl of (char * emi_syntax * emi_syntax list * emi_syntax)
  | TM_mapping of (emi_syntax * emi_syntax)
  | TM_seq of emi_syntax list
  | TM_assign of (char * emi_syntax * emi_syntax)
  | TM_wait of (string * emi_syntax)
  | TM_if of (emi_syntax * emi_syntax list * emi_syntax list)
  | TM_case of (emi_syntax * emi_syntax list)
  | TM_when of (emi_syntax * emi_syntax list)
val tm_name_concat : emi_syntax list -> emi_syntax
val tm_name_split : emi_syntax -> emi_syntax
type tdi_syntax =
    TT_empty
  | TT_ident of (file_pos * string)
  | TT_string of (file_pos * string)
  | TT_range of (char * tdi_syntax * tdi_syntax)
  | TT_sub of (tdi_syntax * tdi_syntax)
  | TT_expr of (string * tdi_syntax * tdi_syntax)
  | TT_block of tdi_syntax list
  | TT_list of tdi_syntax list
  | TT_arg of (string * tdi_syntax)
  | TT_func of (tdi_syntax * tdi_syntax list)
  | TT_var of (file_pos * string)
  | TT_sel of (tdi_syntax * tdi_syntax)
  | TT_foreach of (tdi_syntax * tdi_syntax * tdi_syntax list)
  | TT_for of (tdi_syntax * tdi_syntax * tdi_syntax list)
  | TT_conditional of (tdi_syntax * tdi_syntax)
  | TT_version of (file_pos * string)
  | TT_parameter of tdi_syntax list
  | TT_fun of (tdi_syntax * tdi_syntax list)
  | TT_targets of tdi_syntax list
  | TT_target of (tdi_syntax * tdi_syntax list)
  | TT_value of (char * (file_pos * string))
  | TT_assign of (tdi_syntax * tdi_syntax)
  | TT_if of (tdi_syntax * tdi_syntax list * tdi_syntax list)
  | TT_case of (tdi_syntax * tdi_syntax list)
  | TT_when of (tdi_syntax * tdi_syntax list)
and uci_syntax =
    UT_empty
  | UT_string of (file_pos * string)
  | UT_char of (file_pos * char)
  | UT_ident of (file_pos * string)
  | UT_value of (char * file_pos * string)
  | UT_param of (file_pos * string)
  | UT_s_parameter of uci_syntax list
  | UT_s_modules of uci_syntax list
  | UT_s_export of uci_syntax list
  | UT_s_import of uci_syntax list
  | UT_s_types of uci_syntax list
  | UT_s_data of uci_syntax list
  | UT_s_temp of uci_syntax list
  | UT_s_code of uci_syntax list
  | UT_param_def of (uci_syntax * uci_syntax)
  | UT_obj of (char * uci_syntax * uci_syntax * uci_syntax list * uci_syntax)
  | UT_fundef of (uci_syntax * uci_syntax list)
  | UT_type of (char * uci_syntax)
  | UT_list of uci_syntax list
  | UT_op of (uci_syntax * uci_syntax list)
  | UT_immed of (uci_syntax * uci_syntax list)
  | UT_temp of (uci_syntax * uci_syntax list)
  | UT_alu of (uci_syntax * uci_syntax list)
  | UT_selector of uci_syntax list
  | UT_range of (char * uci_syntax * uci_syntax)
  | UT_label of uci_syntax
  | UT_move of (uci_syntax * uci_syntax * uci_syntax list)
  | UT_expr of
      (uci_syntax * uci_syntax * operator * uci_syntax * uci_syntax list)
  | UT_fun of (uci_syntax * uci_syntax list)
  | UT_jump of uci_syntax
  | UT_falsejump of (uci_syntax * uci_syntax)
  | UT_bind of uci_syntax
  | UT_nop
val char_of_type : 'a * string -> char
type synth_tools_syntax =
    TS_version of (file_pos * string)
  | TS_structs of synth_tools_syntax list
  | TS_struct of synth_tools_syntax list
  | TS_param of (string * (file_pos * string))
  | TS_paraml of (string * (file_pos * string) list)
  | TS_paraml2 of (string * ((file_pos * string) * (file_pos * string)) list)
  | TS_paraml21 of
      (string *
       ((file_pos * string) *
        ((file_pos * string) * (file_pos * string)) list)
       list)
