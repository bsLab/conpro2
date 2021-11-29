type data_id =
    T_logic
  | T_int
  | T_str
  | T_char
  | T_bool
  | T_object
  | T_natural
and data_type =
    DT_logic of int
  | DT_int of int
  | DT_string of int
  | DT_char
  | DT_bool
  | DT_object of string
  | DT_lneg
  | DT_aneg
  | DT_bneg
  | DT_natural of int
and port_type = PT_in | PT_out | PT_bus
and value =
    V_int of int64
  | V_float of float
  | V_string of string
  | V_char of char
  | V_logic of string
  | V_bool of bool
  | V_list of value list
  | V_z
  | V_time of (int64 * Cp_syntax.timeunit)
  | V_freq of (int64 * Cp_syntax.frequnit)
  | V_null
and model_params =
    Mp_readsync
  | Mp_readasync
  | Mp_bigendian
  | Mp_littleendian
  | Mp_EREW
  | Mp_CREW
  | Mp_inline
  | Mp_outline
  | Mp_singleport
  | Mp_dualport
  | Mp_multiport
  | Mp_schedule of string
and data_block = {
  mutable db_name : string;
  mutable db_rules : rl_module option;
  mutable db_width : int;
  mutable db_size : int;
  mutable db_objs : object_type list;
  mutable db_params : model_params list;
  mutable db_domain : string;
  mutable db_flags : (string * string) list;
} 
and guard_type = GD_rd | GD_wr | GD_ext
and guard = {
  gd_name : string;
  gd_procs : process list;
  gd_req : guard_type list;
} 
and obj_flags =
    Obj_created
  | Obj_temp
  | Obj_inuse
  | Obj_local
  | Obj_port_in
  | Obj_port_out
  | Obj_port
  | Obj_mon
  | Obj_loop
  | Obj_array
  | Obj_no_guard
  | Obj_schedule of string
and at_flags = AT_dyn | AT_block | AT_static | AT_temp
and array_type = {
  mutable at_name : string;
  mutable at_objs : object_type array;
  mutable at_dim : int array;
  mutable at_flags : at_flags list;
  mutable at_ao : abstract_object;
} 
and struct_type = {
  mutable st_name : string;
  mutable st_type : type_struct;
  mutable st_objs : object_type list;
  mutable st_connect : object_type list;
  mutable st_array : array_type option;
} 
and core_object = {
  co_name : string;
  mutable co_process : process option;
  mutable co_module : cp_module;
  mutable co_rules : rl_module option;
  mutable co_type : data_type;
  mutable co_type_name: string;
  mutable co_level : int;
  mutable co_init : value;
  mutable co_default : value;
  mutable co_guard : guard option;
  mutable co_reader : process list;
  mutable co_writer : process list;
  mutable co_array : array_type list;
  mutable co_struct : struct_type list;
  mutable co_index : int;
  mutable co_size : int;
  mutable co_subsize : int;
  mutable co_block : data_block option;
  mutable co_flags : obj_flags list;
  mutable co_bittype : type_bit option;
  mutable co_domain : string;
} 
and abstract_object = {
  ao_name : string;
  mutable ao_type : type_abstract;
  mutable ao_module : cp_module;
  mutable ao_procs : (string * process) list;
  mutable ao_array : array_type list;
  mutable ao_struct : struct_type list;
  mutable ao_obj : object_type option;
  mutable ao_flags : (string * string) list;
  mutable ao_objs : (string * uc_data) list;
  mutable ao_domain : string;
  mutable ao_env : (string * string) list;
} 
and channel_model =
    Chan_buffered
  | Chan_unbuffered
  | Chan_uni
  | Chan_bi
  | Chan_multi
and channel_type = {
  ch_obj : core_object;
  ch_ot : object_type option;
  mutable ch_model : channel_model list;
  ch_ao : abstract_object;
} 
and queue_type = {
  qu_obj : core_object;
  qu_ot : object_type option;
  qu_depth : int;
  qu_ao : abstract_object;
} 
and object_id =
    O_const
  | O_named_value
  | O_signal
  | O_reg
  | O_var
  | O_channel
  | O_queue
  | O_component
  | O_struct
  | O_array
  | O_object
  | O_value
  | O_reference
and object_type =
    OT_const of core_object
  | OT_named_value of (string * value)
  | OT_signal of core_object
  | OT_reg of core_object
  | OT_var of core_object
  | OT_component of struct_type
  | OT_struct of struct_type
  | OT_array of array_type
  | OT_array_sel of (array_type * int)
  | OT_object of abstract_object
  | OT_value of value
  | OT_channel of channel_type
  | OT_queue of queue_type
  | OT_reference of (string * int list)
and object_params =
    OD_sub of (int * int)
  | OD_index of instr
  | OD_conv of data_type
  | OD_sel of int array
  | OD_sel_obj of instr
  | OD_lneg
  | OD_aneg
  | OD_bneg
and source = {
  mutable s_file : string;
  mutable s_line : int;
  mutable s_cpos : int;
} 
and scheduler = Sched_refstack | Sched_expr | Sched_basicblock
and schedule_mode = Sched_auto | Sched_custom of scheduler list | Sched_def
and expr_type = EXPR_flat | EXPR_alu | EXPR_binary | EXPR_top
and block_params =
    BP_unroll
  | BP_bind
  | BP_expr of expr_type
  | BP_temp of string
  | BP_alu_min_width of int
  | BP_alu_ops of Cp_syntax.operator list
  | BP_alu_type of data_id list
  | BP_inline
  | BP_schedule of schedule_mode
  | BP_locked
and block_constraints = {
  mutable bc_name : string;
  mutable bc_src : source;
  mutable bc_params : block_params list;
} 
and frame_time =
    FT of int
  | FT_min of int
  | FT_max of int
  | FT_minmax of (frame_time * frame_time)
  | FT_n of (frame_time * frame_time)
  | FT_list of frame_time list
  | FT_0
and frame_type =
    BF_data
  | BF_branch
  | BF_compound
  | BF_empty
  | BF_loop
  | BF_conditional
and block_frame = {
  mutable bf_id : int;
  mutable bf_name : string;
  mutable bf_src_start : source;
  mutable bf_src_end : source;
  mutable bf_parent : block_frame option;
  mutable bf_childs : block_frame list;
  mutable bf_time : frame_time;
  mutable bf_loop : frame_time * frame_time;
  mutable bf_type : frame_type;
  mutable bf_params : block_params list;
} 
and instr =
    PI_obj of (object_params list * object_type)
  | PI_tuple of instr list
  | PI_list of instr list
  | PI_block of (instr list * block_frame)
  | PI_arithm of (Cp_syntax.operator * instr * instr)
  | PI_bool of (Cp_syntax.bool_kind * Cp_syntax.operator * instr * instr)
  | PI_concat of (instr * instr)
  | PI_assign of (source * instr * instr)
  | PI_waitfor of (source * instr * int * Cp_syntax.timeunit * instr * instr)
  | PI_branch of (source * instr * instr * instr)
  | PI_forloop of (source * instr * char * instr * instr * instr)
  | PI_loop of (source * Cp_syntax.loop_kind * instr * instr)
  | PI_select of (source * instr * instr)
  | PI_try of (instr * instr)
  | PI_case of (source * instr list * instr)
  | PI_map of (source * instr * instr)
  | PI_fun of
      (source * (object_params list * object_type) * string * instr list)
  | PI_raise of int
  | PI_monitor of (source * sym_type * bool)
  | PI_nop
and pro_alu = {
  mutable alu_name : string;
  mutable alu_ops : Cp_syntax.operator list;
  mutable alu_type : data_type;
} 
and synth_data =
    Data_in of string
  | Data_sens of string
  | Data_trans_sens of string
  | Data_cond_sens of string
  | Data_out of string
  | Data_signal of string
  | Data_trans of string
  | Data_top of string
  | Data_top_def of string
  | Data_cond of string
  | Data_process of string list
  | Data_def of (string * string)
  | Data_trans_def of (string * string)
and case_state = {
  mutable cs_data : synth_data list;
  mutable cs_next : next_state;
} 
and next_state =
    Next of string
  | Next_instr
  | Branch of (synth_data list * next_state * next_state)
  | Select of (synth_data list * case_state list)
and state = {
  s_name : string;
  mutable s_next : next_state;
  mutable s_data : synth_data list;
  mutable s_block : block_frame;
} 
and state_block =
    State of state
  | State_block of state_block list
  | State_top of state
and process = {
  pro_name : string;
  pro_module : cp_module;
  pro_syntax : Cp_syntax.syntax;
  pro_objs : (string, sym_type) Hashtbl.t;
  pro_import : (string, sym_type) Hashtbl.t;
  pro_export : (string, sym_type) Hashtbl.t;
  mutable pro_temps : core_object list;
  mutable pro_instr : instr list;
  mutable pro_ucode : uc_instr list;
  mutable pro_states : state_block list;
  mutable pro_alu : pro_alu list;
  mutable pro_constr : block_constraints list;
  mutable pro_frame : block_frame list;
  mutable pro_ao : abstract_object;
  mutable pro_domain : string;
  mutable pro_control : pro_control;
} 
and pro_control = {
  mutable pro_start : process list;
  mutable pro_stop : process list;
  mutable pro_call : process list;
  mutable pro_raise : type_exception list;
  mutable pro_catch : pro_catch list;
} 
and pro_catch = {
  mutable catch_env : block_frame;
  mutable catch_exl : type_exception list;
  mutable catch_ctl : pro_control;
} 
and module_flags = Mod_main
and cp_module = {
  mutable mod_name : string;
  mutable mod_objs : (string, sym_type) Hashtbl.t;
  mutable mod_export : (string, sym_type) Hashtbl.t;
  mutable mod_procs : process list;
  mutable mod_external : cp_module list;
  mutable mod_import : (string, sym_type) Hashtbl.t;
  mutable mod_rules : rl_module list;
  mutable mod_instr : instr list;
  mutable mod_fmap : (string * int * int * int) list;
  mutable mod_syntax : Cp_syntax.syntax list;
  mutable mod_flags : module_flags list;
} 
and arg_type = Arg_lhs | Arg_rhs | Arg_lrhs
and arg_desc = {
  mutable arg_label : string;
  mutable arg_type : arg_type;
  mutable arg_data_type : data_type;
} 
and rl_module = {
  rl_name : string;
  rl_my : sym_type -> bool;
  rl_obj_port :
    sym_type -> cp_module -> process option -> string list * string list;
  rl_obj_map : sym_type -> cp_module -> process option -> string list;
  rl_obj_decl :
    sym_type -> cp_module -> process option -> string list * string list;
  rl_obj_code : sym_type -> cp_module -> process option -> string list;
  rl_instr_ucode : instr -> int ref -> process -> uc_instr list;
  mutable rl_types : type_abstract list;
  mutable rl_methods : (string * arg_desc list) list;
  rl_fun_compile : cp_module -> process option -> instr -> bool -> unit;
  rl_fun_scode :
    uc_instr -> string -> string -> cp_module -> process -> state list;
  rl_top_vcode :
    instr -> cp_module -> process option -> string list * string list;
  rl_time : cp_module -> process option -> instr -> frame_time;
  rl_new_obj : cp_module -> string -> (string * env_expr) list -> rl_module;
  rl_interp : string -> string;
  mutable rl_child : rl_module option;
} 
and tp_params = TP_bin | TP_gray | TP_name | TP_onehot
and type_const = {
  mutable tc_name : string;
  mutable tc_type : data_type;
  mutable tc_elems : object_type list;
  mutable tc_params : tp_params list;
} 
and type_elem = {
  mutable te_name : string;
  mutable te_type : data_type * int;
  mutable te_port : port_type option;
} 
and type_struct = {
  mutable ts_name : string;
  mutable ts_elems : type_elem list;
} 
and type_method = {
  mutable tm_method : string;
  mutable tm_method_args : arg_desc list;
} 
and type_module = {
  mutable tm_module : string;
  mutable tm_type : string;
  mutable tm_methods : type_method list;
} 
and type_bitrange = {
  mutable tr_name : string;
  mutable tr_range : int * int;
} 
and type_bit = {
  mutable tb_name : string;
  mutable tb_elems : type_bitrange list;
  mutable tb_dt : data_type;
} 
and type_abstract = {
  mutable ta_name : string;
  mutable ta_rules : rl_module;
  mutable ta_flags : (string * string) list;
} 
and type_exception = { mutable tx_name : string; mutable tx_id : int; } 
and typedef =
    Type_const of type_const
  | Type_struct of type_struct
  | Type_module of type_module
  | Type_bit of type_bit
  | Type_abstract of type_abstract
  | Type_exc of type_exception
and fundef = {
  fun_name : string;
  mutable fun_inline : bool;
  mutable fun_args : string list;
  mutable fun_ret : string list;
  mutable fun_instr : Cp_syntax.syntax;
  mutable fun_objs : (string, sym_type) Hashtbl.t;
  mutable fun_args_obj : core_object list;
  mutable fun_ret_obj : core_object list;
} 
and sym_type =
    Sym_mod of cp_module
  | Sym_rule of rl_module
  | Sym_pro of process
  | Sym_obj of object_type
  | Sym_block of data_block
  | Sym_mon of (bool * sym_type)
  | Sym_type of typedef
  | Sym_fun of fundef
and uo_flags = UO_lhs | UO_rhs | UO_loc | UO_ot of object_type
and uo_type = {
  mutable uo_data_type : data_type;
  mutable uo_phys_type : data_type option;
  mutable uo_expr_type : data_type;
  mutable uo_sub_type : data_type option;
  mutable uo_log_type : data_type option;
  mutable uo_conv : data_type option;
  mutable uo_sign : data_type option;
} 
and uc_object = {
  mutable uo_name : string;
  mutable uo_obj : core_object option;
  mutable uo_type : uo_type;
  mutable uo_range : (int * int) option;
  mutable uo_index : uc_arg option;
  mutable uo_addr : int -> string;
  mutable uo_sel : uc_data list;
  mutable uo_block : data_block option;
  mutable uo_flags : uo_flags list;
} 
and uc_temp = {
  mutable ut_type : uo_type;
  mutable ut_range : (int * int) option;
  mutable ut_name : string;
  mutable ut_frame : uc_label option;
  mutable ut_obj : core_object option;
  mutable ut_flags : uo_flags list;
} 
and uc_val = { mutable uv_type : uo_type; mutable uv_val : value; } 
and uc_alu = {
  mutable ua_type : data_type;
  mutable ua_flags : uo_flags list;
} 
and uc_sel = {
  mutable us_name : string;
  mutable us_obj : uc_data;
  mutable us_array : array_type;
  mutable us_sel : uc_data list;
} 
and uc_list = {
  mutable ul_type : uo_type;
  mutable ul_list : uc_data list;
  mutable ul_flags : uo_flags list;
} 
and uc_array = {
  mutable uat_type : uo_type;
  mutable uat_range : (int * int) option;
  mutable uat_name : string;
  mutable uat_obj : array_type;
  mutable uat_flags : uo_flags list;
} 
and uc_data =
    UC_reg of uc_object
  | UC_var of uc_object
  | UC_sig of uc_object
  | UC_chan of uc_object
  | UC_queue of uc_object
  | UC_val of uc_val
  | UC_list of uc_list
  | UC_sel of uc_sel
  | UC_alu of uc_alu
  | UC_immed of int
  | UC_temp of uc_temp
  | UC_bool of int
  | UC_array of uc_array
and uc_label = UC_next | UC_label of string
and uc_code =
    Label of string
  | Bind of int
  | Move of (uc_data * uc_data)
  | Expr of (Cp_syntax.operator list * uc_data * uc_data * uc_data)
  | Falsejump of (uc_data * uc_label)
  | Jump of uc_label
  | Special of instr
  | Fun of ((object_params list * object_type) * string * uc_arg list)
  | Nop
and uc_arg = UA_data of uc_data | UA_expr of uc_instr list
and uc_instr = { mutable ui_code : uc_code; mutable ui_frame : block_frame; } 
and env_range =
    ENV_point of (char * string)
  | ENV_line of ((char * string) * (char * string))
and env_attr = ENV_scalar | ENV_sorted | ENV_filtered
and env_func =
    F_INDEX
  | F_SIZE
  | F_WIDTH
  | F_INDEX_WIDTH
  | F_EXPAND
  | F_MAX
  | F_MIN
  | F_NTH
  | F_MEMBER
  | F_PRINT
  | F_REV
  | F_TO_LOGIC
  | F_TO_INT
  | F_TO_NAT
  | F_TO_STRING
  | F_TO_BOOL
  | F_USER of string
and env_expr =
    ENV_logic of string
  | ENV_int of int64
  | ENV_str of string
  | ENV_sig of env_expr list
  | ENV_bool of bool
  | ENV_range of (char * env_expr * env_expr)
  | ENV_set of env_expr list
  | ENV_op of (env_expr * string * env_expr)
  | ENV_var of string
  | ENV_sel of (env_expr * env_expr)
  | ENV_fun of (env_func * env_expr list)
and env = {
  mutable env_name : string;
  mutable env_val : env_expr list;
  mutable env_range : env_range list;
  mutable env_type : char;
  mutable env_obj : uc_data list;
  mutable env_attr : env_attr list;
} 
exception Synthesis of string
type enc_format = Enc_binary | Enc_onehot | Enc_gray | Enc_auto
and modgen =
    Mod_mult of string
  | Mod_add of string
  | Mod_shift of string
  | Mod_mux of string
  | Mod_ram of string
and vhdl_data = {
  mutable dp_sig : string;
  mutable dp_def : (string * string) list;
  mutable dp_sen : string list;
  mutable dp_aux : string list;
  mutable dp_aux_def : (string * string) list;
  mutable dp_aux_sen : string list;
  mutable cp_sig : string;
  mutable cp_sen : string list;
  mutable dp_conv : string;
  mutable top_def : string list;
  mutable top_expr : string list;
} 
and analysis = {
  mutable a_mname : string;
  mutable a_curpos : source;
  mutable a_funpos : source;
  mutable a_errlabel : string;
  mutable a_toplevel : bool;
  mutable a_exceptions : int;
  mutable a_pro : process option;
  mutable a_pro_syms : (string, sym_type) Hashtbl.t option;
  mutable a_pro_import : (string, sym_type) Hashtbl.t option;
  mutable a_pro_export : (string, sym_type) Hashtbl.t option;
  mutable a_pro_temps : core_object list ref;
  mutable a_pro_name : string;
  mutable a_pro_subst : (string * string) list;
  mutable a_pro_num : int;
  mutable a_fun_subst : (string * string * Cp_syntax.syntax) list;
  mutable a_fun_syms : (string, sym_type) Hashtbl.t option;
  mutable a_fun_name : string;
  mutable a_procs_to_compile :
    (int * string * Cp_syntax.syntax * abstract_object option) list;
  mutable a_block_level : int;
  mutable a_modu : cp_module;
  mutable a_main_aux : Cp_syntax.syntax list;
  mutable a_loop_index : int;
} 
and compiler = {
  mutable t_incl : string list;
  mutable t_lib : string list;
  mutable t_output : string;
  mutable t_defs: (string*string) list;
  mutable t_proj : string;
  mutable t_synth_tool : synthesis_tool;
  mutable t_syntax : bool;
  mutable t_module : bool;
  mutable t_dump : string list;
  mutable t_emit : bool;
  mutable t_synth : bool;
  mutable t_ucode : bool;
  mutable t_ml : bool;
  mutable t_C : bool;
  mutable t_bf_time_calc : bool;
  mutable t_top : string;
  mutable t_files : string list;
  mutable t_debug : bool;
  mutable t_opt : string list;
  mutable t_check : bool;
  mutable t_trace : bool;
  mutable t_silent : bool;
  mutable t_notty: bool;
  mutable t_report: string list;
  mutable t_parse: bool;
  mutable t_graph : string list;
  mutable t_modgens : modgen list;
  mutable t_block_constr : block_constraints;
  mutable t_modules : string list;
} 
and synthesis_tool = {
  mutable syn_tool : string;
  mutable syn_top : string;
  mutable syn_ver : int;
  mutable syn_vhdl_map : (string * string) list;
  mutable syn_vhdl_lib : string list;
  mutable syn_synth_set : (string * (string * string) list) list;
  mutable syn_tech_set : (string * (string * string) list) list;
} 
and device = {
  dev_name : string;
  mutable dev_vendor : string;
  mutable dev_family : string;
  mutable dev_class : string;
  mutable dev_subclass : string;
  mutable dev_ext : string;
  mutable dev_package : string;
  mutable dev_speed : string;
  mutable dev_library : string;
} 
and resource_timing = {
  rt_ops : Cp_syntax.operator list;
  rt_td : data_id;
  rt_fun : int -> float;
} 
and vd_conv = {
  vdc_sig : string;
  vdc_mask : string;
  vdc_desc : (string * data_type) option;
} 
