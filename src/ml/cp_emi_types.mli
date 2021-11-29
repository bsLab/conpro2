type emi_env = Cp_types.env
and emi_method = {
  mutable emm_name : string;
  mutable emm_args : Cp_types.arg_desc list;
  mutable emm_procs : Cp_types.process list;
} 
and emi_signal = {
  mutable emi_sig_name : string;
  mutable emi_sig_type : Cp_types.data_type;
  mutable emi_sig_dir : Cp_types.port_type option;
  mutable emi_procs : string list;
  mutable emi_kind : char;
} 
and emi_const = {
  mutable emi_const_name: string;
  mutable emi_const_type: Cp_types.data_type;
  mutable emi_const_val: string;
}
and emi_type = {
  mutable emt_type_name : string;
  mutable emt_type_type : Cp_types.data_type option;
  mutable emt_type_range : Cp_types.env_range option;
  mutable emt_type_cont : string list;
  mutable emt_kind : char;
} 
and emi_mapping = {
  mutable ema_lhs : string;
  mutable ema_rhs : string;
  mutable ema_procs : string list;
} 
and emi_arg_context = {
  mutable eac_pro : Cp_types.process;
  mutable eac_ao : Cp_types.abstract_object;
  mutable eac_uargs : Cp_types.uc_arg list;
} 
and emi_access = {
  mutable emc_name : string;
  mutable emc_data :
    (emi_arg_context option -> Cp_types.synth_data list) list;
  mutable emc_control :
    (emi_arg_context option -> Cp_types.synth_data list) list;
  mutable emc_set : (emi_arg_context option -> Cp_types.env_expr * Cp_types.env_expr) list;
} 
and emi_process = {
  mutable emp_name : string;
  mutable emp_decl : string list;
  mutable emp_code : string list;
  mutable emp_sens : string list;
} 
