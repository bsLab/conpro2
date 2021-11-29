val init_analysis : Cp_types.analysis -> unit
val init_modu :
  Cp_types.analysis -> string -> (string * int * int * int) list -> unit
val subst_ident : string -> string
val core_abstract_object : string -> Cp_types.abstract_object
val source : unit -> Cp_types.source
val get_param_list : Cp_syntax.syntax -> (string * string) list
val get_param_env : Cp_syntax.syntax -> (string * Cp_types.env_expr) list
val filter_env : string -> (string * Cp_types.env_expr) list -> (string * Cp_types.env_expr) list
val to_param_list : (string * string) list -> Cp_types.block_params list
val get_name : Cp_syntax.syntax -> string
val get_names : Cp_syntax.syntax -> string list
val get_name_ext : Cp_syntax.syntax -> string
val get_val : Cp_syntax.syntax -> Cp_types.value
val get_int : Cp_syntax.syntax -> int
val ast_is_int : Cp_syntax.syntax -> bool
val get_time : Cp_syntax.syntax -> int * Cp_syntax.timeunit
val check_const : string -> bool
val get_const : Cp_syntax.syntax -> Cp_types.object_type
val get_type_name : Cp_syntax.syntax -> string
val get_type : Cp_syntax.syntax -> Cp_types.data_type * int
val get_port : Cp_syntax.syntax -> Cp_types.port_type
val new_obj : string -> Cp_types.data_type -> Cp_types.core_object
val get_ts : string -> Cp_types.type_struct
val get_tb : string -> Cp_types.type_bit
val check_tb : string -> bool
val get_struc : Cp_syntax.syntax -> Cp_types.struct_type
val get_obj_name : Cp_types.object_type -> string
val assign_rule : Cp_types.object_type -> unit
val set_param : string -> Cp_syntax.syntax -> unit
val fun_subst :
  (string * 'a * Cp_syntax.syntax) list ->
  Cp_syntax.syntax -> Cp_syntax.syntax
