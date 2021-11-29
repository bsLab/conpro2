val main_module : Cp_types.cp_module option ref
val print_sym : Cp_types.sym_type -> unit
val print_instr : Cp_types.instr -> unit
val print_module : Cp_types.cp_module -> unit
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
val get_obj : Cp_syntax.syntax -> Cp_types.object_type list
val pro_sym_add : Cp_types.object_type -> unit
val fun_sym_add : Cp_types.object_type -> unit
val pro_sym_check : string -> bool
val pro_sym_lookup : string -> Cp_types.object_type
val get_typedef :
  string -> Cp_syntax.syntax list -> Cp_types.object_type list
val get_param : 'a -> ('a * 'b) list -> 'b option
val get_objs : Cp_syntax.syntax -> Cp_types.object_type list
val get_sub : Cp_syntax.syntax -> int * int
val check_bind : Cp_types.instr -> unit
val get_obj_of_ident : Cp_syntax.syntax -> Cp_types.instr
val fun_eval : Cp_syntax.syntax -> Cp_syntax.syntax list
val expand_sel_expr :
  Cp_types.process option -> Cp_types.instr list -> Cp_types.instr list
val expand_sel_expr :
  Cp_types.process option -> Cp_types.instr list -> Cp_types.instr list
val expand_fun_arg :
  Cp_types.process option -> Cp_types.instr list -> Cp_types.instr list
val expand_fun_arg :
  Cp_types.process option -> Cp_types.instr list -> Cp_types.instr list
val expand_expr :
  Cp_types.process option -> Cp_types.instr list -> Cp_types.instr list
val expand_expr :
  Cp_types.process option -> Cp_types.instr list -> Cp_types.instr list
val expand_arith_expr :
  Cp_types.process option -> Cp_types.instr list -> Cp_types.instr list
val expand_arith_expr :
  Cp_types.process option -> Cp_types.instr list -> Cp_types.instr list
val assign_struct_transform : 'a -> Cp_types.instr -> Cp_types.instr list
val apply_struct_transform : 'a -> Cp_types.instr -> Cp_types.instr list
val assign_method_transform : 'a -> Cp_types.instr -> Cp_types.instr list
val compare_method_transform :
  Cp_types.process option -> Cp_types.instr -> Cp_types.instr list
val apply_method_transform :
  Cp_types.process option -> Cp_types.instr -> Cp_types.instr list
val array_change_dyn : Cp_types.array_type -> unit
val get_instr : Cp_syntax.syntax -> Cp_types.instr list
val resolve_obj_dep : Cp_types.process -> unit
val syntax_of_file :
  string -> Cp_syntax.syntax list * (string * int * int * int) list
val module_of_syntax :
  string ->
  Cp_syntax.syntax list ->
  (string * int * int * int) list -> Cp_types.cp_module
