val self_emi : Cp_emi_interp.emi option ref
val self_obj : Cp_emi_interp.emi option ref
val self : Cp_types.rl_module option ref
val get_env_val : string -> Cp_types.env_expr list
val set_env_val : string -> Cp_types.env_expr -> unit
val my : Cp_types.sym_type -> bool
val obj_port : 'a -> 'b -> 'c -> 'd list * 'e list
val obj_map : 'a -> 'b -> 'c -> 'd list
val obj_decl : 'a -> 'b -> 'c -> 'd list * 'e list
val obj_code : 'a -> 'b -> 'c -> 'd list
val instr_ucode : 'a -> 'b -> 'c -> 'd list
val emit_mon : 'a -> 'b -> 'c -> 'd -> 'e list
val fun_scode : 'a -> 'b -> 'c -> 'd -> 'e -> 'f list
val emit_top_code : 'a -> 'b -> 'c -> 'd list * 'e list
val fun_compile :
  Cp_types.cp_module ->
  Cp_types.process option -> Cp_types.instr -> bool -> unit
val bf_time : 'a -> 'b -> 'c -> Cp_types.frame_time
val rules : Cp_types.rl_module
val init : unit -> unit
