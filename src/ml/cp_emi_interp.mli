class emi :
  string ->
  Cp_syntax.emi_syntax list ->
  'b ->
  object ('a)
    method add_pro: string -> Cp_types.process -> unit 
    method bf_time :
      Cp_types.cp_module ->
      Cp_types.process option -> Cp_types.instr -> Cp_types.frame_time
    method copy_env : unit
    method create_rules : unit
    method fun_compile :
      Cp_types.cp_module ->
      Cp_types.process option -> Cp_types.instr -> bool -> unit
    method fun_scode :
      Cp_types.uc_instr ->
      string ->
      string -> Cp_types.cp_module -> Cp_types.process -> Cp_types.state list
    method get_env : Cp_emi_types.emi_env list
    method get_rules : Cp_types.rl_module
    method info : string
    method init : unit
    method instance : 'a
    method instr_ucode :
      Cp_types.instr -> int ref -> Cp_types.process -> Cp_types.uc_instr list
    method interp : string -> string
    method my : Cp_types.sym_type -> bool
    method new_obj : Cp_types.cp_module -> string -> (string * Cp_types.env_expr) list -> Cp_types.rl_module
    method obj_code :
      Cp_types.sym_type ->
      Cp_types.cp_module -> Cp_types.process option -> string list
    method obj_decl :
      Cp_types.sym_type ->
      Cp_types.cp_module ->
      Cp_types.process option -> string list * string list
    method obj_map :
      Cp_types.sym_type ->
      Cp_types.cp_module -> Cp_types.process option -> string list
    method obj_port :
      Cp_types.sym_type ->
      Cp_types.cp_module ->
      Cp_types.process option -> string list * string list
    method print_env : Cp_emi_types.emi_env list -> string
    method read_access : Cp_emi_types.emi_access list
    method read_methods : Cp_emi_types.emi_method list
    method set_access : Cp_emi_types.emi_access list -> unit
    method set_env : Cp_emi_types.emi_env list -> unit
    method set_methods : Cp_emi_types.emi_method list -> unit
    method set_oname : string -> unit
    method set_module : Cp_types.cp_module -> unit
    method set_process : Cp_types.process -> unit
    method top_vcode :
      Cp_types.instr ->
      Cp_types.cp_module ->
      Cp_types.process option -> string list * string list
    method verbose : int -> unit
  end
