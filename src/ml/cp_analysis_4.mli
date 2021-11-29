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
val get_obj_of_ident: Cp_syntax.syntax -> Cp_types.instr
val fun_eval : Cp_syntax.syntax -> Cp_syntax.syntax list
