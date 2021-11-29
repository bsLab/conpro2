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
val assign_method_transform : 'a -> Cp_types.instr -> Cp_types.instr list
val assign_struct_transform : 'a -> Cp_types.instr -> Cp_types.instr list
val apply_struct_transform : 'a -> Cp_types.instr -> Cp_types.instr list
val compare_method_transform :
  Cp_types.process option -> Cp_types.instr -> Cp_types.instr list
val apply_method_transform :
  Cp_types.process option -> Cp_types.instr -> Cp_types.instr list
