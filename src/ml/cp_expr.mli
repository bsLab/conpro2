val is_assign : Cp_types.instr -> bool
val split_assign : Cp_types.instr -> Cp_types.instr * Cp_types.instr
val expr_dt_of_ot :
  Cp_types.object_params list ->
  Cp_types.object_type -> Cp_types.data_type option
val is_const_expr : Cp_types.instr -> bool
val get_const_expr : Cp_types.instr -> int64
val get_const_expri : Cp_types.instr -> int
val obj_dt' : Cp_types.instr -> Cp_types.data_type
val expr_type :
  Cp_types.instr -> bool -> Cp_syntax.operator_group * Cp_types.data_type
val check_expr : Cp_types.instr -> bool
val reduced : int ref
type op = {
  mutable op : Cp_syntax.operator;
  mutable op1 : tree;
  mutable op2 : tree;
} 
and invert = { mutable inv_add : bool; mutable inv_mul : bool; } 
and tree = Node of (invert * op) | Leaf of (invert * Cp_types.instr)
val is_tree_leaf : tree -> bool
val is_tree_value : tree -> bool
val is_tree_object : tree -> bool
val is_tree_node : tree -> bool
val get_tree_value : tree -> int64
val get_tree_inv : tree -> invert
val to_tree : Cp_syntax.operator -> Cp_types.instr -> tree
val sprint_tree : tree -> string
val sprint_node : op -> string
val print_tree : tree -> unit
val of_tree : tree -> Cp_types.instr
val normalize_tree : invert -> tree -> tree
val propagate_tree : tree -> bool
val compacted : int ref
val compact_tree : tree -> tree
val fold_tree : tree -> tree
val expr_fold : Cp_types.instr -> Cp_types.instr
val expr_fold : Cp_types.instr -> Cp_types.instr
val expr_synth :
  Cp_types.process ->
  Cp_types.instr ->
  string ->
  Cp_types.data_type option -> Cp_types.uc_data * Cp_types.uc_instr list
val fun_get_expr :
  Cp_types.process ->
  string -> string -> Cp_types.instr list -> int -> Cp_types.uc_instr list
val fun_is_expr : string -> string -> Cp_types.instr list -> int -> bool
val prev_alu : Cp_types.pro_alu option ref
val expr_assign :
  Cp_types.process ->
  Cp_syntax.operator list ->
  Cp_types.uc_data ->
  Cp_types.uc_data ->
  Cp_types.uc_data ->
  Cp_types.synth_data list * Cp_types.synth_data list *
  Cp_types.synth_data list
val branch_expr :
  Cp_types.process ->
  Cp_syntax.operator list ->
  Cp_types.uc_data ->
  Cp_types.uc_data -> 'a list * 'b list * Cp_types.synth_data list
val expr_bind :
  Cp_types.process ->
  Cp_types.uc_instr list ->
  Cp_types.synth_data list * Cp_types.synth_data list *
  Cp_types.uc_instr list
