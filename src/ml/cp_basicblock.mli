type majorblock_kind = MJB_control | MJB_data | MJB_fixed
and minorblock_flags = MIB_scheduled | MIB_jump
and minorblock = {
  mutable mib_id : int;
  mutable mib_instr : Cp_types.uc_instr list;
  mutable mib_guards : Cp_types.guard_type list;
  mutable mib_lhs : string list;
  mutable mib_rhs : string list;
  mutable mib_childs : mib_edge;
  mutable mib_parents : mib_edge;
  mutable mib_next: minorblock list;
  mutable mib_flags : minorblock_flags list;
  mutable mib_load : int;
} 
and mib_edge = {
  mutable mie_forward_dep: minorblock list;
  mutable mie_order_dep: minorblock list;
  mutable mie_backward_dep: minorblock list;
  mutable mie_recursive: bool;
 }
and ddg_forest = {
  mutable ddg_root: minorblock;
  mutable ddg_schedule: minorblock list;
 }
and majorblock = {
  mutable mjb_id : int;
  mutable mjb_minorblocks : minorblock list;
  mutable mjb_minorblocks_head : minorblock;
  mutable mjb_kind : majorblock_kind;
  mutable mjb_prev : majorblock option;
  mutable mjb_next : majorblock option;
  mutable mjb_ddg_forest : ddg_forest list;
} 
val ui_schedule_basicblock :
  Cp_types.process -> Cp_types.uc_instr list -> Cp_types.uc_instr list
