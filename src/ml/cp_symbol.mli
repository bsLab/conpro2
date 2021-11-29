val top_syms : (string, Cp_types.sym_type) Hashtbl.t
val sym_lookup : (string, 'a) Hashtbl.t -> string -> 'a
val sym_get_obj :
  (string, Cp_types.sym_type) Hashtbl.t -> string -> Cp_types.object_type
val sym_get_mod :
  ('a, Cp_types.sym_type) Hashtbl.t -> 'a -> Cp_types.cp_module
val sym_get_rule :
  ('a, Cp_types.sym_type) Hashtbl.t -> 'a -> Cp_types.rl_module
val sym_get_type :
  ('a, Cp_types.sym_type) Hashtbl.t -> 'a -> Cp_types.typedef
val sym_get_pro :
  (string, Cp_types.sym_type) Hashtbl.t -> string -> Cp_types.process
val sym_get_fun : ('a, Cp_types.sym_type) Hashtbl.t -> 'a -> Cp_types.fundef
val sym_check : ('a, 'b) Hashtbl.t -> 'a -> bool
val sym_check_obj : ('a, Cp_types.sym_type) Hashtbl.t -> 'a -> bool
val sym_check_mod : ('a, Cp_types.sym_type) Hashtbl.t -> 'a -> bool
val sym_check_rule : ('a, Cp_types.sym_type) Hashtbl.t -> 'a -> bool
val sym_check_pro : (string, Cp_types.sym_type) Hashtbl.t -> string -> bool
val sym_check_type : ('a, Cp_types.sym_type) Hashtbl.t -> 'a -> bool
val sym_check_fun : ('a, Cp_types.sym_type) Hashtbl.t -> 'a -> bool
val sym_check_block : ('a, Cp_types.sym_type) Hashtbl.t -> 'a -> bool
val sym_check_ext : Cp_types.cp_module -> string -> bool
val sym_lookup_ext : Cp_types.cp_module -> string -> Cp_types.sym_type
val sym_check_obj_ext : Cp_types.cp_module -> string -> bool
val sym_get_obj_ext : Cp_types.cp_module -> string -> Cp_types.object_type
val sym_check_mod_ext : Cp_types.cp_module -> string -> bool
val sym_get_mod_ext : Cp_types.cp_module -> string -> Cp_types.cp_module
val sym_check_pro_ext : Cp_types.cp_module -> string -> bool
val sym_get_pro_ext : Cp_types.cp_module -> string -> Cp_types.process
val sym_check_type_ext : Cp_types.cp_module -> string -> bool
val sym_get_type_ext : Cp_types.cp_module -> string -> Cp_types.typedef
val sym_check_fun_ext : Cp_types.cp_module -> string -> bool
val sym_get_fun_ext : Cp_types.cp_module -> string -> Cp_types.fundef
val sym_name : Cp_types.sym_type -> string
val sym_add :
  (string, Cp_types.sym_type) Hashtbl.t -> Cp_types.sym_type -> unit
val sym_check_sym : (string, 'a) Hashtbl.t -> Cp_types.sym_type -> bool
val sym_delete : (string, 'a) Hashtbl.t -> Cp_types.sym_type -> unit
val sym_move :
  (string, Cp_types.sym_type) Hashtbl.t ->
  (string, Cp_types.sym_type) Hashtbl.t -> string -> unit
val sym_copy : Cp_types.process -> Cp_types.sym_type -> Cp_types.sym_type
val list_of_sym : ('a, 'b) Hashtbl.t -> 'b list
val length : ('a, 'b) Hashtbl.t -> int
