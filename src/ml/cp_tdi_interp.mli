class tdi :
  string ->
  Cp_syntax.tdi_syntax list ->
  'b ->
  object ('a)
    method clear_env : unit 
    method copy_env: unit
    method get_env : (string, Cp_tdi_types.tdi_env) Hashtbl.t
    method info : string
    method instance : 'a
    method interp : string -> string
    method new_obj : string -> (string * Cp_tdi_types.tdi_expr) list -> 'a
    method print_env : (string, Cp_tdi_types.tdi_env) Hashtbl.t -> string
    method set_env :
      (string, Cp_tdi_types.tdi_env) Hashtbl.t ->
      Cp_tdi_types.tdi_env list -> unit
    method set_oname : string -> unit
  end
