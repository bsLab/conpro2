class uci_out :
  Cp_types.process ->
  object
    method uci_out_emit : unit
    method uci_out_get_env : Cp_uci_types.uci_env list
    method uci_out_info : string
    method uci_out_interp : string -> string
    method uci_out_set_env : Cp_uci_types.uci_env list -> unit
  end
