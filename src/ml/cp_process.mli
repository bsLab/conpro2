
class process :
  Cp_types.process ->
  object
    method name : string
    method uc_synth : unit
    method uci_out_emit : unit
    method uci_out_get_env : Cp_uci_types.uci_env list
    method uci_out_info : string
    method uci_out_interp : string -> string
    method uci_out_set_env : Cp_uci_types.uci_env list -> unit
    method vhdl_map : string list
    method vhdl_port : string list * string list * int
    method vhdl_synth : unit
    val modu : Cp_types.cp_module
  end
val pro : Cp_types.process -> process
