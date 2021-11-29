val vhdl_port : Cp_types.cp_module -> string list * string list
val vhdl_synth : Cp_types.cp_module -> unit
class modu :
  Cp_types.cp_module ->
  object
    method compile : unit
    method name : string
    method uc_synth : unit
    method vhdl_port : string list * string list
    method vhdl_synth : unit
  end
val modu_tab : (string, modu) Hashtbl.t
val modu : Cp_types.cp_module -> modu
val emit_modu : Cp_types.cp_module -> unit
