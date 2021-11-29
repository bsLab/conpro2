val vhdl_tools : (string * Cp_tdi_interp.tdi) list ref
val synth_tools : Cp_types.synthesis_tool list ref
val open_synth_tools : unit -> unit
val open_generic : unit -> unit
val vhdl_src : Cp_types.cp_module -> string -> string list
val open_toolset : string -> unit
val emit_toolset : Cp_types.cp_module -> unit
