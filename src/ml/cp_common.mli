val nilsrc : unit -> Cp_types.source
val conpro_path : string
val compiler : Cp_types.compiler
val print_string : string -> unit
val print_newline : unit -> unit
val log_oc : out_channel ref
val out_ind : int ref
val ind_incr : unit -> unit
val ind_decr : unit -> unit
val out : string -> unit
val out_ : string -> unit
val last_progress : string ref
val progress : string -> int -> int -> unit
val vhdl_oc : out_channel ref
val vhdl_ind : int ref
val vhdl_incr : unit -> unit
val vhdl_decr : unit -> unit
val uc_oc : out_channel ref
val uc_ind : int ref
val uc_incr : unit -> unit
val uc_decr : unit -> unit
val vhdl : string -> unit
val uc_out : string -> unit
val to_mod : string -> string
val of_mod : string -> string
val core_rules : Cp_types.rl_module option ref
val process_rules : Cp_types.rl_module option ref
val an : Cp_types.analysis
val line_of_pos :
  Cp_syntax.file_pos -> (string * int * int * int) list -> int * int
val source_of_pos : Cp_syntax.file_pos -> Cp_types.source
val pos_of_source : Cp_types.source -> Cp_syntax.file_pos
val chars : char -> int -> string
val print_err : int -> string -> string
val print_err_tab : int -> string -> string list
val print_syntax_error : string -> int -> 'a -> unit
val print_src : Cp_types.source -> string
val error : int -> string -> 'a
val print_error : string -> unit
val warning : string -> unit
val warning_ : string -> unit
val info : string -> unit
val where : unit -> string
val split_last : 'a list -> 'a list -> 'a list * 'a
val debug_it_select_list : string list ref
val debug_it : string -> bool
val debug_it_print : string -> string -> unit
val lookup_env : (string -> Cp_types.env_expr list) option ref
val store_env : (string -> Cp_types.env_expr -> unit) option ref
val top_env : Cp_types.env list ref
val get_env_val : string -> Cp_types.env_expr list
val get_env : string -> Cp_types.env_expr
val get_env_int64 : string -> int64
val get_env_str : string -> string
val get_env_int : string -> int
val get_env_bool : string -> bool
val get_enc_format : string -> Cp_types.enc_format
val set_env : string -> Cp_types.env_expr -> unit
val print_env : Cp_types.env_expr -> string
