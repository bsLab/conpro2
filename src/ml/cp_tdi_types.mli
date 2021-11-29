type func_builtin =
    F_PRINT
  | F_PRINT_LINE
  | F_PRINT_FILE
  | F_WRITE
  | F_WRITE_LINE
  | F_OPEN_FILE
  | F_CREATE_FILE
  | F_APPEND_FILE
  | F_REMOVE_FILE
  | F_RENAME_FILE
  | F_COPY_FILE
  | F_MOVE_FILE
  | F_LIST
  | F_REMOVE_DIR
  | F_MAKE_DIR
  | F_CHANGE_DIR
  | F_EXIT
  | F_EXEC
  | F_EXEC_LOG
  | F_EXEC_WRITE
  | F_BASENAME
  | F_CHOPEXT
  | F_DIRNAME
  | F_GETENV
  | F_GETOPT
  | F_GETOPTS
  | F_DATE
  | F_TIME
  | F_EXPORT
  | F_PORT
  | F_EXIST
  | F_REV
  | F_USER of string
and target_lang = L_BASH | L_MAKE | L_TCL
and tde_range = TDE_point of int64 | TDE_line of (int64 * int64)
and tdi_expr =
    TDE_int of int
  | TDE_str of string
  | TDE_op of (tdi_expr * string * tdi_expr)
  | TDE_var of string
  | TDE_sel of (string * tdi_expr)
  | TDE_fun of (func_builtin * tdi_expr list)
and tdi_env = {
  mutable tde_name : string;
  mutable tde_val : tdi_expr list;
  mutable tde_range : tde_range list;
  mutable tde_type : char;
  mutable tde_var : bool;
  mutable tde_array : bool;
} 
and tdi_fun = {
  mutable tdf_name : string;
  mutable tdf_code : string list;
  mutable tdf_env : (string, tdi_env) Hashtbl.t;
} 
