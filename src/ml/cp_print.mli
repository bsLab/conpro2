val box : int -> string -> string -> string
val align : int -> string -> string
val remove_last_nl : string -> string
val hlist : ('a -> string) -> 'a list -> string
val vlist : ('a -> string) -> 'a list -> string
val ilist : ('a -> string) -> 'a list -> string
val ibox : int -> string -> string
val lbox : int -> string -> string
type table = {
  tab_outline : bool;
  tab_width : int;
  tab_title : string;
  mutable tab_header : tab_row list;
  mutable tab_rows : tab_row list;
} 
and tab_row = { row_cols : tab_col list; } 
and tab_col = { col_width : int; col_align : char; col_cont : string; } 
val format_table : table -> string list
