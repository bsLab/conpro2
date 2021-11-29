type uci_env = {
  mutable uce_name : string;
  mutable uce_val : string;
  mutable uce_type : char;
  mutable uce_var : bool;
  mutable uce_array : bool;
} 
and uci_type = { uct_name : string; uct_kind : char; uct_def : string list; } 
