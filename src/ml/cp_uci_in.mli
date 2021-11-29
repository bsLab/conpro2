class uci_in :
  string ->
  Cp_syntax.uci_syntax list ->
  'a ->
  object
    method get_env : Cp_uci_types.uci_env list
    method get_pro : Cp_types.process
    method print_env : string
  end
