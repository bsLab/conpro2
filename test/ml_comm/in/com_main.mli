open Com_types
open Com_const
open Com_models
class process_main :
  unit ->
  object ('a)
    method process : unit -> unit
    method stop  : unit
    method start  : unit
    method call  : unit
  end
val main : process_main
