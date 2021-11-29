open Com_types
open Com_const
open Com_models
class process_recv :
  unit ->
  object ('a)
    method process : unit -> unit
    method stop  : unit
    method start  : unit
    method call  : unit
  end
val recv : process_recv
