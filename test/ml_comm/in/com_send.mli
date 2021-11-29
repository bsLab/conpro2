open Com_types
open Com_const
open Com_models
class process_send :
  unit ->
  object ('a)
    method process : unit -> unit
    method stop  : unit
    method start  : unit
    method call  : unit
  end
val send : process_send
