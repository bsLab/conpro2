open Com2_types
open Com2_const
open Com2_models
class process_recv :
  unit ->
  object ('a)
    method process : unit -> unit
    method stop  : unit
    method start  : unit
    method call  : unit
  end
val recv : process_recv
