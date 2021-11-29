open Dining_types
open Dining_const
open Dining_models
open Dining_objs
open Dining_init
open Dining_philosopher

let pro_main process_id =
  stat <- 'I';
  init#call();
  stat <- 'S';
  for i = 0 to 4 do
    philosopher.(i)#start();
  done;
  stat <- 'W';
  ev#wakeup();
  process_end()

let main = new process ["name","main"] pro_main
