open Dining_types
open Dining_const
open Dining_models
open Dining_objs

let pro_init process_id =
  for i = 0 to 4 do
    fork.(i)#init(1);
  done;
  ev#init();
  process_end()

let init = new process ["name","init"] pro_init
