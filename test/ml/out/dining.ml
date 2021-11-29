open Dining_types
open Dining_const
open Dining_models
open Dining_objs
open Dining_init
open Dining_FUN_eat
open Dining_philosopher
open Dining_main
let conpro () = 
  process_init();
  conpro_exit#init();
  main#call();
  conpro_exit#await()
let _ = conpro ()
