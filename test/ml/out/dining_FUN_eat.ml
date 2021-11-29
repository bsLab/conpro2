open Dining_types
open Dining_const
open Dining_models
open Dining_objs

let eat(n) =
  eating.(n) <- 1;
  thinking.(n) <- 0;
  process_delay(5);
  eating.(n) <- 0;
  thinking.(n) <- 1;


