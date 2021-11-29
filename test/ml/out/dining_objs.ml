open Dining_types
open Dining_const
open Dining_models
let conpro_exit = new event []
let mutable stat = ' '
let sys = new system []
let ev = new event []
let thinking = Array.init 5 (fun i -> 0)
let fork = Array.init 5 (fun i -> new semaphore ["depth","8";"scheduler","fifo"])
let eating = Array.init 5 (fun i -> 0)
