open Conpro
open Process
open Cp_event
open Cp_queue
open Printf

let conpro_exit = new event ["latch","1"] 
let ev12 = new event []
let q = new queue [] 0
  
let ep1 id =
  conpro_print "EP1 started";
  for i = 1 to 5 
  do
    ev12#await ();
    conpro_print "EP1";
  done;
  process_end ()
  
let ep2 id =
  conpro_print "EP2 started";
  for i = 1 to 5 
  do
    conpro_print "EP2";
    ev12#wakeup ();
  done;
  conpro_exit#wakeup ();
  process_end ()

let qp1 id =
  conpro_print "QP1 started";
  for i = 1 to 5 
  do
    conpro_print "QP1: read...";
    let x = q#read () in
    conpro_print (sprintf "QP1: %d" x);
  done;
  process_end ()
  
let qp2 id =
  conpro_print "QP2 started";
  for i = 1 to 5 
  do
    conpro_print "QP2";
    q#write i;
    process_schedule ();
  done;
  conpro_exit#wakeup ();
  process_end ()


  
let _ =
  process_init ();
  let pro1 = new process ["name","p1";"id","0"] qp1 in
  let pro2 = new process ["name","p2";"id","0"] qp2 in
  pro1#start ();
  pro2#start ();
  conpro_exit#await ();
  conpro_print "DONE."
  
  
  
