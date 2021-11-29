open Core;
open Process;
open Mutex;
open System;

object sys:system;
  sys.sim_cycles(300);

object m: mutex with scheduler=fifo;

reg x,y,z,d:int[8];

export x,y,z,d;

array p: process[4] of
begin
  for i = 1 to 10 do
  begin
    m.lock ();
    d <- # + 1;
    x <- x + # + 1;
    y <- y + # + 1;
    z <- z + # + 1;
    m.unlock ();
  end;
end;

process main:
begin
  m.init ();
  x <- 0;
  y <- 0;
  z <- 0;
  for i = 0 to 3 do
   p.[i].start (); 
end;
