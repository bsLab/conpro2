open Core;
open Process;
open Barrier;
open System;

object sys:system;
  sys.sim_cycles(300);

object b: barrier;

array d: reg[4] of int[8];

export d;

array p: process[4] of
begin
  for i = 1 to 5 do
  begin
    b.await ();
    d.[#] <- # + 1;
    d.[#] <- 0;
  end;
end;

process main:
begin
  b.init ();
  -- d <- 64;
  for i = 0 to 3 do
   p.[i].start (); 
end;
