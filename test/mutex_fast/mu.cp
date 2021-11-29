open Core;
open Process;
open Mutex;
open System;

object sys: system;
sys.simu_cycles(200);

array d: reg[4] of logic;
export d;
object mu: mutex with fsm="mealy";

array p: process[4] of
begin
  d.[#] <- 0;
  for i = 1 to 5 do
  begin
    -- wait for 1 microsec;
    mu.lock ();
    d.[#] <- 1;
    mu.unlock ();
    d.[#] <- 0;
  end;
end;

process main:
begin
  mu.init ();
  for i = 0 to 3 do
  begin
    p.[i].start ();
  end with unroll;
end;


