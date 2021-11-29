open Core;
open Process;
open System;

object sys:system;
  sys.simu_cycles(200);
  
block ram1 with Ram.arch="dualport";
array ar: var[10] of int[8] in ram1;

reg sum: int[16];
reg dbg: int[8];
export sum,dbg;

process p1:
begin
  sum <- 0;
  dbg <- (-1);
  for i = 1 to 10 do
  begin
    dbg <- i;
    sum <- sum + ar.[i-1]; 
    ar.[i-1] <- sum;
  end; 
  dbg <- (-2);
end with schedule="basicblock";

process main:
begin
  for i = 1 to 10 do
  begin
    ar.[i-1] <- i;
    dbg <- ar.[i-1];
  end with schedule="basicblock";
  p1.start ();
end;

