open Core;
open Process;
open Timer;
open System;

object sys:system;
  sys.simu_cycles(300);
  sys.target("a3p125-ft256-2");

object t: timer;
  t.time (1 microsec);

reg d: logic[4];

export d;

array p: process[4] of
begin
  for i = 1 to 5 do
  begin
    t.await ();
    d[#] <- 1;
    d[#] <- 0;
  end;
end;

process main:
begin
  t.init ();
  t.time (2 microsec);
  for i = 0 to 3 do
   p.[i].start (); 
  t.mode (0);
  t.start ();
end;
