open Core;
open Process;
open Queuex;
open System;

object sys: system;
  sys.simu_cycles(1000);
  sys.set_env("processes","main");
  sys.set_env("processes","p1");
  sys.set_env("processes","p2");
  
object q: queuex with datatype="logic";

reg d1,d2: logic[8];
export d1,d2;

process p1:
begin
  reg d: logic[8];
  d <- 0x41;
  for i = 0 to 9 do 
  begin
    d <- d + 1;
    d1 <- d;
    q.write(d);
  end;
end;

process p2:
begin
  reg d: logic[8];
  d <- 0;
  for i = 0 to 9 do 
  begin
    q.read(d);
    d2 <- d;
  end;
end;

process main:
begin
  q.init ();
  p1.start ();
  p2.start ();
end;
