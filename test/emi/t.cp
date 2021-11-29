open Core;
open Process;

const V: value := 16;

type command: {
  REQ;
  REP;
};
open F;
open System;

object sys: system;
  sys.simu_cycles(1000);

object f1: f with datawidth=10;

reg d: int[10];
reg c: int[8];
sig x,y: int[10];
sig rl: logic[10];
export x,y,rl;
block b;
var v: logic[8] in b;

process p1:
begin
  f1.time (2000);
  f1.time (3000);
  f1.time (4000);
  v <- v + 1;
  for i = 1 to 10 do
  begin
    f1.read (to_logic(d));  
    begin
      x <- i;
      y <- d;
    end with bind;
  end;
end;

process main:
begin
  f1.time (1000);
  f1.set (rl);
  f1.init ();
  
  p1.start ();
end;
