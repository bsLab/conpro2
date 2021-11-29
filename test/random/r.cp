open Core;
open Process;
open Random;
open System;

object sys: system;
  sys.sim_cycles(100);

object rnd1: random with datawidth=10;
  rnd1.seed (1000);

reg d: int[10];
reg e: logic[12];
reg f: int[4];
reg g: logic[4];

reg c: int[8];
sig x,y: int[10];
export x,y;


process p1:
begin
  -- type conversion and scaling test
  rnd1.read (e);
  e <- e + 1;
  rnd1.read (to_logic(f));
  f <- f + 1;
  rnd1.read (g);
  g <- g + 1;
  
  -- random generator test
  for i = 1 to 10 do
  begin
    rnd1.read (to_logic(d));  
    begin
      x <- i;
      y <- d;
    end with bind;
  end;
end;

process main:
begin
  rnd1.init ();
  p1.start ();
end;
