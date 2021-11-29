open Core;
open Process;
open System;

reg d: int[16];
export d;

const FPGA: value := true;
if FPGA = true
then
begin 
  object sys: system;
  sys.clock  (18500 kilohz);
  sys.target ("xc3s1000-ft256-5");
  --sys.target ("xcf04s");
  sys.target ("xc18v04");
  sys.reset_level (0);
end;

process p1:
begin
  reg sum,n: int[16];
  sum <- d,n <- 2;
  for i = 0 to 15 do
  begin
    sum <- sum + i + n;
    n <- n + 2; 
  end with unroll;
  d <- sum;
end with expr="shared";

process p2:
begin
  reg prod,n: int[16];
  prod <- d, n <- 2;
  for i = 1 to 16 do
  begin
    prod <- prod * i * n;
    n <- n * 2; 
  end with unroll;
  d <- prod;
end with expr="shared";

process main:
begin
  p2.start ();
  
end;
