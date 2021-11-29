open Core;
open Process;
open Random;

object rnd: random with datawidth=10 and datatype="int";

type s1: {
  se1: int[4];
  se2: int[4];
};

type s2: {
  se1: int[4];
  se4: int[64];
};

reg sx1: s1;
reg sx2: s2;
reg x: int[10];

array sa1: reg[4] of s1;
array sa2: reg[4] of s2;

export sa1;

function f(x: int[8],a:int[4]) return (y:int[8]):
begin
  y <- x*x+a;
end;

function f2(x: int[8],a:int[4]) return (y:int[8],z:int[8]):
begin
  y <- x*x+a;
  z <- x*x-a;
end;

process p1:
begin
  reg d,e: int[10];

  d <- 4;
  d <- f(d,5);
  {d,e} <- f2(d,5);
  for i = 0 to 3 do
  begin
    d <- d + i;
    sa1.[i].se1 <- sa1.[i].se1+1;
    sa1.[i].se2 <- sa1.[i].se2+1;
  end;
  for i = 1 to 2 do
  begin
    sa1.[i+1].se2 <- sa1.[i-1].se2+1;
  end;
  for i = 0 to 3 do
  begin
    sa2.[i].se1 <- sa2.[i].se1+1;    
    sa2.[i].se4 <- sa2.[i].se4+1;    
  end with unroll;
  sx1.se1 <- sx1.se1 + 1;
  sx1.se2 <- sx1.se2 + 1;
  sx2.se1 <- sx2.se1 + 1;
  sx2.se4 <- sx2.se4 + 1;
  rnd.read (d);
  sx2.se4 <- d; 
  x <- x + d;
end;

process main:
begin
  x <- 0;
  rnd.seed (1234);
  rnd.init ();
  p1.start ();
end;
