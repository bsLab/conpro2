open Core;
open Process;

reg x:int[8];
export x;

process p1:
begin
  reg a: int[8];
  a <- x;
  for i = 1 to 10 do
   a <- a * 2;
  x <- a;
end;

process main:
begin
  p1.start ();
end;