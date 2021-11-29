open Core;
open Process;

reg x: int[8];
export x;

process p1:
begin
  reg d1,d2: bool;
  d1 <- true;
  d2 <- true;
  while d1 = true do
  begin
    while d2 = true do
    begin
      x <- x + 1;
      d2 <- false;
    end;
    d1 <- false;
  end;
end;


process main:
begin
  p1.start ();
end;
