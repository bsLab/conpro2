open Core;
open Process;

const USE_PS: value := true;

if USE_PS = true then
begin
  reg x: int[8];
  process p1:
  begin
    for i = 1 to 10 do x <- x + 1;
  end;
  process p2:
  begin
    for i = 1 to 10 do x <- x - 1;
  end;
end;


process main:
begin
  x <- 0;
  if USE_PS = true then
  begin
    p1.start ();
    p2.start ();
  end;
end;
