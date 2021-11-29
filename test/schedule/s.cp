open Core;
open Process;

reg x: int[8];

process p1_def:
begin
  reg y,z: int [8];
  begin
    y <- x;
    z <- x;
  end with bind;
  for i = 1 to 10 do
  begin
    y <- y + 1;
    z <- z + 1;
  end;
  x <- y + z;
end;

process p1_opt:
begin
  reg y,z: int [8];
  begin
    y <- x;
    z <- x;
  end with bind;
  for i = 1 to 10 do
  begin
    y <- y + 1;
    z <- z + 1;
  end;
  x <- y + z;
end with schedule="basicblock";

process main:
begin
  x <- 1;
  p1_def.call ();
  p1_opt.call ();
end;
