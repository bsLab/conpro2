open Core;
open Process;

block ram1;
array ar: var[10] of int[8] in ram1;


reg d: int[8];
export d;

process p1:
begin
  for i = 1 to 10 do
  begin
    ar.[i-1] <- i; 
  end;
end;

process p2:
begin
  for i = 1 to 10 do
  begin
    ar.[i-1] <- ar.[i-1] - 1; 
  end;  
end;


process main:
begin
  d <- 1;
  p1.call ();
  d <- 2;
  p2.call ();
  d <- 0;
end;

monitor p1,p2;
