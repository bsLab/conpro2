open Core;
open Process;

const DSC: value := 8;
const DSC1: value := DSC+1;

process main:
begin
  reg x: int[DSC];
  reg y: int[DSC1];
  reg z: int[(DSC+1)*2];
  
  x <- 0, y <- 1, z <- 0;
  z <- z+x+y;
  
end;
