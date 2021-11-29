open Core;
open Process;

const ADC: value := 2;

process main:
begin
  reg x,y: int[8];
  
  x <- 0; x <- x + 1;
  y <- 1; y <- x + 1;
  if ADC=2 and x=1 and y = 0 then x <- x - 1;
  
end;
