open Core;
open Process;
open Mutex;
open Semaphore;

reg x:int[10];
object mu: mutex;
object sm: semaphore with init=1;
array a: reg[100] of int[10];
type complex : {
  real: int[16];
  imag: int[16];
}; 

reg c1,c2: complex;
array ca: reg[100] of complex;

function f(x:int[8],y:int[8]) return (z:int[8]):
begin
  z <- x+y;
end;

process p1:
begin
  for i = 1 to 10 do
  begin
    mu.lock ();
    x <- x + 1;
    a.[i] <- a.[i] + 1;
    mu.unlock ();  
  end;
  sm.up ();
end;

process p2:
begin
  for i = 1 to 10 do
  begin
    mu.lock ();
    x <- x - 1;
    a.[i] <- a.[i] - 1;
    mu.unlock ();  
  end;
  p1.stop ();
  sm.up ();
end;

array pa: process[4] of
begin
  for i = 1 to 10 do
  begin
    mu.lock ();
    x <- x + 1;
    a.[i] <- a.[#] + #;
    mu.unlock ();  
  end;  
end;

process main:
begin
  mu.init ();
  sm.init (1);
  x <- f(3,4);
  c1.real <- 0;
  c1.imag <- 1;
  for i = 0 to 3 do pa.[i].start();
  
  for i = 1 to 99 do
  begin
    x <- f(ca.[i].real,i);
    ca.[i].real <- ca.[i].imag + i;
    ca.[i].imag <- ca.[i].real + i;
  end;
  p1.start ();
  p2.start ();
  for i = 1 to 2 do
    sm.down ();
end;
