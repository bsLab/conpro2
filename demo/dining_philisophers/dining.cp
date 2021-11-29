--
-- Dining philosophers problem using semaphores.
--
-- Five philosophers sit around a circular table. Each philosopher spends
-- his life alternately thinking and eating. In the center of the table is a
-- large platter of spaghetti. Each philosopher needs two forks two eat. But
-- there are only five forks for all. One fork is placed between each pair
-- of philosophers, and they agree that each will use only the forks to the
-- immeadiate left and right.
--
-- [Andrews 2000, Multihtreaded, Parallel, and Distributed Programming]
--

open Core;
open Process;
open Semaphore;
open System;
open Event;
object sys: system;
  sys.simu_cycles (500);
object ev: event;


array eating,thinking: reg[5] of logic;
export eating,thinking;

reg stat: char;
export stat;

array fork: object semaphore[5] with Semaphore.depth=8 and Semaphore.scheduler="fifo";

process init:
begin
  for i = 0 to 4 do
  begin
    fork.[i].init (1);
  end; -- with unroll;
  ev.init ();
end with schedule="basicblock";

function eat(n):
begin
  begin
    eating.[n] <- 1;
    thinking.[n] <- 0;
  end with bind;
  wait for 5;
  begin
    eating.[n] <- 0;
    thinking.[n] <- 1;
  end with bind;
end with inline;

array philosopher: process[5] of
begin
  if # < 4 then
  begin
   ev.await ();
   always do
   begin
     -- get left fork then right
     fork.[#].down ();
     fork.[#+1].down ();
     eat (#);
     fork.[#].up ();
     fork.[#+1].up ();
   end;
  end
  else
  begin
   always do
   begin
     -- get right fork then left
     fork.[4].down ();
     fork.[0].down ();
     eat (#);
     fork.[4].up ();
     fork.[0].up ();
   end;
  end;
end;

process main:
begin
  stat <- 'I';
  init.call ();
  stat <- 'S';
  for i = 0 to 4 do
  begin
    philosopher.[i].start ();
  end;
  stat <- 'W';
  ev.wakeup ();
end with schedule="basicblock";

-- monitor fork,main,philosopher;
-- monitor fork;
