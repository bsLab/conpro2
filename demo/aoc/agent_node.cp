open System;

const TABLE_SIZE : value := 16;
const TABLE_SIZE2 : value := TABLE_SIZE*2;
const NUM_LINKS : value := 4;
const MAX_LIVE : value := 7;

const CLOCK : value := 50;
const TARGET : value := 5;

object sys : system;

if CLOCK = 50 then
begin
  sys.clock  (50000 kilohz);
end;
if CLOCK = 20 then
begin
  sys.clock  (18500 kilohz);
end;

if TARGET = 5 then
begin
  sys.target ("xc3s1000-ft256-5");
  --sys.target ("xcf04s");
  sys.target ("xc18v04");
  sys.reset_level (0);
end;

if TARGET = 1 then
begin
  sys.target ("xc3s500e-cp132");
  sys.target ("xcf04s");
  sys.reset_level (0);
  sys.reset_internal (1); 
end;
