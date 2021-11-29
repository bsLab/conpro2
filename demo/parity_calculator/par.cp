open Core;
open Process;
open Mutex;

const WIDTH: value := 64; 
reg d: logic[WIDTH];
reg p: logic;
export p,d;

function parity1 (x: logic[WIDTH]) 
         return (p: logic): 
begin
  reg pl: logic;
  reg xl: logic[WIDTH];
  xl <- x;
  pl <- 0;
  for i = 0 to WIDTH-1 do
  begin
    pl <- pl lxor xl[i];
  end;
  p <- pl;
end;

function parity2 (x: logic[WIDTH]) 
         return (p: logic): 
begin
  reg pl: logic;
  reg xl: logic[WIDTH];
  xl <- x;
  pl <- 0;
  for i = 0 to WIDTH-1 do
  begin
    pl <- pl lxor xl[i];
  end;
  p <- pl;
end with schedule="basicblock";
q
function parity3 (x: logic[WIDTH]) 
         return (p: logic): 
begin
  reg pl: logic;
  reg xl: logic[WIDTH];
  xl <- x;
  pl <- 0;
  for i = 0 to WIDTH-1 do
  begin
    pl <- pl lxor xl[i];
  end with unroll;
  p <- pl;
end;

function parity4 (x: logic[WIDTH]) 
         return (p: logic): 
begin
  reg pl: logic;
  reg xl: logic[WIDTH];
  xl <- x;
  pl <- 0;
  for i = 0 to WIDTH-1 do
  begin
    pl <- pl lxor xl[i];
  end with unroll;
  p <- pl;
end with schedule="refstack";

function parity5 (x: logic[WIDTH]) 
         return (p: logic): 
begin
  reg pl: logic;
  reg xl: logic[WIDTH];
  xl <- x;
  pl <- 0;
  for i = 0 to WIDTH-1 do
  begin
    pl <- pl lxor xl[i];
  end with unroll;
  p <- pl;
end with schedule="basicblock";

function parity6 (x: logic[WIDTH]) 
         return (p: logic): 
begin
  reg pl: logic;
  reg xl: logic[WIDTH];
  xl <- x;
  pl <- 0;
  for i = 0 to WIDTH-1 do
  begin
    pl <- pl lxor xl[i];
  end with unroll;
  p <- pl;
end with schedule="refstack,basicblock";

process main: 
begin
  d <- 0x12345670;
  p <- parity1(d);
  p <- parity2(d);
  p <- parity3(d);
  p <- parity4(d);
  p <- parity5(d);
  p <- parity6(d);
  
end;
