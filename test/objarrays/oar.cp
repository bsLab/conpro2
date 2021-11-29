open Core;
open Process;
open Mutex;
open Ioport;


const DYNAMIC: value := true;

type top_port: {
  port d1: inout logic[8];
  port d2: inout logic[8];
};
component top: top_port;
export top;

array ma: object mutex[2];
array iop: object ioport[2];
iop.[0].interface(top.d1);
iop.[1].interface(top.d2);


array xa: reg[2] of int[8];
export xa;

type pkt: {
  data: logic[8];
  addr: logic[8];
};

array pkt_pool: var[8] of pkt;

process p1:
begin
  reg d: logic[8];
  reg sp1,sp2: pkt;
  
  ma.[0].lock ();
  xa.[0] <- xa.[0] + 1;
  ma.[0].unlock (); 
  ma.[1].lock ();
  xa.[1] <- xa.[1] + 1;
  ma.[1].unlock (); 


  iop.[1].read (d);
  d <- d - 1;
  iop.[1].write(d);  
  
  sp1 <- sp2;
  sp2 <- sp1;
  
  sp1 <- pkt_pool.[0];
  pkt_pool.[1] <- sp2;
end;

process p2:
begin
  reg d: logic[8];
  
  ma.[0].lock ();
  xa.[0] <- xa.[0] + 1;
  ma.[0].unlock (); 
  ma.[1].lock ();
  xa.[1] <- xa.[1] + 1;
  ma.[1].unlock (); 

  iop.[0].read (d);
  d <- d - 1;
  iop.[0].write(d);  
end;

if DYNAMIC = true then
process p3:
begin
  reg d: logic[8];
  reg p: pkt;
  
  for i = 0 to 1 do
  begin
    ma.[i].lock ();
    xa.[i] <- xa.[i] + 1;
    ma.[i].unlock (); 
  end;
  for i = 0 to 1 do
  begin
    iop.[i].read (d);
    d <- d - 1;
    iop.[i].write(d);
  end;
  for i = 0 to 5 do
  begin
    p <- pkt_pool.[i];
    pkt_pool.[i+1] <- p;  
  end;
end;

process main:
begin
  ma.[0].init ();
  ma.[1].init ();
  iop.[0].init ();
  iop.[1].init ();
  p1.start ();
  p2.start ();
  if DYNAMIC = true then p3.start ();
end;

