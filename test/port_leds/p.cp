open Core;
open Process;
open Ioport;
open System;

object sys : system;
  sys.clock  (18500 kilohz);
  -- sys.clock  (20000 kilohz);
  sys.target ("xc3s1000-ft256-5");
  --sys.target ("xcf04s");
  sys.target ("xc18v04");
  sys.reset_level (0);


type top_port: {
  port p_leds: inout logic[8];
};
component top: top_port; 
export top;

object pt: ioport with width=8;
  pt.interface(top.p_leds);
  
process p1:
begin
  reg d: logic[8];
  
  
  pt.dir(0b11111111);

  always do
  begin
    for i = 0 to 7 do
    begin
      d <- 0;
      d[i] <- 1;
      pt.write(d);
      wait for 1 sec;
    end;
  end;
end;

process main:
begin
  pt.init ();
  p1.start ();
end;
