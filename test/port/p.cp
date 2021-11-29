open Core;
open Process;
open Ioport;

type top_port: {
  port p_io1: inout logic[8];
  port p_io2: inout logic[8];
};
component top: top_port; 
export top;

object pt1,pt2: ioport with width=8;
  pt1.interface(top.p_io1);
  pt2.interface(top.p_io2);
  
process p1:
begin
  reg d1,d2: logic[8];
  
  
  pt1.dir(0b00000000);
  pt2.dir(0b11111111);
  pt1.read(d1);
  pt2.read(d2);
  
  for i = 1 to 8 do
  begin
    d1 <- d1 land 0b00001111;
    d1[4 to 7] <- to_logic(i);
    d2[3] <- 1;
    pt1.write(d1);
    pt1.dir(0b11111000),pt2.write(d2);
    d2[3] <- 0;
    pt1.dir(0b00000000),pt2.write(d2);
    pt1.read (d1);
    while d1[0] = 0 do
      pt1.read (d1);
  end;
end;

process main:
begin
  pt1.init ();
  pt2.init ();
  p1.start ();
end;
