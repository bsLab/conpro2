open Core;
open Process;
open Ioport;
open Random;

type top_dev : {
  port xleds: inout logic[8];
};

component top: top_dev;
export top;


object iop: ioport;
  iop.interface(top.xleds);
object rnd: random with datawidth=8;

process p1:
begin
  reg d:logic[8];
  for i =1 to 8 do
  begin
    rnd.read(d);
    iop.write(d);
  end;
end;

process main:
begin
  iop.init ();
  iop.dir (0xFF);
  rnd.seed(1234);
  rnd.init();
  p1.start ();
end;
