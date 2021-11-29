open Core;
open Process;
open Uart;
open Random;

type dev_port: {
  port data_in: input logic;
  port data_out: output logic;
  port data: output logic[8];
  port data_en: output logic;
};

component DEV: dev_port;
export DEV;

object ln: uart;
  ln.interface(DEV.data_in,DEV.data_out);
object rnd: random with datawidth=8;

exception SEND_err;
exception RECV_err;

process send:
begin
  reg d: logic[8];
  reg r8: logic[8];
  reg err: bool;
  try
  begin
    for i = 1 to 64 do
    begin
      rnd.read (r8);
      d <- r8;
      ln.write (d,err);
      if err = true then raise SEND_err;
    end;
  end
  with 
  begin
    when SEND_err: ln.stop ();
  end;
end;
               
process recv:
begin
  reg d: logic[8];
  reg err: bool;
  
  try
  begin
    for i = 1 to 64 do
    begin
      ln.read (d,err);
      if err = true then raise RECV_err;
      DEV.data <- d,
      DEV.data_en <- 1;
    end;
  end
  with
  begin
    when RECV_err: ln.stop ();
  end;
end;

process main:
begin
  rnd.init ();
  ln.baud (9600);
  ln.init ();
  ln.start ();
  recv.start ();
  send.start ();
end;
               
