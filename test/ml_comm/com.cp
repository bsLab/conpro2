open Core;
open Process;
open Link;
open Random;

type dev_port: {
  port data_in: input logic[32];
  port data_in_ack: output logic;
  port data_out: output logic[32];
  port data_out_ack: input logic;
  port data: output logic[16];
  port data_en: output logic;
};

component DEV: dev_port;
export DEV;

object ln: link with datawidth=16;
  ln.interface(DEV.data_in,DEV.data_in_ack,
               DEV.data_out,DEV.data_out_ack);
object rnd: random with datawidth=8;

exception SEND_err;
exception RECV_err;

process send:
begin
  reg d: logic[16];
  reg r8: logic[8];
  reg err: bool;
  try
  begin
    for i = 1 to 64 do
    begin
      rnd.read (r8);
      d[0 to 7] <- r8;
      d[8 to 15] <- to_logic(i);
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
  reg d: logic[16];
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
  ln.init ();
  ln.start ();
  recv.start ();
  send.start ();
end;
               
