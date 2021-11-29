open Core;
open Process;
open Link;
open System;
open Clock;

object sysclk: clock;
  sysclk.clock(1 megahz);
  sysclk.source("CLK");
  
--
-- Toplevel port
--
--  input: from external world into conpro, read signal
--  output: to external world from conpro, write signal
--
type dev_top_port:{
  port ln_din: input logic[20];
  port ln_din_ack: output logic;
  port ln_dout: output logic[20];
  port ln_dout_ack: input logic;
};
component DEV: dev_top_port;
export DEV;

--
-- Embedded VHDL component port
--
--  input: to component from conpro, write signal
--  output: from component to conpro, read signal
--
type dev_keyboard:{
  port kb_data: output logic[8];
  port kb_init: input logic;
  port kb_clk: input logic;
};
component KEY: dev_keyboard;
sysclk.add(KEY.kb_clk);

object sys: system;
  sys.simu_cycles(100);

object ln: link with datawidth=10;
  ln.interface(DEV.ln_din,DEV.ln_din_ack,DEV.ln_dout,DEV.ln_dout_ack);
  
reg x,y: int[10];
reg xa,ya: int[8];
reg d: logic[8];
export x,y,xa,ya,d;

exception Exit;


process p1:
begin
  reg err:bool;
  reg d:logic[10];

  try
  begin
    for i = 1 to 10 do
    begin
      xa <- 'r';
      ln.read (d,err);
      xa <- '.';
      if err = true then raise Exit;  
      x <- to_int(d);
    end;
  end
  with
  begin
    when Exit: ln.stop ();
  end;
end;

process p2:
begin
  reg err:bool;
  reg d:logic[10];


  try
  begin
    for i = 1 to 10 do
    begin
      d <- to_logic(i);
      y <- i;
      ya <- 'w';
      ln.write(d,err);
      ya <- '.';
      if err = true then raise Exit;  
    end;
  end
  with
  begin
    when Exit: ln.stop ();
  end;
end;

process main:
begin
  ln.init ();
  ln.start ();
  p1.start ();
  p2.start ();
  wait for 100 with KEY.kb_init <- 1;
  always do
  begin
    d <- KEY.kb_data;
  end;
end;

module C_simu:
begin
  --
  -- Instantiate components, import
  -- THIS toplevel module.
  --
  import C;
  component Cc1,Cc2: C;
  
  --
  -- Structural Interconnection
  --
  type c_connect: {
    port Cc1_ln_din: output logic[20];
    port Cc1_ln_din_ack: input logic;
    port Cc1_ln_dout: input logic[20];
    port Cc1_ln_dout_ack: output logic;
    port Cc2_ln_din: output logic[20];
    port Cc2_ln_din_ack: input logic;
    port Cc2_ln_dout: input logic[20];
    port Cc2_ln_dout_ack: output logic;
  };
  component C_c: c_connect :=
                  {Cc1.DEV.ln_din,
                   Cc1.DEV.ln_din_ack,
                   Cc1.DEV.ln_dout,
                   Cc1.DEV.ln_dout_ack,
                   Cc2.DEV.ln_din,
                   Cc2.DEV.ln_din_ack,
                   Cc2.DEV.ln_dout,
                   Cc2.DEV.ln_dout_ack};
  --
  -- Interconnect
  --
  C_c.Cc1_ln_din << C_c.Cc2_ln_dout;
  C_c.Cc1_ln_din_ack >> C_c.Cc2_ln_dout_ack;
  C_c.Cc1_ln_dout >> C_c.Cc2_ln_din;
  C_c.Cc1_ln_dout_ack << C_c.Cc2_ln_din_ack;
end;


