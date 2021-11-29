open Core;
open Process;
open Link2;
open System;

type dev_type:{
  port ln_din: input logic[2];
  port ln_din_ack: output logic;
  port ln_dout: output logic[2];
  port ln_dout_ack: input logic;
};
component DEV: dev_type;
export DEV;

object sys: system;
  sys.simu_cycles(100);

object ln: link2 with datawidth=10;
  ln.interface(DEV.ln_din,DEV.ln_din_ack,DEV.ln_dout,DEV.ln_dout_ack);
  
reg x,y: int[10];
reg xa,ya: int[8];
export x,y,xa,ya;

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
end;

module L_simu:
begin
  --
  -- Instantiate components, import
  -- THIS toplevel module.
  --
  import L;
  component Lc1,Lc2: L;
  
  --
  -- Structural Interconnection
  --
  type l_connect: {
    port Lc1_ln_din: output logic[2];
    port Lc1_ln_din_ack: input logic;
    port Lc1_ln_dout: input logic[2];
    port Lc1_ln_dout_ack: output logic;
    port Lc2_ln_din: output logic[2];
    port Lc2_ln_din_ack: input logic;
    port Lc2_ln_dout: input logic[2];
    port Lc2_ln_dout_ack: output logic;
  };
  component L_c: l_connect :=
                  {Lc1.DEV.ln_din,
                   Lc1.DEV.ln_din_ack,
                   Lc1.DEV.ln_dout,
                   Lc1.DEV.ln_dout_ack,
                   Lc2.DEV.ln_din,
                   Lc2.DEV.ln_din_ack,
                   Lc2.DEV.ln_dout,
                   Lc2.DEV.ln_dout_ack};
  --
  -- Interconnect
  --
  L_c.Lc1_ln_din << L_c.Lc2_ln_dout;
  L_c.Lc1_ln_din_ack >> L_c.Lc2_ln_dout_ack;
  L_c.Lc1_ln_dout >> L_c.Lc2_ln_din;
  L_c.Lc1_ln_dout_ack << L_c.Lc2_ln_din_ack;
end;


