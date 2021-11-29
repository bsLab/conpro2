open Core;
open Process;
open Link4;
open System;

type dev_type:{
  port ln_din: input logic[8];
  port ln_din_ack: output logic;
  port ln_dout: output logic[8];
  port ln_dout_ack: input logic;
};
component DEV: dev_type;
export DEV;

object sys: system;
  sys.simu_cycles(100);

sig select: logic;

object ln: link4 with datawidth=12 and select=1;
  ln.interface(DEV.ln_din,DEV.ln_din_ack,DEV.ln_dout,DEV.ln_dout_ack);
  ln.select(select);  
  
reg x: int[12];
reg xa: int[8];
export x,xa;

exception Exit;


process p1:
begin
  reg err:bool;
  reg d:logic[12];

  d <- 0x349;
  try
  begin
    for i = 1 to 10 do
    begin
      xa <- 'w',x <- to_int(d);
      ln.write(d,err);
      if err = true then raise Exit;  
      xa <- 'r';
      ln.read (d,err);
      if err = true then raise Exit;  
      x <- to_int(d),d <- d + 1;
    end;
  end
  with
  begin
    when Exit: ln.stop ();
  end;
  xa <- '.';
end;


process main:
begin
  ln.init ();
  ln.start ();
  p1.start ();
end;

module L2_simu:
begin
  --
  -- Instantiate components, import
  -- THIS toplevel module.
  --
  import L2;
  component Lc1,Lc2: L2;
  
  --
  -- Structural Interconnection
  --
  type l_connect: {
    port Lc1_ln_din: output logic[8];
    port Lc1_ln_din_ack: input logic;
    port Lc1_ln_dout: input logic[8];
    port Lc1_ln_dout_ack: output logic;
    port Lc2_ln_din: output logic[8];
    port Lc2_ln_din_ack: input logic;
    port Lc2_ln_dout: input logic[8];
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


