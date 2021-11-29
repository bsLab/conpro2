open Core;
open Process;
open Reset;
open System;

object sys : system;
  sys.clock  (10 megahz);
  sys.sim_res (1 nanosec);

object res: reset;
  res.source ("RESET");
  res.action (1);

type muller_test_shift_2 : {
    port M_load: input logic;
    port M_shift: input logic;
    port M_din: input logic[4];
    port M_qe_0: output logic[2];
    port M_qd_0: output logic[2];
    port M_qe_1: output logic[2];
    port M_qd_1: output logic[2];
    port M_qe_2: output logic[2];
    port M_qd_2: output logic[2];
    port M_qe_3: output logic[2];
    port M_qd_3: output logic[2];
    port M_qe_4: output logic[2];
    port M_qd_4: output logic[2];
    port M_qe_5: output logic[2];
    port M_qd_5: output logic[2];
    port M_qe_6: output logic[2];
    port M_qd_6: output logic[2];
    port M_qe_7: output logic[2];
    port M_qd_7: output logic[2];
    port M_RST: input logic;
};
component mu_test: muller_test_shift_2;
  res.add (mu_test.M_RST);

type top_port : {
    port D_qe_0: output logic[2];
    port D_qd_0: output logic[2];
    port D_qe_1: output logic[2];
    port D_qd_1: output logic[2];
    port D_qe_2: output logic[2];
    port D_qd_2: output logic[2];
    port D_qe_3: output logic[2];
    port D_qd_3: output logic[2];
    port D_qe_4: output logic[2];
    port D_qd_4: output logic[2];
    port D_qe_5: output logic[2];
    port D_qd_5: output logic[2];
    port D_qe_6: output logic[2];
    port D_qd_6: output logic[2];
    port D_qe_7: output logic[2];
    port D_qd_7: output logic[2];
    port D_load: output logic;
    port D_shift: output logic;
};

component top: top_port;
export top;
  top.D_qe_0 << mu_test.M_qe_0;
  top.D_qd_0 << mu_test.M_qd_0;
  top.D_qe_1 << mu_test.M_qe_1;
  top.D_qd_1 << mu_test.M_qd_1;
  top.D_qe_2 << mu_test.M_qe_2;
  top.D_qd_2 << mu_test.M_qd_2;
  top.D_qe_3 << mu_test.M_qe_3;
  top.D_qd_3 << mu_test.M_qd_3;
  top.D_qe_4 << mu_test.M_qe_4;
  top.D_qd_4 << mu_test.M_qd_4;
  top.D_qe_5 << mu_test.M_qe_5;
  top.D_qd_5 << mu_test.M_qd_5;
  top.D_qe_6 << mu_test.M_qe_6;
  top.D_qd_6 << mu_test.M_qd_6;
  top.D_qe_7 << mu_test.M_qe_7;
  top.D_qd_7 << mu_test.M_qd_7;


sig S_shift,S_load: logic;
  mu_test.M_shift << S_shift;
  mu_test.M_load << S_load;
  top.D_load << S_load;
  top.D_shift << S_shift;

reg d: logic[4];
  mu_test.M_din << d;

process p1:
begin
  for i = 0 to 15 do
    d <- i;

  d <- 0x9;
  
  S_load <- 1;
  delay 1;
  apply 2 with S_shift <- 1 else S_shift <- 0;
end;


process main:
begin
  p1.start ();
end;
