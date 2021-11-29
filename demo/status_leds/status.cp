open Core;
open Process;
open Timer;
open System;
open Random;


object sys : system;
  sys.clock  (18500 kilohz);
  -- sys.clock  (20000 kilohz);
  sys.target ("xc3s1000-ft256-5");
  --sys.target ("xcf04s");
  sys.target ("xc18v04");
  sys.reset_level (0);

type dev_type : {
  port leds: output logic[4];
};

component DEV: dev_type;
export DEV;

reg stat_leds,stat_ev: logic[4];
reg diag: logic[3];
DEV.leds << stat_leds;

object watch_timer: timer;
  watch_timer.time (300 millisec);

object rand: random; -- with seed=1234;

--
-- Diagnostics
--

process watch_set:
begin
  diag <- 0;
    
  watch_timer.init ();
  watch_timer.start ();

  always do
  begin
    if  diag[0] = 1 then
    begin
        stat_ev[0] <- 1;
        diag[0] <- 0;
    end;
    if  diag[1] = 1 then
    begin
        stat_ev[1] <- 1;
        diag[1] <- 0;
    end;
    if  diag[2] = 1 then
    begin
        stat_ev[2] <- 1;
        diag[2] <- 0;
    end;
  end;
end;

process watch_reset:
begin
  stat_leds <- 0b0000;
  always do
  begin
    for i = 1 to 2 do
    begin
        match i with
        begin
          when 1:
            begin
                stat_leds[0 to 2] <- stat_ev;
                stat_leds[3] <- 1;
                stat_ev <- 0;
            end;
          when 2:
            begin
                stat_leds <- 0b0000;
            end;
        end;
        watch_timer.await ();
    end;
  end;
end;

process main:
begin
  reg d: logic[8];
  watch_set.start ();
  watch_reset.start ();
  rand.init ();
  
  always do
  begin
    rand.read (d);
    match d with
    begin
      when 0 to 16: diag[0] <- 1;
      when 17 to 45: diag[1] <- 1;
      when others: diag[2] <- 1;
    end;
    wait for 3 sec;    
  end;
end;
