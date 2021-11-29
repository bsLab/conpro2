open Core;
open Process;
open System;
open Timer;
open Lcd;
open Mutex;
open Random;

open System;


const simulation: value := false;
reg version: logic[8];

object sys : system;
if simulation = false then
begin
  sys.clock  (18500 kilohz);
  sys.target ("xc3s1000-ft256-5");
  --sys.target ("xcf04s");
  sys.target ("xc18v04");
  sys.reset_level (0);
end
else
begin
  sys.simu_cycles(300);
end;

type dev_type : {
  port leds: output logic[4];
  port lcd_db: inout logic[4];
  port lcd_rw: output logic;
  port lcd_rs: output logic;
  port lcd_en: output logic;
  port lcd_reset: output logic;
};

component dev: dev_type;
export dev;


reg stat_a,stat_b,stat_c: logic;
reg stat_leds,stat_ev: logic[4];
dev.leds << stat_leds;


const lines: value := 3;
const chars_per_line: value := 16;

object lcd1: lcd with mode=4 and controller=st7036 and lines=3;
  lcd1.interface(dev.lcd_db,dev.lcd_rw,dev.lcd_en,dev.lcd_rs,dev.lcd_reset);
object print_lock: mutex with model=group;

object watch_timer: timer;
  watch_timer.time (300 millisec);

object rnd1: random with datawidth=8;

array line: reg[32] of logic[8];
--
-- Print Utilities
--
function clear ():
begin
  lcd1.cmd(0b00000001);
end with inline;

function print (linenum:logic[2]):
begin
  reg i:logic[5];
  reg eol: bool;
  stat_b <- 1;
  i <- 0;
  eol <- false;
  
  match linenum with
  begin
    when 1: lcd1.cmd(0x80);
    when 2: lcd1.cmd(0x80+0x10);
    when 3: lcd1.cmd(0x80+0x20);
  end;
  
  while eol = false do
  begin
    lcd1.write(line.[i]);
    eol <- line.[i] = '.';
    i <- i + 1;
  end;
  print_lock.unlock ();
end;

function init ():
begin
  lcd1.init ();
  
  -- function set
  lcd1.cmd (0x29);
  -- bias set
  lcd1.cmd (0x15);
  -- power control
  lcd1.cmd (0x55);
  -- follower control
  lcd1.cmd (0x6e);
  -- contrast set
  lcd1.cmd (0x72);
  -- function set
  lcd1.cmd (0x28);
  -- display on
  lcd1.cmd (0b00001100);
  -- clear
  lcd1.cmd (0x01);
  -- entry mode set
  lcd1.cmd (0x06);

  print_lock.lock ();
  line <- "READY VXX.";
  if version[4 to 7] < 10 then 
    line.[7] <- '0' + version[4 to 7]
  else 
    line.[7] <- 'A' + version[4 to 7] - 10;    
  if version[0 to 3] < 10 then 
    line.[8] <- '0' + version[0 to 3]
  else 
    line.[8] <- 'A' + version[0 to 3] - 10;    
  print(1);

  stat_a <- 1;

end with inline;

process watch_set:
begin
  stat_ev <- 0;
  stat_a <- 0;
  stat_b <- 0;
  stat_c <- 0;
  
  watch_timer.start ();

  always do
  begin
    if  (stat_a = 1) then
    begin
        stat_ev[0] <- 1;
        stat_a <- 0;  
    end;
    if stat_b = 1 then
    begin               
        stat_ev[1] <- 1;
        stat_b <- 0;
    end;              
    if stat_c = 1 then
    begin               
        stat_ev[2] <- 1;
        stat_c <- 0;
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


object long_timer: timer;
  long_timer.time (1000 millisec);

process p1:
begin
  reg t: logic[8];
  reg s: bool;
  reg err: bool;
 
  long_timer.init ();
  long_timer.start ();
  s <- false;
  for i = 0 to 15 do
  begin
    long_timer.await ();
    print_lock.lock ();
    line <- "INDEX: X.";
    if i < 10 then 
      line.[7] <- '0' + to_logic(i)
    else 
      line.[7] <- 'A' + to_logic(i) - 10;    
    print(2);
  end;
end;

process p2:
begin
  reg t,tv: logic[8];
  reg s: bool;
  reg err: bool;
 
  long_timer.init ();
  long_timer.start ();
  s <- false;
  for i = 0 to 15 do
  begin
    long_timer.await ();
    long_timer.await ();

    print_lock.lock ();
    line <- "VALUE: XY.";
    rnd1.read(t);
    
    
    if t[4 to 7] < 10 then 
      line.[7] <- '0' + t[4 to 7]
    else 
      line.[7] <- 'A' + t[4 to 7] - 10;    
    if t[0 to 3] < 10 then 
      line.[8] <- '0' + t[0 to 3]
    else 
      line.[8] <- 'A' + t[0 to 3] - 10;    
    print(3);
  end;
end;

process main:
begin
  version <- 0xA0;

  stat_a <- 0;
  stat_b <- 0;
  stat_c <- 0;

  watch_timer.init ();
  watch_set.start ();
  watch_reset.start ();
  
  print_lock.init ();

  rnd1.init ();
  
  init ();

  p1.start ();
  p2.start ();
    
end;
