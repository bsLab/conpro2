open Core;
open Process;
open Uart;
open System;
open Timer;
open Lcd;
open Mutex;

open System;


const simulation: value := false;
reg version: logic[8];
reg count: logic[16];


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
  port rx: input logic;
  port tx: output logic;
  port lcd_db: inout logic[4];
  port lcd_rw: output logic;
  port lcd_rs: output logic;
  port lcd_en: output logic;
  port lcd_reset: output logic;
};

component dev: dev_type;
export dev;

reg stat_leds,stat_ev: logic[4];
reg stat_req,stat_err,stat_rx: logic;
dev.leds << stat_leds;

reg d: logic[8];
reg line_pos: logic[5];


queue q_u1_rx,q_u1_tx: logic[8] with depth=16;
queue q_lcd1_tx: logic[9] with depth=64;

object u1: uart;
  u1.baud (115200);
  u1.interface(dev.rx,dev.tx);
  
object watch_timer: timer;
  watch_timer.time (300 millisec);

object lcd1: lcd with mode=4 and controller=st7036 and lines=3;
  lcd1.interface(dev.lcd_db,dev.lcd_rw,dev.lcd_en,dev.lcd_rs,dev.lcd_reset);
  const chars_per_line: value := 16;
  
object print_lock: mutex with model=group;

array line: var[32] of logic[8];


--
-- LCD display
--

process lcd1_tx:
begin
  reg t:logic[9];
  always do
  begin
    t <- q_lcd1_tx;
    if t[8] = 1 then
      lcd1.cmd(t[0 to 7])
    else
      lcd1.write(t[0 to 7]);
  end;
end;


--
-- Print Utilities
--

function init ():
begin
  line_pos <- 0;
  version <- 0xA1;
  print_lock.init ();  

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
  lcd1.cmd (0x70);
  -- function set
  lcd1.cmd (0x28);
  -- display on
  lcd1.cmd (0b00001100);
  -- clear
  lcd1.cmd (0x01);
  -- entry mode set
  lcd1.cmd (0x06);

  lcd1_tx.start ();

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


  
  u1.baud(38400);
  u1.init ();
  u1.start ();

  u1_rx.start ();
  u1_tx.start ();


end with inline;

function clear ():
begin
  q_lcd1_tx <- 0x101;
end with inline;

function print (linenum:logic[2]):
begin
  reg i:logic[5];
  reg eol: bool;
  i <- 0;
  eol <- false;
  
  match linenum with
  begin
    when 1: q_lcd1_tx <- 0x180;
    when 2: q_lcd1_tx <- (0x180+0x10);
    when 3: q_lcd1_tx <- (0x180+0x20);
  end;
  
  while eol = false do
  begin
    q_lcd1_tx <- line.[i];
    eol <- line.[i] = '.';
    i <- i + 1;
  end;
  print_lock.unlock ();
end;

function print_val(v:logic[8]):
begin
  print_lock.lock ();
  count <- count + 1;
  line <- "VALUE XX #YYYY.";
  if v[4 to 7] < 10 then 
    line.[6] <- '0' + v[4 to 7]
  else 
    line.[6] <- 'A' + v[4 to 7] - 10;    
  if v[0 to 3] < 10 then 
    line.[7] <- '0' + v[0 to 3]
  else 
    line.[7] <- 'A' + v[0 to 3] - 10;    
  if count[12 to 15] < 10 then 
    line.[10] <- '0' + count[12 to 15]
  else 
    line.[10] <- 'A' + count[12 to 15] - 10;    
  if count[8 to 11] < 10 then 
    line.[11] <- '0' + count[8 to 11]
  else 
    line.[11] <- 'A' + count[8 to 11] - 10;    
  if count[4 to 7] < 10 then 
    line.[12] <- '0' + count[4 to 7]
  else 
    line.[12] <- 'A' + count[4 to 7] - 10;    
  if count[0 to 3] < 10 then 
    line.[13] <- '0' + count[0 to 3]
  else 
    line.[13] <- 'A' + count[0 to 3] - 10;    
  print(2);
  
end;


function print_line(v:logic[8]):
begin

  print_lock.lock ();
  q_lcd1_tx <- 0x180+0x20+line_pos;
  line_pos <- line_pos + 1;
  if line_pos = chars_per_line then
    line_pos <- 0;
  q_lcd1_tx <- v;
  print_lock.unlock ();
      
end;

--
-- UART communication
--
process u1_rx:
begin
  reg err: bool;
  reg t:logic[8];
  
  always do
  begin
    u1.read(t,err);
    if err = true then stat_err <- 1;
    if err = false then q_u1_rx <- t;
  end;
end;

process u1_tx:
begin
  reg err: bool;
  reg t:logic[8];
  
  err <- false;
  always do
  begin
    if err = false then 
      t <- q_u1_tx;
    u1.write(t,err);
  end;
end;


--
-- Diagnostics
--

process watch_set:
begin
  reg rx_last: logic;
  
  stat_ev <- 0;
  stat_req <- 0;
  stat_err <- 0;
  stat_rx <- dev.rx;
  
  watch_timer.start ();

  always do
  begin
    if  (stat_req = 1) then
    begin
        stat_ev[0] <- 1;
        stat_req <- 0;
    end;
    if stat_err = 1 then
    begin
        stat_ev[1] <- 1;
        stat_err <- 0;
    end;
    if stat_rx <> dev.rx  then
    begin
        stat_rx <- dev.rx; 
        stat_ev[2] <- 1;
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



process p1:
begin
  reg t: logic[8];

  always do
  begin
    t <- q_u1_rx;

    print_val(t);
    print_line(t);
    
    if t = 'Z' then 
    begin
      d <- t - 1;
      stat_req <- 1;
    end
    else
    begin
      d <- t + 1;
      stat_req <- 1;
    end;
    q_u1_tx <- d;      
  end;
end;

process main:
begin
  init ();

  watch_timer.init ();
  
  watch_set.start ();
  watch_reset.start ();

  p1.start ();
  
end;
