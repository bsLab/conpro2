open Core;
open Process;
open Uart;
open System;
open Event;
open Timer;

const REQ: value := 'R';
const REP: value := 'P';


const CLOCK: value := 20;
object sys : system;
if CLOCK = 50 then
begin
  sys.clock  (50000 kilohz);
end;
if CLOCK = 20 then
begin
  sys.clock  (18500 kilohz);
end;
  -- sys.clock  (20000 kilohz);
  -- sys.clock  (44236 kilohz);
  sys.target ("xc3s1000-ft256-5");
  --sys.target ("xcf04s");
  sys.target ("xc18v04");
  sys.reset_level (0);



reg stat_led: logic;
export stat_led;

type status_types : {
  STATUS_OK;
  STATUS_ERR; 
  STATUS_ACT;
  STATUS_EV;
  STATUS_DOWN;
};



reg sys_status: status_types;
reg sys_status_next: status_types;

object watch_timer: timer;
  watch_timer.time (50 millisec);

--
-- Diagnostics
--

process sys_status_proc:
begin
  reg counter: int[6];
  reg onl:  bool;
  reg last_status:status_types;
  stat_led <- 0b0;
  
  counter <- 0, onl <- false;
  
    
  watch_timer.init ();
  watch_timer.start ();

  last_status <- STATUS_DOWN;
  sys_status_next <- last_status;
  
  always do
  begin
    last_status <- sys_status_next;

    match sys_status with
    begin
      when STATUS_OK: 
      begin
        onl <- true, counter <- 0,
        last_status <- STATUS_OK;
      end;
      when STATUS_DOWN: 
      begin
        onl <- false, counter <- 0,
        last_status <- STATUS_DOWN;
      end;
      when STATUS_ERR:
      begin
        if onl = false and counter = 0 then 
        begin
          onl <- true, counter <- 2,
          last_status <- STATUS_ERR;
        end 
        else if onl = true and counter = 0 then
        begin
          onl <- false, counter <- 2;
        end
        else counter <- counter - 1;  
      end;
      when STATUS_ACT:
      begin
        if onl = false and counter = 0 then 
        begin
          onl <- true, counter <- 6;
          last_status <- STATUS_ACT;
        end 
        else if onl = true and counter = 0 then
        begin
          onl <- false, counter <- 6;
        end
        else counter <- counter - 1;  
      end;
      when STATUS_EV:
      begin
        if counter = 0 then 
        begin
          onl <- true, counter <- 3;
        end 
        else if counter = 1 then
        begin
          counter <- 0;
          sys_status <- last_status;
        end
        else 
        begin
          onl <- not onl;
          counter <- counter - 1;  
        end;
      end;
    end;
    sys_status_next <- last_status;
    if onl = true then stat_led <- 1 else stat_led <- 0;
    watch_timer.await ();
  end;
end;

type dev_io_t : {
  port RX: input logic;
  port TX: output logic;
};
component dev_io: dev_io_t;
export dev_io;

object com: uart;
  com.baud(115200);
  com.interface(dev_io.RX,dev_io.TX);


reg rep_d: logic[8];
object rep: event;

reg com_timeout: bool;
queue tx_q,rx_q: logic[8] with depth=16;
  
process com_tmo:
begin
  com_timeout <- false;
  wait for 10 millisec;
  rx_q.unlock ();
  com_timeout <- true;
end;


process com_rx:
begin
  reg d,d2: logic[8];
  reg err: bool;
  err <- false;
  
  while err = false do
  begin
    com.read(d,err);
    sys_status <- STATUS_EV;
    if err = false then rx_q <- d;
  end;
end;

process com_tx:
begin
  reg d: logic[8];
  reg err: bool;

  err <- false;
  
  while err = false do 
  begin
    d <- tx_q;
    com.write (d,err);
  end;  
end;

process interpreter:
begin
  reg d: logic[8];
  
  com.init ();
  com.start ();
  com_rx.start ();
  com_tx.start ();
  
  sys_status <- STATUS_OK;  
  
  always do
  begin
    d <- rx_q;
    sys_status_next <- STATUS_OK;
    match d with
    begin
      when REQ:
      begin
        com_tmo.start ();
        d <- rx_q;
        com_tmo.stop ();
        if com_timeout = false then 
        begin
          tx_q <- REP;
          tx_q <- d;
        end else sys_status <- STATUS_ERR;
      end;
      when REP: 
      begin
        com_tmo.start ();
        d <- rx_q;
        com_tmo.stop ();
        if com_timeout = false then 
        begin
          rep_d <- d;
          rep.wakeup ();
        end else sys_status <- STATUS_ERR;
      end;
      when others:
      begin
        sys_status <- STATUS_ERR;
      end;
    end;
  end;
end;


function request(d1: logic[8]) return (d2: logic[8]):
begin
  tx_q <- REQ;
  tx_q <- d1;
  rep.await ();
  d2 <- rep_d;
end;

process main:
begin
  reg d:logic[8];
  sys_status_proc.start ();
  rep.init ();
  interpreter.start ();
  d<-request('x');
end;
