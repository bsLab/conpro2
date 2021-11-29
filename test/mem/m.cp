open Core;
open Process;
open Uart;
open System;
open Timer;
open Lcd;
open Mutex;
open Mem;

open System;
open Event;

const core_ram: value := false;
const simulation: value := false;
reg version: logic[8];
reg count: logic[16];


object sys : system;
if simulation = false then
begin
  sys.clock  (18500 kilohz);
  -- sys.clock  (20000 kilohz);
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
  port ram_db: bus logic[8];
  port ram_addr: inout logic[19];
  port ram_we: output logic;
  port ram_re: output logic;
  port ram_cs: output logic;
  port eth_db: bus logic[8];
  port eth_addr: output logic[8];
  port eth_we: output logic;
  port eth_re: output logic;
  port eth_cs: output logic;
  port eth_rst: bus logic;
  port eth_int: input logic;
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
  version <- 0xC0;
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

function print_req(k:logic[8],v:logic[8]):
begin
  print_lock.lock ();
  count <- count + 1;
  line <- "REQ X YY #ZZZZ.";
  line.[4] <- k;
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
  print(3);
  
end;

function print_kb(k:logic[8],v:logic[12]):
begin
  print_lock.lock ();
  line <- "MEM X YYY kB.";
  line.[4] <- k;
  if v[8 to 11] < 10 then 
    line.[6] <- '0' + v[8 to 11]
  else 
    line.[6] <- 'A' + v[8 to 11] - 10;    
  if v[4 to 7] < 10 then 
    line.[7] <- '0' + v[4 to 7]
  else 
    line.[7] <- 'A' + v[4 to 7] - 10;    
  if v[0 to 3] < 10 then 
    line.[8] <- '0' + v[0 to 3]
  else 
    line.[8] <- 'A' + v[0 to 3] - 10;    
  print(3);
  
end;

function print_mem_err(addr:logic[19]):
begin
  print_lock.lock ();
  line <- "MEM ERR AAAAA.";

  line.[8] <- '0' + addr[16 to 18];
  if addr[12 to 15] < 10 then 
    line.[9] <- '0' + addr[12 to 15]
  else 
    line.[9] <- 'A' + addr[12 to 15] - 10;    
  if addr[8 to 11] < 10 then 
    line.[10] <- '0' + addr[8 to 11]
  else 
    line.[10] <- 'A' + addr[8 to 11] - 10;    
  if addr[4 to 7] < 10 then 
    line.[11] <- '0' + addr[4 to 7]
  else 
    line.[11] <- 'A' + addr[4 to 7] - 10;    
  if addr[0 to 3] < 10 then 
    line.[11] <- '0' + addr[0 to 3]
  else 
    line.[11] <- 'A' + addr[0 to 3] - 10;    
  print(3);
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
-- Ethernet Controller CP2200
--
object eth: mem with 
    fsm=1 and
    datawidth=8 and 
    addrwidth=8 and 
    control=w0 and 
    control=r0 and 
    control=c0 and
    waitstates=8;
  eth.interface(dev.eth_db,dev.eth_addr,dev.eth_re,dev.eth_we,dev.eth_cs);

const ETH_INT0: value := 0x63;
  const ETH_RXINT: value := 0x01;
  const ETH_RXFINT: value := 0x02;
  const ETH_TXINT: value := 0x04;
  const ETH_FLWEINT: value := 0x08;
  const ETH_OSCINT: value := 0x10;
  const ETH_SELFINT: value := 0x20;
  const ETH_SELFINT_BIT: value := 5;
  const ETH_RXEINT: value := 0x40;
  const ETH_EOPINT: value := 0x80;
  
const ETH_INT0EN: value := 0x64;
const ETH_IOPWR: value := 0x70;
const ETH_INT1EN: value := 0x7d;

const ETH_RSTEN: value := 0x72;
const ETH_RSTSTA: value := 0x73;
const ETH_INT0RD: value := 0x76;
const ETH_PHYCN: value := 0x78;
  const ETH_LINKSTA: value := 0x01;
  const ETH_POLREV: value := 0x02;
  const ETH_LPRFAULT: value := 0x04;
  const ETH_LBMD: value := 0x08;
  const ETH_DPLXMD: value := 0x10;
  const ETH_RXEN: value := 0x20;
  const ETH_TXEN: value := 0x40;
  const ETH_PHYEN: value := 0x80;
const ETH_PHYCF: value := 0x79;
  const ETH_REVPOL: value := 0x01;
  const ETH_AUTOPOL: value := 0x02;
  const ETH_ADPAUSE: value := 0x04;
  const ETH_AUTONEG: value := 0x10;
  const ETH_JABBER: value := 0x20;
  const ETH_LINKINTG: value := 0x40;
  const ETH_SMSQ: value := 0x80;
const ETH_PHYSTA: value := 0x80;

const ETH_TXPWR: value := 0x7A;
const ETH_INT1RD: value := 0x7e;
const ETH_INT1: value := 0x7f;
  const ETH_ANCINT: value := 0x01;
  const ETH_ANCINT_BIT: value := 0;
  const ETH_ANFINT: value := 0x04;
  const ETH_ANFINT_BIT: value := 2;
  const ETH_JABINT: value := 0x08;
  const ETH_LINKINT: value := 0x10;
  const ETH_WAKEINT: value := 0x20;
  const ETH_WAKEINT_BIT: value := 5;


 

object eth_ev: event with latch=1;
reg eth_int0,eth_int1,eth_rststa,eth_physta: logic[8];
reg eth_irq_count: logic[8];
reg eth_stat: logic[8];
reg eth_irq_stat: logic[8];
reg eth_link_stat: logic[8];

process eth_info:
begin
  reg r: logic[8];
  reg o: logic[4];
  

  always do
  begin
    watch_timer.await ();
    print_lock.lock ();
    line <- "PXX I0 I1 RS IC.";
    eth.read(r,ETH_PHYSTA);
    if r[4 to 7] < 10 then 
      line.[1] <- '0' + r[4 to 7]
    else 
      line.[1] <- 'A' + r[4 to 7] - 10;    
    if r[0 to 3] < 10 then 
      line.[2] <- '0' + r[0 to 3]
    else 
      line.[2] <- 'A' + r[0 to 3] - 10;  

    print(1);
    print_lock.lock (); 
    line <- "S-- XX XX XX XX.";
    o <- 0;
    line.[0] <- eth_stat;
    line.[1] <- eth_irq_stat;
    line.[2] <- eth_link_stat;
    for i = 1 to 4 do
    begin
      match i with
      begin
        when 1: r <- eth_int0;
        when 2: r <- eth_int1;
        when 3: eth.read(r,ETH_RSTSTA); 
        when 4: r <- eth_irq_count;
      end;
      if r[4 to 7] < 10 then 
        line.[4+o] <- '0' + r[4 to 7]
      else 
        line.[4+o] <- 'A' + r[4 to 7] - 10;    
      if r[0 to 3] < 10 then 
        line.[5+o] <- '0' + r[0 to 3]
      else 
        line.[5+o] <- 'A' + r[0 to 3] - 10;  
      o <- o + 3;
    end;
    print(2);
  end;
end;


process eth_irq:
begin
  reg r0,r1: logic[8];
  eth_int0 <- 0;
  eth_int1 <- 0;
  
  always do
  begin
    eth_irq_stat <- '0';
    wait for dev.eth_int = 0;
    while dev.eth_int = 0 do
    begin
      eth.read(r0,ETH_INT0);
      if r0 <> 0 then
        eth_int0 <- r0;
      eth.read(r1,ETH_INT1);
      if r1 <> 0 then
        eth_int1 <- r1;
      if r0 <> 0 or r1 <> 0 then
      begin
        eth_ev.wakeup ();
        eth_irq_count <- eth_irq_count + 1;
      end;
      eth_irq_stat <- '1';
      wait for 1 microsec;
    end;
  end;
end;

const phy_init : value := false;

object eth_res_timer: timer;
  eth_res_timer.time(20 microsec);
  eth_res_timer.mode(1);
  eth_res_timer.sig_action(dev.eth_rst,0,2);
  
signal eth_rst: logic;
  eth_rst << dev.eth_rst;

type eth_states: {
  ETH_I0;
  ETH_I1;
  ETH_I2;
  ETH_I3;
  ETH_I4;
  ETH_P0;
  ETH_P1;
  ETH_S0;
  ETH_F0;
};
reg eth_state: eth_states; 
reg eth_timeout: bool;
process eth_tmo:
begin
  eth_timeout <- false;
  wait for 1 sec;
  eth_timeout <- true;
  eth_ev.wakeup ();

end;

process eth_svr:
begin
  reg r:logic[8];
  reg n:int[5];
  
  eth_res_timer.init ();
  eth_state <- ETH_I0;  

  always do
  begin
    match eth_state with
    begin
      when ETH_I0:
      begin
        eth_link_stat <- '?';
        eth_stat <- 'R';
        --
        -- Reset PIN
        --
        eth_res_timer.start ();
        eth_res_timer.await ();
        eth_state <- ETH_I1;
      end; 
      when ETH_I1:
      begin
        eth_stat <- 'r';
        wait for eth_rst = 1;
        eth_state <- ETH_I2;
      end;
      when ETH_I2:
      begin
        eth_stat <- 'o';

        --
        -- Wait for Osc. complete
        --

        wait for dev.eth_int = 0;
        
        eth_state <- ETH_I3;
      end;
      when ETH_I3:
      begin
        --
        --  Wait for Init. complete
        --
        eth_stat <- 'O';

        eth.read(eth_int0,ETH_INT0RD);
        while eth_int0[ETH_SELFINT_BIT] = 0 do
        begin
          eth.read(eth_int0,ETH_INT0RD);
        end;
        
        eth_state <- ETH_I4;      
      end;
      when ETH_I4:
      begin
        eth_stat <- 'i';
        eth_ev.init ();
        eth_irq.start ();
        eth_stat <- 'I';
        eth_ev.await ();
        
        --
        -- Disable all interrupt sources actually not required
        --
        eth.write(0x30,ETH_INT0EN);
        eth.write(0x3D,ETH_INT1EN);
        
        eth_state <- ETH_P0;
      end;
      when ETH_P0:
      begin
        --
        -- PHY initialization
        --
        eth_link_stat <- '*';
        eth_stat <- 'P';

        eth.write(0x00,ETH_PHYCN);
        eth.write(0x80,ETH_TXPWR);
        eth.write(ETH_SMSQ lor ETH_JABBER lor ETH_ADPAUSE lor ETH_AUTOPOL,ETH_PHYCF);
        eth.write(ETH_PHYEN,ETH_PHYCN);
        wait for 10 millisec;
        eth.write(ETH_PHYEN lor ETH_TXEN lor ETH_RXEN,ETH_PHYCN);
  
        n <- 0;
        while eth_int1[ETH_WAKEINT_BIT] = 0 and n <> 2 do
        begin
          eth_tmo.start ();
          eth_ev.await ();
          eth_tmo.stop ();
          if eth_timeout = true then 
          begin
            n <- n + 1;
            eth_stat <- '0' + to_logic(n);
          end;
        end;
        
        if eth_int1[ETH_WAKEINT_BIT] = 1 then
        begin
          eth_link_stat <- 'W';
          wait for 250 microsec;
        end;
        eth_state <- ETH_P1;
      end;
      when ETH_P1:
      begin
        eth_stat <- 'A';
        --
        -- Disable WAKEINT
        --
        eth.write(0x00,ETH_INT0EN);
        eth.write(0x1D,ETH_INT1EN);
        
        eth.write(0x00,ETH_PHYCN);
        
        eth.write(ETH_SMSQ lor ETH_LINKINTG lor ETH_JABBER lor ETH_AUTONEG lor ETH_ADPAUSE lor ETH_AUTOPOL,ETH_PHYCF);
        eth.write(ETH_PHYEN,ETH_PHYCN);
        wait for 10 millisec;
        eth.write(ETH_PHYEN lor ETH_TXEN lor ETH_RXEN,ETH_PHYCN);


        n <- 0;
        
        while (eth_int1[ETH_ANCINT_BIT] = 0 and eth_int1[ETH_ANFINT_BIT] = 0) and n <> 6 do
        begin
          eth_tmo.start ();
          eth_ev.await ();
          eth_tmo.stop ();
          if eth_timeout = true then 
          begin
            n <- n + 1;
            eth_stat <- '0' + to_logic(n);
          end;
        end;
         
        --
        -- Enable LINK and Activity LEDs
        --
        eth.write(0x0c,ETH_IOPWR);

        if eth_int1[ETH_ANCINT_BIT] = 1 then
        begin
          -- Auto-Negotiation has passed
          eth_link_stat <- 'L';
          eth_state <- ETH_S0;
        end
        else
        begin
          if eth_int1[ETH_ANFINT_BIT] = 1 then
            eth_link_stat <- '!'; -- Auto-Negotiation has failed
          eth_state <- ETH_F0;
        end; 
      end;
      when ETH_S0:
      begin
        eth_stat <- '#';
        
        eth_ev.await ();         
      end;
      when ETH_F0:
      begin
        eth_stat <- '!'; 
        eth_ev.await ();        
      end;
    end; 
  end;
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



--
-- Register dual port RAM
--
if core_ram = true then
begin
  const RU_SIZE: value := 256;
  block ru_ram with model=dualport;
  array ru: var[RU_SIZE] of logic[12] in ru_ram;
end
else
begin
  object ru: mem with 
    datawidth=8 and 
    addrwidth=19 and 
    control=w0 and 
    control=r0 and 
    control=c0 and
    waitstates=0;
  ru.interface(dev.ram_db,dev.ram_addr,dev.ram_re,dev.ram_we,dev.ram_cs);
end;

--
-- Conversion functions
--

function char_to_num(x:logic[8]) return (y:logic[8]):
begin
  if x >= '0' and  x <= '9' then
    y <- x - '0' 
  else if x >= 'A' and x <= 'F' then
    y <- x - 'A' + 10
  else 
    y <- 0;
end;

function char_of_num(x:logic[4]) return (y:logic[8]):
begin
  if x <= 9 then
    y <- x + '0'
  else 
    y <- x + 'A' - 10;
end;

-- UART client
process interp:
begin
  reg data_c,addr,data_x,data_x2: logic[8];
  reg data_12: logic[12];
  reg data_8: logic[8];


  always do
  begin
    data_c <- q_u1_rx;
    match data_c with
    begin
        when 'Z':
        begin
            stat_req <- 1;  
            q_u1_tx <- 'O'; q_u1_tx <- 'K'; q_u1_tx <- '.';
        end;
        when 'R':
        begin
            stat_req <- 1;
            -- get address bytes
            addr <- 0;
            for i = 1 to 2 do
            begin
                data_c <- q_u1_rx;
                data_x <- char_to_num(data_c);
                addr <- addr lsl 4;
                addr <- addr + data_x;
            end;
            print_req('R',addr);
            if core_ram = true then
            begin
            
              -- send data bytes
              data_12 <- ru.[addr];
              for i = 1 to 3 do    
              begin
                  data_c <- char_of_num(data_12[8 to 11]);
                  q_u1_tx <- data_c;  
                  data_12 <- data_12 lsl 4;
              end;
            end
            else
            begin
              ru.read(data_8,addr);
              for i = 1 to 2 do    
              begin
                  data_c <- char_of_num(data_8[4 to 7]);
                  q_u1_tx <- data_c;  
                  data_8 <- data_8 lsl 4;
              end;
            end;            
        end;
        when 'W':
        begin
            stat_req <- 1;
            -- get address bytes
            addr <- 0;
            data_x2 <- 0;
            for i = 1 to 2 do
            begin
                data_c <- q_u1_rx;
                data_x <- char_to_num(data_c);
                addr <- addr lsl 4;
                addr <- addr + data_x;
            end;
            print_req('W',addr);
            -- get data bytes
            if core_ram = true then
            begin
              data_12 <- 0;
              for i = 1 to 3 do
              begin
                  data_c <- q_u1_rx;
                  data_x <- char_to_num(data_c);
                  data_12 <- data_12 lsl 4;
                  data_12 <- data_12 + data_x;
              end;
              ru.[addr] <- data_12;
            end
            else
            begin
              data_8 <- 0;
              for i = 1 to 2 do
              begin
                  data_c <- q_u1_rx;
                  data_x <- char_to_num(data_c);
                  data_8 <- data_8 lsl 4;
                  data_8 <- data_8 + data_x;
              end;
              ru.write(data_8,addr);
            end;
            q_u1_tx <- 'O'; q_u1_tx <- 'K'; q_u1_tx <- '.';
        end;
        when 'r':
        begin
            stat_req <- 1;
            -- get address bytes
            addr <- 0;
            for i = 1 to 2 do
            begin
                data_c <- q_u1_rx;
                data_x <- char_to_num(data_c);
                addr <- addr lsl 4;
                addr <- addr + data_x;
            end;
            print_req('r',addr);
            eth.read(data_8,addr);
            for i = 1 to 2 do    
            begin
                data_c <- char_of_num(data_8[4 to 7]);
                q_u1_tx <- data_c;  
                data_8 <- data_8 lsl 4;
            end;
        end;
        when 'w':
        begin
            stat_req <- 1;
            -- get address bytes
            addr <- 0;
            data_x2 <- 0;
            for i = 1 to 2 do
            begin
                data_c <- q_u1_rx;
                data_x <- char_to_num(data_c);
                addr <- addr lsl 4;
                addr <- addr + data_x;
            end;
            print_req('w',addr);
            -- get data bytes
            data_8 <- 0;
            for i = 1 to 2 do
            begin
                data_c <- q_u1_rx;
                data_x <- char_to_num(data_c);
                data_8 <- data_8 lsl 4;
                data_8 <- data_8 + data_x;
            end;
            eth.write(data_8,addr);
            q_u1_tx <- 'O'; q_u1_tx <- 'K'; q_u1_tx <- '.';
        end;
        when others:
        begin
            stat_err <- 1;   
            q_u1_tx <- 'E'; q_u1_tx <- 'R'; q_u1_tx <- 'R'; 
        end;
    end;
  end;  
end;

process test:
begin
  reg x,y: logic[8];
  reg addr: logic[19];
  reg kb: logic[12];
  
  x <- 3;
  kb <- 0;
  
  for i = 0 to 524287 do
  begin
    addr <- to_logic(i);
    ru.write(x,addr);
    x <- x + 7;
    if addr[0 to 9] = 0 then
    begin
      kb <- kb + 1;
      print_kb('W',kb);
    end;
  end;
  
  x <- 3;
  kb <- 0;
  
  for i = 0 to 524287 do
  begin
    addr <- to_logic(i);
    ru.read(y,addr);
    if x <> y then
    begin
      print_mem_err(addr);  
    end;
    x <- x + 7;
    if addr[0 to 9] = 0 then
    begin
      kb <- kb + 1;
      print_kb('R',kb);
    end;
  end;
  
end;


process main:
begin
  init ();

  watch_timer.init ();
  
  watch_set.start ();
  watch_reset.start ();

  interp.start ();
  test.start ();
  eth_info.start ();
  eth_svr.start ();
end;
