--
--      ==================================
--      OOOO   OOOO OOOO  O      O   OOOO
--      O   O  O    O     O     O O  O   O
--      O   O  O    O     O     O O  O   O
--      OOOO   OOOO OOOO  O     OOO  OOOO
--      O   O     O    O  O    O   O O   O
--      O   O     O    O  O    O   O O   O
--      OOOO   OOOO OOOO  OOOO O   O OOOO
--      ================================== 
--      BSSLAB, Dr. Stefan Bosse http://www.bsslab.de
--
--      COPYRIGHT: THIS SOFTWARE, EXECUTABLE AND SOURCE CODE IS OWNED BY BSSLAB.
--                 THIS SOURCE CODE MAY NOT BE COPIED, EXTRACTED,
--                 MODIFIED, OR OTHERWISE USED IN A CONTEXT
--                 OUTSIDE OF THE SOFTWARE SYSTEM.
--
--    $AUTHORS:     Stefan Bosse
--    $INITIAL:     (C) 2007-2010 BSSLAB
--    $CREATED:     6.5.2007
--    $VERSION:     1.03
--
--    $INFO:
--
-- LED diagnostics
--
--    $ENDOFINFO
--

type diag_dev_type: {
  port leds: output logic[4];
};
component diag_dev: diag_dev_type;
export diag_dev;
reg stat_leds: logic[4];
diag_dev.leds << stat_leds;

type status_types : {
  STATUS_OK;
  STATUS_ERR; 
  STATUS_ACT;
  STATUS_EV;
  STATUS_DOWN;
};


const STATUS_UJTAG: value := 0;
const STATUS_TAP:   value := 1;
const STATUS_LINK:  value := 2;
const STATUS_APPL:    value := 3;

array sys_status: reg[4] of status_types;
array sys_status_next: reg[4] of status_types;

object watch_timer: timer;
  watch_timer.time (50 millisec);

--
-- Diagnostics
--

process sys_status_proc:
begin
  array counter: reg[4] of int[6];
  array on: reg[4] of bool;
  array last_status: reg[4] of status_types;
  stat_leds <- 0b0000;
  
  for i = 0 to 3 do 
  begin
    counter.[i] <- 0, on.[i] <- false;
  end;
  
    
  watch_timer.init ();
  watch_timer.start ();

  for i = 0 to 3 do
  begin
    last_status.[i] <- STATUS_DOWN;
    sys_status_next.[i] <- last_status.[i];
  end;
  
  always do
  begin
    for i = 0 to 3 do
    begin
      last_status.[i] <- sys_status_next.[i];

      match sys_status.[i] with
      begin
        when STATUS_OK: 
        begin
          on.[i] <- true, counter.[i] <- 0,
          last_status.[i] <- STATUS_OK;
        end;
        when STATUS_DOWN: 
        begin
          on.[i] <- false, counter.[i] <- 0,
          last_status.[i] <- STATUS_DOWN;
        end;
        when STATUS_ERR:
        begin
          if on.[i] = false and counter.[i] = 0 then 
          begin
            on.[i] <- true, counter.[i] <- 2,
            last_status.[i] <- STATUS_ERR;
          end 
          else if on.[i] = true and counter.[i] = 0 then
          begin
            on.[i] <- false, counter.[i] <- 2;
          end
          else counter.[i] <- counter.[i] - 1;  
        end;
        when STATUS_ACT:
        begin
          if on.[i] = false and counter.[i] = 0 then 
          begin
            on.[i] <- true, counter.[i] <- 6;
            last_status.[i] <- STATUS_ACT;
          end 
          else if on.[i] = true and counter.[i] = 0 then
          begin
            on.[i] <- false, counter.[i] <- 6;
          end
          else counter.[i] <- counter.[i] - 1;  
        end;
        when STATUS_EV:
        begin
          if counter.[i] = 0 then 
          begin
            on.[i] <- true, counter.[i] <- 3;
          end 
          else if counter.[i] = 1 then
          begin
            counter.[i] <- 0;
            sys_status.[i] <- last_status.[i];
          end
          else 
          begin
            on.[i] <- not on.[i];
            counter.[i] <- counter.[i] - 1;  
          end;
        end;
      end;
      sys_status_next.[i] <- last_status.[i];
    end;
    for i = 0 to 3 do
    begin
      match i with
      begin
        when 0: if on.[0] = true then stat_leds[0] <- 1 else stat_leds[0] <- 0;
        when 1: if on.[1] = true then stat_leds[1] <- 1 else stat_leds[1] <- 0;
        when 2: if on.[2] = true then stat_leds[2] <- 1 else stat_leds[2] <- 0;
        when 3: if on.[3] = true then stat_leds[3] <- 1 else stat_leds[3] <- 0;
      end;
    end;
    watch_timer.await ();
  end;
end;
