reg stat_leds,stat_ev: logic[4];
reg diag: logic[3];
DEV.leds << stat_leds;

object watch_timer: timer;
  watch_timer.time (300 millisec);

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

