begin
  init();
  init();
  init(1);
  begin
    lock();
    ARG_FUN_f_x <- 3;
    ARG_FUN_f_y <- 4;
    call();
    x <- RET_FUN_f_z;
    unlock();
  end;
  c1_real <- 0;
  c1_imag <- 1;
  for LOOP_i_2 = 1 TO 99 do
    begin
      .LOOP_i_2ca_real <- (.LOOP_i_2ca_imag + LOOP_i_2);
      .LOOP_i_2ca_imag <- (.LOOP_i_2ca_real + LOOP_i_2);
    end;
  start();
  start();
  for LOOP_i_3 = 1 TO 2 do
    down();
end;
