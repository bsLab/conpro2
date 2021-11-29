modules:
begin
  module U: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Uart: IMPORT
  module System: IMPORT
  module Timer: IMPORT
  module Lcd: IMPORT
  module Mutex: IMPORT
end

import:
begin
  function init()
  object watch_reset: Process.Process
  object LOCK_FUN_print: Mutex.mutex
  object LOCK_FUN_print_val: Mutex.mutex
  object watch_timer: Timer.timer
  object u1_rx: Process.Process
  register line_pos: L[5]
  register version: L[8]
  object print_lock: Mutex.mutex
  object FUN_print: Process.Process
  object lcd1: Lcd.lcd
  object lcd1_tx: Process.Process
  object watch_set: Process.Process
  object u1_tx: Process.Process
  array line: variable[32] of L[8]var_array_line
  object u1: Uart.uart
  block var_array_line: cells[32] of L[8]
  object p1: Process.Process
  register ARG_FUN_print_linenum: L[2]
  function print(linenum:L[2])
  object LOCK_FUN_print_line: Mutex.mutex
end

temp:
begin
  register TEMP_0: L[5]
end

data:
begin
  register TEMP_0: L[5]
end

types:
begin
  type Uart.uart: {
      method init ()
      method start ()
      method stop ()
      method baud (N[0]:RHS)
      method write (L[8]:RHS,B:LHS)
      method read (L[8]:LHS,B:LHS)
      method interface (L[1]:RHS,L[1]:LHS)
    }
  type Process.Process: {
      method start ()
      method stop ()
      method call ()
    }
  type Lcd.lcd: {
      method init ()
      method cmd (L[8]:RHS)
      method write (L[8]:RHS)
      method interface (L[4]:LRHS,L[1]:LHS,L[1]:LHS,L[1]:LHS,L[1]:LHS)
    }
  type Mutex.mutex: {
      method init ()
      method lock ()
      method unlock ()
    }
  type Timer.timer: {
      method init ()
      method time (N[0]:RHS)
      method await ()
      method wakeup ()
      method start ()
      method stop ()
      method mode (L[1]:RHS)
      method sig_action (L[1]:LHS,N[0]:RHS,N[0]:RHS)
    }
end

code:
begin
               i1_fun: 
                       fun LOCK_FUN_print.init()
           i1_fun_end: 
                       nop
               i2_fun: 
                       fun LOCK_FUN_print_val.init()
           i2_fun_end: 
                       nop
               i3_fun: 
                       fun LOCK_FUN_print_line.init()
           i3_fun_end: 
                       nop
            i4_assign: 
                       move (line_pos,0) with ET=L[5]
        i4_assign_end: 
                       nop
            i5_assign: 
                       move (version,161) with ET=L[8]
        i5_assign_end: 
                       nop
               i6_fun: 
                       fun print_lock.init()
           i6_fun_end: 
                       nop
               i7_fun: 
                       fun lcd1.init()
           i7_fun_end: 
                       nop
               i8_fun: 
                       fun lcd1.cmd(41)
           i8_fun_end: 
                       nop
               i9_fun: 
                       fun lcd1.cmd(21)
           i9_fun_end: 
                       nop
              i10_fun: 
                       fun lcd1.cmd(85)
          i10_fun_end: 
                       nop
              i11_fun: 
                       fun lcd1.cmd(110)
          i11_fun_end: 
                       nop
              i12_fun: 
                       fun lcd1.cmd(112)
          i12_fun_end: 
                       nop
              i13_fun: 
                       fun lcd1.cmd(40)
          i13_fun_end: 
                       nop
              i14_fun: 
                       fun lcd1.cmd()
          i14_fun_end: 
                       nop
              i15_fun: 
                       fun lcd1.cmd(1)
          i15_fun_end: 
                       nop
              i16_fun: 
                       fun lcd1.cmd(6)
          i16_fun_end: 
                       nop
              i17_fun: 
                       fun lcd1_tx.start()
          i17_fun_end: 
                       nop
              i18_fun: 
                       fun print_lock.lock()
          i18_fun_end: 
                       nop
              i19_fun: 
                       fun line.copy()
          i19_fun_end: 
                       nop
           i20_branch: 
                       bind (2)
                       expr ($immed.[1],version[4 to 7],<,10) with ET=L[4]
                       falsejump ($immed.[1],i22_assign)
           i21_assign: 
                       expr (line.[7],,+,version[4 to 7]) with ET=L[8]
                       nop
       i21_assign_end: 
                       jump (i23_branch)
           i22_assign: 
                       expr (line.[7],55,+,version[4 to 7]) with ET=L[8]
                       nop
       i22_assign_end: 
                       nop
       i20_branch_end: 
                       nop
           i23_branch: 
                       bind (2)
                       expr ($immed.[1],version[0 to 3],<,10) with ET=L[4]
                       falsejump ($immed.[1],i25_assign)
           i24_assign: 
                       expr (line.[8],,+,version[0 to 3]) with ET=L[8]
                       nop
       i24_assign_end: 
                       jump (i26_fun)
           i25_assign: 
                       expr (line.[8],55,+,version[0 to 3]) with ET=L[8]
                       nop
       i25_assign_end: 
                       nop
       i23_branch_end: 
                       nop
              i26_fun: 
                       fun LOCK_FUN_print.lock()
          i26_fun_end: 
                       nop
           i27_assign: 
                       move (ARG_FUN_print_linenum,1) with ET=L[2]
       i27_assign_end: 
                       nop
              i28_fun: 
                       fun FUN_print.call()
          i28_fun_end: 
                       nop
              i29_fun: 
                       fun LOCK_FUN_print.unlock()
          i29_fun_end: 
                       nop
              i30_fun: 
                       fun u1.baud(38400)
          i30_fun_end: 
                       nop
              i31_fun: 
                       fun u1.init()
          i31_fun_end: 
                       nop
              i32_fun: 
                       fun u1.start()
          i32_fun_end: 
                       nop
              i33_fun: 
                       fun u1_rx.start()
          i33_fun_end: 
                       nop
              i34_fun: 
                       fun u1_tx.start()
          i34_fun_end: 
                       nop
              i35_fun: 
                       fun watch_timer.init()
          i35_fun_end: 
                       nop
              i36_fun: 
                       fun watch_set.start()
          i36_fun_end: 
                       nop
              i37_fun: 
                       fun watch_reset.start()
          i37_fun_end: 
                       nop
              i38_fun: 
                       fun p1.start()
          i38_fun_end: 
end
