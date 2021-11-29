modules:
begin
  module R: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Uart: IMPORT
  module System: IMPORT
  module Timer: IMPORT
  module Lcd: IMPORT
  module Mutex: IMPORT
  module Ram: IMPORT
end

import:
begin
  object test: Process.Process
  object LOCK_FUN_char_of_num: Mutex.mutex
  function init()
  object watch_reset: Process.Process
  object LOCK_FUN_print: Mutex.mutex
  object watch_timer: Timer.timer
  object u1_rx: Process.Process
  register line_pos: L[5]
  object LOCK_FUN_print_kb: Mutex.mutex
  register version: L[8]
  object print_lock: Mutex.mutex
  object FUN_print: Process.Process
  object lcd1: Lcd.lcd
  object LOCK_FUN_print_req: Mutex.mutex
  object lcd1_tx: Process.Process
  object watch_set: Process.Process
  object u1_tx: Process.Process
  array line: variable[32] of L[8]var_array_line
  object u1: Uart.uart
  block var_array_line: cells[32] of L[8]
  register ARG_FUN_print_linenum: L[2]
  object LOCK_FUN_print_mem_err: Mutex.mutex
  object interp: Process.Process
  function print(linenum:L[2])
  object LOCK_FUN_char_to_num: Mutex.mutex
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
                       fun LOCK_FUN_print_req.init()
           i2_fun_end: 
                       nop
               i3_fun: 
                       fun LOCK_FUN_print_kb.init()
           i3_fun_end: 
                       nop
               i4_fun: 
                       fun LOCK_FUN_print_mem_err.init()
           i4_fun_end: 
                       nop
               i5_fun: 
                       fun LOCK_FUN_char_to_num.init()
           i5_fun_end: 
                       nop
               i6_fun: 
                       fun LOCK_FUN_char_of_num.init()
           i6_fun_end: 
                       nop
            i7_assign: 
                       move (line_pos,0) with ET=L[5]
        i7_assign_end: 
                       nop
            i8_assign: 
                       move (version,177) with ET=L[8]
        i8_assign_end: 
                       nop
               i9_fun: 
                       fun print_lock.init()
           i9_fun_end: 
                       nop
              i10_fun: 
                       fun lcd1.init()
          i10_fun_end: 
                       nop
              i11_fun: 
                       fun lcd1.cmd(41)
          i11_fun_end: 
                       nop
              i12_fun: 
                       fun lcd1.cmd(21)
          i12_fun_end: 
                       nop
              i13_fun: 
                       fun lcd1.cmd(85)
          i13_fun_end: 
                       nop
              i14_fun: 
                       fun lcd1.cmd(110)
          i14_fun_end: 
                       nop
              i15_fun: 
                       fun lcd1.cmd(112)
          i15_fun_end: 
                       nop
              i16_fun: 
                       fun lcd1.cmd(40)
          i16_fun_end: 
                       nop
              i17_fun: 
                       fun lcd1.cmd(00001100)
          i17_fun_end: 
                       nop
              i18_fun: 
                       fun lcd1.cmd(1)
          i18_fun_end: 
                       nop
              i19_fun: 
                       fun lcd1.cmd(6)
          i19_fun_end: 
                       nop
              i20_fun: 
                       fun lcd1_tx.start()
          i20_fun_end: 
                       nop
              i21_fun: 
                       fun print_lock.lock()
          i21_fun_end: 
                       nop
              i22_fun: 
                       fun line.copy("READY VXX.")
          i22_fun_end: 
                       nop
           i23_branch: 
                       bind (2)
                       expr ($immed.[1],version[4 to 7],<,10) with ET=L[4]
                       falsejump ($immed.[1],i25_assign)
           i24_assign: 
                       expr (line.[7],'0',+,version[4 to 7]) with ET=L[8]
                       nop
       i24_assign_end: 
                       jump (i26_branch)
           i25_assign: 
                       expr (line.[7],55,+,version[4 to 7]) with ET=L[8]
                       nop
       i25_assign_end: 
                       nop
       i23_branch_end: 
                       nop
           i26_branch: 
                       bind (2)
                       expr ($immed.[1],version[0 to 3],<,10) with ET=L[4]
                       falsejump ($immed.[1],i28_assign)
           i27_assign: 
                       expr (line.[8],'0',+,version[0 to 3]) with ET=L[8]
                       nop
       i27_assign_end: 
                       jump (i29_fun)
           i28_assign: 
                       expr (line.[8],55,+,version[0 to 3]) with ET=L[8]
                       nop
       i28_assign_end: 
                       nop
       i26_branch_end: 
                       nop
              i29_fun: 
                       fun LOCK_FUN_print.lock()
          i29_fun_end: 
                       nop
           i30_assign: 
                       move (ARG_FUN_print_linenum,1) with ET=L[2]
       i30_assign_end: 
                       nop
              i31_fun: 
                       fun FUN_print.call()
          i31_fun_end: 
                       nop
              i32_fun: 
                       fun LOCK_FUN_print.unlock()
          i32_fun_end: 
                       nop
              i33_fun: 
                       fun u1.baud(38400)
          i33_fun_end: 
                       nop
              i34_fun: 
                       fun u1.init()
          i34_fun_end: 
                       nop
              i35_fun: 
                       fun u1.start()
          i35_fun_end: 
                       nop
              i36_fun: 
                       fun u1_rx.start()
          i36_fun_end: 
                       nop
              i37_fun: 
                       fun u1_tx.start()
          i37_fun_end: 
                       nop
              i38_fun: 
                       fun watch_timer.init()
          i38_fun_end: 
                       nop
              i39_fun: 
                       fun watch_set.start()
          i39_fun_end: 
                       nop
              i40_fun: 
                       fun watch_reset.start()
          i40_fun_end: 
                       nop
              i41_fun: 
                       fun interp.start()
          i41_fun_end: 
                       nop
              i42_fun: 
                       fun test.start()
          i42_fun_end: 
end
