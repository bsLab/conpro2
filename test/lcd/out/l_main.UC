modules:
begin
  module L: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module System: IMPORT
  module Timer: IMPORT
  module Lcd: IMPORT
  module Mutex: IMPORT
  module Random: IMPORT
end

import:
begin
  function init()
  object watch_reset: Process.Process
  object LOCK_FUN_print: Mutex.mutex
  register stat_a: L[1]
  register stat_b: L[1]
  register stat_c: L[1]
  object watch_timer: Timer.timer
  register version: L[8]
  object print_lock: Mutex.mutex
  object FUN_print: Process.Process
  object lcd1: Lcd.lcd
  object watch_set: Process.Process
  array line: register[32] of L[8]
  object p1: Process.Process
  object p2: Process.Process
  register ARG_FUN_print_linenum: L[2]
  object rnd1: Random.random
  function print(linenum:L[2])
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
  type Random.random: {
      method init ()
      method read (L[8]:LHS)
      method seed (N[0]:RHS)
    }
  type Timer.timer: {
      method init ()
      method time (N[0]:RHS)
      method await ()
      method wakeup ()
      method start ()
      method stop ()
      method mode (L[1]:RHS)
    }
end

code:
begin
               i1_fun: 
                       fun LOCK_FUN_print.init()
           i1_fun_end: 
                       nop
            i2_assign: 
                       move (version,160) with ET=L[8]
        i2_assign_end: 
                       nop
            i3_assign: 
                       move (stat_a,0) with ET=L[1]
        i3_assign_end: 
                       nop
            i4_assign: 
                       move (stat_b,0) with ET=L[1]
        i4_assign_end: 
                       nop
            i5_assign: 
                       move (stat_c,0) with ET=L[1]
        i5_assign_end: 
                       nop
               i6_fun: 
                       fun watch_timer.init()
           i6_fun_end: 
                       nop
               i7_fun: 
                       fun watch_set.start()
           i7_fun_end: 
                       nop
               i8_fun: 
                       fun watch_reset.start()
           i8_fun_end: 
                       nop
               i9_fun: 
                       fun print_lock.init()
           i9_fun_end: 
                       nop
              i10_fun: 
                       fun rnd1.init()
          i10_fun_end: 
                       nop
              i11_fun: 
                       fun lcd1.init()
          i11_fun_end: 
                       nop
              i12_fun: 
                       fun lcd1.cmd(41)
          i12_fun_end: 
                       nop
              i13_fun: 
                       fun lcd1.cmd(21)
          i13_fun_end: 
                       nop
              i14_fun: 
                       fun lcd1.cmd(85)
          i14_fun_end: 
                       nop
              i15_fun: 
                       fun lcd1.cmd(110)
          i15_fun_end: 
                       nop
              i16_fun: 
                       fun lcd1.cmd(114)
          i16_fun_end: 
                       nop
              i17_fun: 
                       fun lcd1.cmd(40)
          i17_fun_end: 
                       nop
              i18_fun: 
                       fun lcd1.cmd()
          i18_fun_end: 
                       nop
              i19_fun: 
                       fun lcd1.cmd(1)
          i19_fun_end: 
                       nop
              i20_fun: 
                       fun lcd1.cmd(6)
          i20_fun_end: 
                       nop
              i21_fun: 
                       fun print_lock.lock()
          i21_fun_end: 
                       nop
              i22_fun: 
                       fun line.copy()
          i22_fun_end: 
                       nop
           i23_branch: 
                       bind (2)
                       expr ($immed.[1],version[4 to 7],<,10) with ET=L[4]
                       falsejump ($immed.[1],i25_assign)
           i24_assign: 
                       expr (line.[7],,+,version[4 to 7]) with ET=L[8]
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
                       expr (line.[8],,+,version[0 to 3]) with ET=L[8]
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
           i33_assign: 
                       move (stat_a,1) with ET=L[1]
       i33_assign_end: 
                       nop
              i34_fun: 
                       fun p1.start()
          i34_fun_end: 
                       nop
              i35_fun: 
                       fun p2.start()
          i35_fun_end: 
end
