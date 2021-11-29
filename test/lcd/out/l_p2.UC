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
  object LOCK_FUN_print: Mutex.mutex
  object print_lock: Mutex.mutex
  object long_timer: Timer.timer
  object FUN_print: Process.Process
  array line: register[32] of L[8]
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
  register t: L[8]
  register TEMP_0: L[5]
  register LOOP_i_2: I[6]
end

types:
begin
  type Process.Process: {
      method start ()
      method stop ()
      method call ()
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
                       fun long_timer.init()
           i1_fun_end: 
                       nop
               i2_fun: 
                       fun long_timer.start()
           i2_fun_end: 
                       nop
          i3_for_loop: 
                       move (LOOP_i_2,0) with ET=I[6]
     i3_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],15,>=,LOOP_i_2) with ET=I[6]
                       falsejump ($immed.[1],%END)
               i4_fun: 
                       fun long_timer.await()
           i4_fun_end: 
                       nop
               i5_fun: 
                       fun long_timer.await()
           i5_fun_end: 
                       nop
               i6_fun: 
                       fun print_lock.lock()
           i6_fun_end: 
                       nop
               i7_fun: 
                       fun line.copy()
           i7_fun_end: 
                       nop
               i8_fun: 
                       fun rnd1.read(t)
           i8_fun_end: 
                       nop
            i9_branch: 
                       bind (2)
                       expr ($immed.[1],t[4 to 7],<,10) with ET=L[4]
                       falsejump ($immed.[1],i11_assign)
           i10_assign: 
                       expr (line.[7],,+,t[4 to 7]) with ET=L[8]
                       nop
       i10_assign_end: 
                       jump (i12_branch)
           i11_assign: 
                       expr (line.[7],55,+,t[4 to 7]) with ET=L[8]
                       nop
       i11_assign_end: 
                       nop
        i9_branch_end: 
                       nop
           i12_branch: 
                       bind (2)
                       expr ($immed.[1],t[0 to 3],<,10) with ET=L[4]
                       falsejump ($immed.[1],i14_assign)
           i13_assign: 
                       expr (line.[8],,+,t[0 to 3]) with ET=L[8]
                       nop
       i13_assign_end: 
                       jump (i15_fun)
           i14_assign: 
                       expr (line.[8],55,+,t[0 to 3]) with ET=L[8]
                       nop
       i14_assign_end: 
                       nop
       i12_branch_end: 
                       nop
              i15_fun: 
                       fun LOCK_FUN_print.lock()
          i15_fun_end: 
                       nop
           i16_assign: 
                       move (ARG_FUN_print_linenum,3) with ET=L[2]
       i16_assign_end: 
                       nop
              i17_fun: 
                       fun FUN_print.call()
          i17_fun_end: 
                       nop
              i18_fun: 
                       fun LOCK_FUN_print.unlock()
          i18_fun_end: 
                       nop
     i3_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_2,LOOP_i_2,+,1) with ET=I[6]
                       nop
                       jump (i3_for_loop_cond)
      i3_for_loop_end: 
end
