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
  function print(linenum:L[2])
end

temp:
begin
  register TEMP_0: L[5]
end

data:
begin
  register TEMP_0: L[5]
  register LOOP_i_1: I[6]
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
                       move (LOOP_i_1,0) with ET=I[6]
     i3_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],15,>=,LOOP_i_1) with ET=I[6]
                       falsejump ($immed.[1],%END)
               i4_fun: 
                       fun long_timer.await()
           i4_fun_end: 
                       nop
               i5_fun: 
                       fun print_lock.lock()
           i5_fun_end: 
                       nop
               i6_fun: 
                       fun line.copy()
           i6_fun_end: 
                       nop
            i7_branch: 
                       bind (2)
                       expr ($immed.[1],LOOP_i_1,<,10) with ET=I[6]
                       falsejump ($immed.[1],i9_assign)
            i8_assign: 
                       expr (line.[7],,+,LOOP_i_1:CT=L[0]) with ET=L[8]
                       nop
        i8_assign_end: 
                       jump (i10_fun)
            i9_assign: 
                       expr (line.[7],55,+,LOOP_i_1:CT=L[0]) with ET=L[8]
                       nop
        i9_assign_end: 
                       nop
        i7_branch_end: 
                       nop
              i10_fun: 
                       fun LOCK_FUN_print.lock()
          i10_fun_end: 
                       nop
           i11_assign: 
                       move (ARG_FUN_print_linenum,2) with ET=L[2]
       i11_assign_end: 
                       nop
              i12_fun: 
                       fun FUN_print.call()
          i12_fun_end: 
                       nop
              i13_fun: 
                       fun LOCK_FUN_print.unlock()
          i13_fun_end: 
                       nop
     i3_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_1,LOOP_i_1,+,1) with ET=I[6]
                       nop
                       jump (i3_for_loop_cond)
      i3_for_loop_end: 
end
