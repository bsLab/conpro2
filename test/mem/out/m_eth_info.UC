modules:
begin
  module M: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Uart: IMPORT
  module System: IMPORT
  module Timer: IMPORT
  module Lcd: IMPORT
  module Mutex: IMPORT
  module Mem: IMPORT
  module Event: IMPORT
end

import:
begin
  register eth_stat: L[8]
  object LOCK_FUN_print: Mutex.mutex
  object watch_timer: Timer.timer
  object print_lock: Mutex.mutex
  register eth_int0: L[8]
  register eth_int1: L[8]
  object FUN_print: Process.Process
  array line: variable[32] of L[8]var_array_line
  object eth: Mem.mem
  register eth_irq_count: L[8]
  register eth_link_stat: L[8]
  block var_array_line: cells[32] of L[8]
  register eth_irq_stat: L[8]
  register ARG_FUN_print_linenum: L[2]
  function print(linenum:L[2])
end

temp:
begin
  register TEMP_0: L[5]
end

data:
begin
  register o: L[4]
  register r: L[8]
  register TEMPS_0: L[5]
  register TEMP_0: L[5]
  register LOOP_i_0: I[4]
end

types:
begin
  type Mem.mem: {
      method write (L[8]:RHS,L[8]:RHS)
      method read (L[8]:LHS,L[8]:RHS)
      method interface (L[8]:LRHS,L[8]:LHS,L[1]:LHS,L[1]:LHS,L[1]:LHS)
    }
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
      method sig_action (L[1]:LHS,N[0]:RHS,N[0]:RHS)
    }
end

code:
begin
              i1_loop: 
                       nop
               i2_fun: 
                       fun watch_timer.await()
           i2_fun_end: 
                       nop
               i3_fun: 
                       fun print_lock.lock()
           i3_fun_end: 
                       nop
               i4_fun: 
                       fun line.copy()
           i4_fun_end: 
                       nop
               i5_fun: 
                       fun eth.read(r,128)
           i5_fun_end: 
                       nop
            i6_branch: 
                       bind (2)
                       expr ($immed.[1],r[4 to 7],<,10) with ET=L[4]
                       falsejump ($immed.[1],i8_assign)
            i7_assign: 
                       expr (line.[1],,+,r[4 to 7]) with ET=L[8]
                       nop
        i7_assign_end: 
                       jump (i9_branch)
            i8_assign: 
                       expr (line.[1],55,+,r[4 to 7]) with ET=L[8]
                       nop
        i8_assign_end: 
                       nop
        i6_branch_end: 
                       nop
            i9_branch: 
                       bind (2)
                       expr ($immed.[1],r[0 to 3],<,10) with ET=L[4]
                       falsejump ($immed.[1],i11_assign)
           i10_assign: 
                       expr (line.[2],,+,r[0 to 3]) with ET=L[8]
                       nop
       i10_assign_end: 
                       jump (i12_fun)
           i11_assign: 
                       expr (line.[2],55,+,r[0 to 3]) with ET=L[8]
                       nop
       i11_assign_end: 
                       nop
        i9_branch_end: 
                       nop
              i12_fun: 
                       fun LOCK_FUN_print.lock()
          i12_fun_end: 
                       nop
           i13_assign: 
                       move (ARG_FUN_print_linenum,1) with ET=L[2]
       i13_assign_end: 
                       nop
              i14_fun: 
                       fun FUN_print.call()
          i14_fun_end: 
                       nop
              i15_fun: 
                       fun LOCK_FUN_print.unlock()
          i15_fun_end: 
                       nop
              i16_fun: 
                       fun print_lock.lock()
          i16_fun_end: 
                       nop
              i17_fun: 
                       fun line.copy()
          i17_fun_end: 
                       nop
           i18_assign: 
                       move (o,0) with ET=L[4]
       i18_assign_end: 
                       nop
           i19_assign: 
                       move (line.[0],eth_stat)
       i19_assign_end: 
                       nop
           i20_assign: 
                       move (line.[1],eth_irq_stat)
       i20_assign_end: 
                       nop
           i21_assign: 
                       move (line.[2],eth_link_stat)
       i21_assign_end: 
                       nop
         i22_for_loop: 
                       move (LOOP_i_0,1) with ET=I[4]
    i22_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],4,>=,LOOP_i_0) with ET=I[4]
                       falsejump ($immed.[1],i39_fun)
           i23_select: 
                       bind (16)
           i23_case_1: 
                       expr ($immed.[1],LOOP_i_0,=,1) with ET=I[4]
                       falsejump ($immed.[1],i23_case_2)
                       jump (i24_assign)
           i23_case_2: 
                       expr ($immed.[1],LOOP_i_0,=,2) with ET=I[4]
                       falsejump ($immed.[1],i23_case_3)
                       jump (i25_assign)
           i23_case_3: 
                       expr ($immed.[1],LOOP_i_0,=,3) with ET=I[4]
                       falsejump ($immed.[1],i23_case_4)
                       jump (i26_fun)
           i23_case_4: 
                       expr ($immed.[1],LOOP_i_0,=,4) with ET=I[4]
                       falsejump ($immed.[1],i28_branch)
                       jump (i27_assign)
           i24_assign: 
                       move (r,eth_int0)
       i24_assign_end: 
                       jump (i28_branch)
           i25_assign: 
                       move (r,eth_int1)
       i25_assign_end: 
                       jump (i28_branch)
              i26_fun: 
                       fun eth.read(r,115)
          i26_fun_end: 
                       jump (i28_branch)
           i27_assign: 
                       move (r,eth_irq_count)
       i27_assign_end: 
                       nop
       i23_select_end: 
                       nop
           i28_branch: 
                       bind (2)
                       expr ($immed.[1],r[4 to 7],<,10) with ET=L[4]
                       falsejump ($immed.[1],i31_assign)
           i29_assign: 
                       expr (TEMPS_0,4,+,o) with ET=L[5]
                       nop
       i29_assign_end: 
                       nop
           i30_assign: 
                       expr (line.[TEMPS_0],,+,r[4 to 7]) with ET=L[8]
                       nop
       i30_assign_end: 
                       jump (i33_branch)
           i31_assign: 
                       expr (TEMPS_0,4,+,o) with ET=L[5]
                       nop
       i31_assign_end: 
                       nop
           i32_assign: 
                       expr (line.[TEMPS_0],55,+,r[4 to 7]) with ET=L[8]
                       nop
       i32_assign_end: 
                       nop
       i28_branch_end: 
                       nop
           i33_branch: 
                       bind (2)
                       expr ($immed.[1],r[0 to 3],<,10) with ET=L[4]
                       falsejump ($immed.[1],i36_assign)
           i34_assign: 
                       expr (TEMPS_0,5,+,o) with ET=L[5]
                       nop
       i34_assign_end: 
                       nop
           i35_assign: 
                       expr (line.[TEMPS_0],,+,r[0 to 3]) with ET=L[8]
                       nop
       i35_assign_end: 
                       jump (i38_assign)
           i36_assign: 
                       expr (TEMPS_0,5,+,o) with ET=L[5]
                       nop
       i36_assign_end: 
                       nop
           i37_assign: 
                       expr (line.[TEMPS_0],55,+,r[0 to 3]) with ET=L[8]
                       nop
       i37_assign_end: 
                       nop
       i33_branch_end: 
                       nop
           i38_assign: 
                       expr (o,o,+,3) with ET=L[4]
                       nop
       i38_assign_end: 
                       nop
    i22_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_0,LOOP_i_0,+,1) with ET=I[4]
                       nop
                       jump (i22_for_loop_cond)
     i22_for_loop_end: 
                       nop
              i39_fun: 
                       fun LOCK_FUN_print.lock()
          i39_fun_end: 
                       nop
           i40_assign: 
                       move (ARG_FUN_print_linenum,2) with ET=L[2]
       i40_assign_end: 
                       nop
              i41_fun: 
                       fun FUN_print.call()
          i41_fun_end: 
                       nop
              i42_fun: 
                       fun LOCK_FUN_print.unlock()
          i42_fun_end: 
                       jump (i2_fun)
          i1_loop_end: 
end
