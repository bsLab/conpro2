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
  register ARG_FUN_print_eth_stat: L[8]
  object LOCK_FUN_print: Mutex.mutex
  object print_lock: Mutex.mutex
  object FUN_print: Process.Process
  array line: variable[32] of L[8]var_array_line
  object eth: Mem.mem
  register eth_irq_count: L[8]
  block var_array_line: cells[32] of L[8]
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
      method interface (L[8]:LRHS,L[8]:RHS,L[1]:LHS,L[1]:LHS,L[1]:LHS)
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
end

code:
begin
               i1_fun: 
                       fun print_lock.lock()
           i1_fun_end: 
                       nop
            i2_select: 
                       bind (12)
            i2_case_1: 
                       expr ($immed.[1],ARG_FUN_print_eth_stat,=,) with ET=L[8]
                       falsejump ($immed.[1],i2_case_2)
                       jump (i3_fun)
            i2_case_2: 
                       expr ($immed.[1],ARG_FUN_print_eth_stat,=,) with ET=L[8]
                       falsejump ($immed.[1],i2_case_3)
                       jump (i4_fun)
            i2_case_3: 
                       expr ($immed.[1],ARG_FUN_print_eth_stat,=,) with ET=L[8]
                       falsejump ($immed.[1],i30_fun)
                       jump (i5_fun)
               i3_fun: 
                       fun line.copy()
           i3_fun_end: 
                       jump (i30_fun)
               i4_fun: 
                       fun line.copy()
           i4_fun_end: 
                       jump (i30_fun)
               i5_fun: 
                       fun line.copy()
           i5_fun_end: 
                       nop
               i6_fun: 
                       fun LOCK_FUN_print.lock()
           i6_fun_end: 
                       nop
            i7_assign: 
                       move (ARG_FUN_print_linenum,1) with ET=L[2]
        i7_assign_end: 
                       nop
               i8_fun: 
                       fun FUN_print.call()
           i8_fun_end: 
                       nop
               i9_fun: 
                       fun LOCK_FUN_print.unlock()
           i9_fun_end: 
                       nop
              i10_fun: 
                       fun print_lock.lock()
          i10_fun_end: 
                       nop
              i11_fun: 
                       fun line.copy()
          i11_fun_end: 
                       nop
           i12_assign: 
                       move (o,0) with ET=L[4]
       i12_assign_end: 
                       nop
         i13_for_loop: 
                       move (LOOP_i_0,1) with ET=I[4]
    i13_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],4,>=,LOOP_i_0) with ET=I[4]
                       falsejump ($immed.[1],i30_fun)
           i14_select: 
                       bind (16)
           i14_case_1: 
                       expr ($immed.[1],LOOP_i_0,=,1) with ET=I[4]
                       falsejump ($immed.[1],i14_case_2)
                       jump (i15_fun)
           i14_case_2: 
                       expr ($immed.[1],LOOP_i_0,=,2) with ET=I[4]
                       falsejump ($immed.[1],i14_case_3)
                       jump (i16_fun)
           i14_case_3: 
                       expr ($immed.[1],LOOP_i_0,=,3) with ET=I[4]
                       falsejump ($immed.[1],i14_case_4)
                       jump (i17_fun)
           i14_case_4: 
                       expr ($immed.[1],LOOP_i_0,=,4) with ET=I[4]
                       falsejump ($immed.[1],i19_branch)
                       jump (i18_assign)
              i15_fun: 
                       fun eth.read(r,118)
          i15_fun_end: 
                       jump (i19_branch)
              i16_fun: 
                       fun eth.read(r,126)
          i16_fun_end: 
                       jump (i19_branch)
              i17_fun: 
                       fun eth.read(r,115)
          i17_fun_end: 
                       jump (i19_branch)
           i18_assign: 
                       move (r,eth_irq_count)
       i18_assign_end: 
                       nop
       i14_select_end: 
                       nop
           i19_branch: 
                       bind (2)
                       expr ($immed.[1],r[4 to 7],<,10) with ET=L[4]
                       falsejump ($immed.[1],i22_assign)
           i20_assign: 
                       expr (TEMPS_0,4,+,o) with ET=L[5]
                       nop
       i20_assign_end: 
                       nop
           i21_assign: 
                       expr (line.[TEMPS_0],,+,r[4 to 7]) with ET=L[8]
                       nop
       i21_assign_end: 
                       jump (i24_branch)
           i22_assign: 
                       expr (TEMPS_0,4,+,o) with ET=L[5]
                       nop
       i22_assign_end: 
                       nop
           i23_assign: 
                       expr (line.[TEMPS_0],55,+,r[4 to 7]) with ET=L[8]
                       nop
       i23_assign_end: 
                       nop
       i19_branch_end: 
                       nop
           i24_branch: 
                       bind (2)
                       expr ($immed.[1],r[0 to 3],<,10) with ET=L[4]
                       falsejump ($immed.[1],i27_assign)
           i25_assign: 
                       expr (TEMPS_0,5,+,o) with ET=L[5]
                       nop
       i25_assign_end: 
                       nop
           i26_assign: 
                       expr (line.[TEMPS_0],,+,r[0 to 3]) with ET=L[8]
                       nop
       i26_assign_end: 
                       jump (i29_assign)
           i27_assign: 
                       expr (TEMPS_0,5,+,o) with ET=L[5]
                       nop
       i27_assign_end: 
                       nop
           i28_assign: 
                       expr (line.[TEMPS_0],55,+,r[0 to 3]) with ET=L[8]
                       nop
       i28_assign_end: 
                       nop
       i24_branch_end: 
                       nop
           i29_assign: 
                       expr (o,o,+,3) with ET=L[4]
                       nop
       i29_assign_end: 
                       nop
    i13_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_0,LOOP_i_0,+,1) with ET=I[4]
                       nop
                       jump (i13_for_loop_cond)
     i13_for_loop_end: 
                       nop
        i2_select_end: 
                       nop
              i30_fun: 
                       fun LOCK_FUN_print.lock()
          i30_fun_end: 
                       nop
           i31_assign: 
                       move (ARG_FUN_print_linenum,2) with ET=L[2]
       i31_assign_end: 
                       nop
              i32_fun: 
                       fun FUN_print.call()
          i32_fun_end: 
                       nop
              i33_fun: 
                       fun LOCK_FUN_print.unlock()
          i33_fun_end: 
end
