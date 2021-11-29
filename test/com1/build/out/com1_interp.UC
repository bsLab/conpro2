modules:
begin
  module Com1: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Mutex: IMPORT
  module Uart: IMPORT
  module Timer: IMPORT
  module System: IMPORT
end

import:
begin
  function tty_read(c,tmo)
  object FUN_str_to_int: Process.Process
  function tty_print()
  object FUN_str_of_int: Process.Process
  queue tty_stdout: C8
  register ARG_FUN_str_of_int_x: I[16]
  function str_to_int()
  register tty_stdin_tmo: L[1]
  array line: register[32] of C8
  object LOCK_FUN_str_to_int: Mutex.mutex
  object FUN_tty_print: Process.Process
  function str_of_int(x:I[16])
  object tty_rx_tmo: Process.Process
  object LOCK_FUN_str_of_int: Mutex.mutex
  queue tty_stdin: C8
  object LOCK_FUN_tty_print: Mutex.mutex
  register RET_FUN_str_to_int_y: I[16]
end

temp:
begin
  register TEMP_0: L[5]
end

data:
begin
  register d: I[16]
  register token_state: I[8]
  register TEMP_0: L[5]
  register pos: I[8]
  register tmo: B
  register buf_ind: I[4]
  value TOKEN_ERR: 31
  register c: C8
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
end

code:
begin
               i1_fun: 
                       fun line.copy("Interpreter Ready;.")
           i1_fun_end: 
                       nop
               i2_fun: 
                       fun LOCK_FUN_tty_print.lock()
           i2_fun_end: 
                       nop
               i3_fun: 
                       fun FUN_tty_print.call()
           i3_fun_end: 
                       nop
               i4_fun: 
                       fun LOCK_FUN_tty_print.unlock()
           i4_fun_end: 
                       nop
            i5_assign: 
                       move (token_state,0) with ET=I[8]
        i5_assign_end: 
                       nop
            i6_assign: 
                       move (buf_ind,0) with ET=I[4]
        i6_assign_end: 
                       jump (i8_select)
              i7_loop: 
                       nop
            i8_select: 
                       bind (8)
            i8_case_1: 
                       expr ($immed.[1],token_state,=,0) with ET=I[8]
                       falsejump ($immed.[1],i8_case_2)
                       jump (i9_assign)
            i8_case_2: 
                       expr ($immed.[1],token_state,=,1) with ET=I[8]
                       falsejump ($immed.[1],i19_select)
                       jump (i10_assign)
            i9_assign: 
                       move (c,tty_stdin:CT=L[8]) with ET=C8
        i9_assign_end: 
                       jump (i19_select)
           i10_assign: 
                       expr (pos,pos,+,1) with ET=I[8]
                       nop
       i10_assign_end: 
                       nop
              i11_fun: 
                       fun tty_rx_tmo.start()
          i11_fun_end: 
                       nop
           i12_assign: 
                       move (c,tty_stdin:CT=L[8]) with ET=C8
       i12_assign_end: 
                       nop
              i13_fun: 
                       fun tty_rx_tmo.stop()
          i13_fun_end: 
                       nop
           i14_branch: 
                       bind (2)
                       expr ($immed.[1],tty_stdin_tmo,=,1) with ET=L[1]
                       falsejump ($immed.[1],i16_assign)
           i15_assign: 
                       move (tmo,true) with ET=B
       i15_assign_end: 
                       jump (i17_branch)
           i16_assign: 
                       move (tmo,false) with ET=B
       i16_assign_end: 
                       nop
       i14_branch_end: 
                       nop
           i17_branch: 
                       bind (2)
                       expr ($immed.[1],tmo,=,true) with ET=B
                       falsejump ($immed.[1],i19_select)
           i18_assign: 
                       move (token_state,31) with ET=I[8]
       i18_assign_end: 
                       nop
       i17_branch_end: 
                       nop
        i8_select_end: 
                       nop
           i19_select: 
                       bind (16)
           i19_case_1: 
                       expr ($immed.[1],token_state,=,0) with ET=I[8]
                       falsejump ($immed.[1],i19_case_2)
                       jump (i20_branch)
           i19_case_2: 
                       expr ($immed.[1],token_state,=,1) with ET=I[8]
                       falsejump ($immed.[1],i19_case_3)
                       jump (i20_branch)
           i19_case_3: 
                       expr ($immed.[1],token_state,=,10) with ET=I[8]
                       falsejump ($immed.[1],i19_case_4)
                       jump (i36_assign)
           i19_case_4: 
                       expr ($immed.[1],token_state,=,31) with ET=I[8]
                       falsejump ($immed.[1],i8_select)
                       jump (i52_fun)
           i20_branch: 
                       bind (4)
                       expr ($immed.[1],c,>=,'0') with ET=L[8]
                       expr ($immed.[2],c,<=,'9') with ET=L[8]
                       expr ($immed.[3],$immed.[1],and,$immed.[2])
                       falsejump ($immed.[3],i24_select)
           i21_assign: 
                       move (line.[buf_ind:CT=L[5]],c:CT=L[8]) with ET=C8
       i21_assign_end: 
                       nop
           i22_assign: 
                       expr (buf_ind,buf_ind,+,1) with ET=I[4]
                       nop
       i22_assign_end: 
                       nop
           i23_assign: 
                       move (token_state,1) with ET=I[8]
       i23_assign_end: 
                       jump (i8_select)
           i24_select: 
                       bind (14)
           i24_case_1: 
                       expr ($immed.[1],c,=,';') with ET=L[8]
                       falsejump ($immed.[1],i24_case_2)
                       jump (i25_branch)
           i24_case_2: 
                       expr ($immed.[1],c,=,' ') with ET=L[8]
                       falsejump ($immed.[1],i24_case_3)
                       jump (i28_branch)
           i24_case_3: 
                       expr ($immed.[1],c,=,'-') with ET=L[8]
                       falsejump ($immed.[1],i24_case_4)
                       jump (i30_branch)
           i24_case_4: 
                       jump (i35_assign)
           i25_branch: 
                       bind (2)
                       expr ($immed.[1],buf_ind,>,1) with ET=I[4]
                       falsejump ($immed.[1],i27_assign)
           i26_assign: 
                       move (token_state,10) with ET=I[8]
       i26_assign_end: 
                       jump (i8_select)
           i27_assign: 
                       move (token_state,31) with ET=I[8]
       i27_assign_end: 
                       nop
       i25_branch_end: 
                       jump (i8_select)
           i28_branch: 
                       bind (2)
                       expr ($immed.[1],token_state,<>,0) with ET=I[8]
                       falsejump ($immed.[1],i8_select)
           i29_assign: 
                       move (token_state,31) with ET=I[8]
       i29_assign_end: 
                       nop
       i28_branch_end: 
                       jump (i8_select)
           i30_branch: 
                       bind (2)
                       expr ($immed.[1],token_state,=,0) with ET=I[8]
                       falsejump ($immed.[1],i34_assign)
           i31_assign: 
                       move (line.[0],'-') with ET=C8
       i31_assign_end: 
                       nop
           i32_assign: 
                       move (buf_ind,1) with ET=I[4]
       i32_assign_end: 
                       nop
           i33_assign: 
                       move (token_state,1) with ET=I[8]
       i33_assign_end: 
                       jump (i8_select)
           i34_assign: 
                       move (token_state,31) with ET=I[8]
       i34_assign_end: 
                       nop
       i30_branch_end: 
                       jump (i8_select)
           i35_assign: 
                       move (token_state,31) with ET=I[8]
       i35_assign_end: 
                       nop
       i24_select_end: 
                       nop
       i20_branch_end: 
                       jump (i8_select)
           i36_assign: 
                       move (line.[buf_ind:CT=L[5]],'.') with ET=C8
       i36_assign_end: 
                       nop
              i37_fun: 
                       fun LOCK_FUN_str_to_int.lock()
          i37_fun_end: 
                       nop
              i38_fun: 
                       fun FUN_str_to_int.call()
          i38_fun_end: 
                       nop
           i39_assign: 
                       move (d,RET_FUN_str_to_int_y) with ET=I[16]
       i39_assign_end: 
                       nop
              i40_fun: 
                       fun LOCK_FUN_str_to_int.unlock()
          i40_fun_end: 
                       nop
           i41_assign: 
                       expr (d,d,+,1) with ET=I[16]
                       nop
       i41_assign_end: 
                       nop
              i42_fun: 
                       fun LOCK_FUN_str_of_int.lock()
          i42_fun_end: 
                       nop
           i43_assign: 
                       move (ARG_FUN_str_of_int_x,d) with ET=I[16]
       i43_assign_end: 
                       nop
              i44_fun: 
                       fun FUN_str_of_int.call()
          i44_fun_end: 
                       nop
              i45_fun: 
                       fun LOCK_FUN_str_of_int.unlock()
          i45_fun_end: 
                       nop
              i46_fun: 
                       fun LOCK_FUN_tty_print.lock()
          i46_fun_end: 
                       nop
              i47_fun: 
                       fun FUN_tty_print.call()
          i47_fun_end: 
                       nop
              i48_fun: 
                       fun LOCK_FUN_tty_print.unlock()
          i48_fun_end: 
                       nop
           i49_assign: 
                       move (tty_stdout,';') with ET=C8
       i49_assign_end: 
                       nop
           i50_assign: 
                       move (token_state,0) with ET=I[8]
       i50_assign_end: 
                       nop
           i51_assign: 
                       move (buf_ind,0) with ET=I[4]
       i51_assign_end: 
                       jump (i8_select)
              i52_fun: 
                       fun line.copy("ERR;.")
          i52_fun_end: 
                       nop
              i53_fun: 
                       fun LOCK_FUN_tty_print.lock()
          i53_fun_end: 
                       nop
              i54_fun: 
                       fun FUN_tty_print.call()
          i54_fun_end: 
                       nop
              i55_fun: 
                       fun LOCK_FUN_tty_print.unlock()
          i55_fun_end: 
                       nop
           i56_assign: 
                       move (token_state,0) with ET=I[8]
       i56_assign_end: 
                       nop
           i57_assign: 
                       move (buf_ind,0) with ET=I[4]
       i57_assign_end: 
                       nop
       i19_select_end: 
                       jump (i8_select)
          i7_loop_end: 
end
