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
  queue q_u1_tx: L[8]
  object LOCK_FUN_char_of_num: Mutex.mutex
  object FUN_char_to_num: Process.Process
  register RET_FUN_char_of_num_y: L[8]
  function char_to_num(x:L[8])
  register RET_FUN_char_to_num_y: L[8]
  object FUN_char_of_num: Process.Process
  function char_of_num(x:L[4])
  object FUN_print_req: Process.Process
  register stat_err: L[1]
  function print_req(k:L[8],v:L[8])
  object LOCK_FUN_print_req: Mutex.mutex
  register ARG_FUN_print_req_k: L[8]
  register ARG_FUN_char_of_num_x: L[4]
  queue q_u1_rx: L[8]
  register ARG_FUN_char_to_num_x: L[8]
  register ARG_FUN_print_req_v: L[8]
  object ru: Ram.ram
  register stat_req: L[1]
  object LOCK_FUN_char_to_num: Mutex.mutex
end

data:
begin
  register data_c: L[8]
  register data_x: L[8]
  register addr: L[8]
  register LOOP_i_1: I[3]
  register LOOP_i_2: I[3]
  register LOOP_i_3: I[3]
  register LOOP_i_4: I[3]
  register data_8: L[8]
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
  type Ram.ram: {
      method write (L[8]:RHS,L[12]:RHS)
      method read (L[8]:LHS,L[12]:RHS)
    }
end

code:
begin
              i1_loop: 
                       nop
            i2_assign: 
                       move (data_c,q_u1_rx)
        i2_assign_end: 
                       nop
            i3_select: 
                       bind (14)
            i3_case_1: 
                       expr ($immed.[1],data_c,=,'Z') with ET=L[8]
                       falsejump ($immed.[1],i3_case_2)
                       jump (i4_assign)
            i3_case_2: 
                       expr ($immed.[1],data_c,=,'R') with ET=L[8]
                       falsejump ($immed.[1],i3_case_3)
                       jump (i8_assign)
            i3_case_3: 
                       expr ($immed.[1],data_c,=,'W') with ET=L[8]
                       falsejump ($immed.[1],i3_case_4)
                       jump (i33_assign)
            i3_case_4: 
                       jump (i62_assign)
            i4_assign: 
                       move (stat_req,1) with ET=L[1]
        i4_assign_end: 
                       nop
            i5_assign: 
                       move (q_u1_tx,'O') with ET=L[8]
        i5_assign_end: 
                       nop
            i6_assign: 
                       move (q_u1_tx,'K') with ET=L[8]
        i6_assign_end: 
                       nop
            i7_assign: 
                       move (q_u1_tx,'.') with ET=L[8]
        i7_assign_end: 
                       jump (i2_assign)
            i8_assign: 
                       move (stat_req,1) with ET=L[1]
        i8_assign_end: 
                       nop
            i9_assign: 
                       move (addr,0) with ET=L[8]
        i9_assign_end: 
                       nop
         i10_for_loop: 
                       move (LOOP_i_1,1) with ET=I[3]
    i10_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],2,>=,LOOP_i_1) with ET=I[3]
                       falsejump ($immed.[1],i19_fun)
           i11_assign: 
                       move (data_c,q_u1_rx)
       i11_assign_end: 
                       nop
              i12_fun: 
                       fun LOCK_FUN_char_to_num.lock()
          i12_fun_end: 
                       nop
           i13_assign: 
                       move (ARG_FUN_char_to_num_x,data_c)
       i13_assign_end: 
                       nop
              i14_fun: 
                       fun FUN_char_to_num.call()
          i14_fun_end: 
                       nop
           i15_assign: 
                       move (data_x,RET_FUN_char_to_num_y)
       i15_assign_end: 
                       nop
              i16_fun: 
                       fun LOCK_FUN_char_to_num.unlock()
          i16_fun_end: 
                       nop
           i17_assign: 
                       expr (addr,addr,lsl,4) with ET=L[8]
                       nop
       i17_assign_end: 
                       nop
           i18_assign: 
                       expr (addr,addr,+,data_x)
                       nop
       i18_assign_end: 
                       nop
    i10_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_1,LOOP_i_1,+,1) with ET=I[3]
                       nop
                       jump (i10_for_loop_cond)
     i10_for_loop_end: 
                       nop
              i19_fun: 
                       fun LOCK_FUN_print_req.lock()
          i19_fun_end: 
                       nop
           i20_assign: 
                       move (ARG_FUN_print_req_k,'R') with ET=L[8]
       i20_assign_end: 
                       nop
           i21_assign: 
                       move (ARG_FUN_print_req_v,addr)
       i21_assign_end: 
                       nop
              i22_fun: 
                       fun FUN_print_req.call()
          i22_fun_end: 
                       nop
              i23_fun: 
                       fun LOCK_FUN_print_req.unlock()
          i23_fun_end: 
                       nop
              i24_fun: 
                       fun ru.read(data_8,addr)
          i24_fun_end: 
                       nop
         i25_for_loop: 
                       move (LOOP_i_2,1) with ET=I[3]
    i25_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],2,>=,LOOP_i_2) with ET=I[3]
                       falsejump ($immed.[1],i2_assign)
              i26_fun: 
                       fun LOCK_FUN_char_of_num.lock()
          i26_fun_end: 
                       nop
           i27_assign: 
                       move (ARG_FUN_char_of_num_x,data_8[4 to 7]) with ET=L[4]
       i27_assign_end: 
                       nop
              i28_fun: 
                       fun FUN_char_of_num.call()
          i28_fun_end: 
                       nop
           i29_assign: 
                       move (data_c,RET_FUN_char_of_num_y)
       i29_assign_end: 
                       nop
              i30_fun: 
                       fun LOCK_FUN_char_of_num.unlock()
          i30_fun_end: 
                       nop
           i31_assign: 
                       move (q_u1_tx,data_c)
       i31_assign_end: 
                       nop
           i32_assign: 
                       expr (data_8,data_8,lsl,4) with ET=L[8]
                       nop
       i32_assign_end: 
                       nop
    i25_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_2,LOOP_i_2,+,1) with ET=I[3]
                       nop
                       jump (i25_for_loop_cond)
     i25_for_loop_end: 
                       jump (i2_assign)
           i33_assign: 
                       move (stat_req,1) with ET=L[1]
       i33_assign_end: 
                       nop
           i34_assign: 
                       move (addr,0) with ET=L[8]
       i34_assign_end: 
                       nop
         i35_for_loop: 
                       move (LOOP_i_3,1) with ET=I[3]
    i35_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],2,>=,LOOP_i_3) with ET=I[3]
                       falsejump ($immed.[1],i44_fun)
           i36_assign: 
                       move (data_c,q_u1_rx)
       i36_assign_end: 
                       nop
              i37_fun: 
                       fun LOCK_FUN_char_to_num.lock()
          i37_fun_end: 
                       nop
           i38_assign: 
                       move (ARG_FUN_char_to_num_x,data_c)
       i38_assign_end: 
                       nop
              i39_fun: 
                       fun FUN_char_to_num.call()
          i39_fun_end: 
                       nop
           i40_assign: 
                       move (data_x,RET_FUN_char_to_num_y)
       i40_assign_end: 
                       nop
              i41_fun: 
                       fun LOCK_FUN_char_to_num.unlock()
          i41_fun_end: 
                       nop
           i42_assign: 
                       expr (addr,addr,lsl,4) with ET=L[8]
                       nop
       i42_assign_end: 
                       nop
           i43_assign: 
                       expr (addr,addr,+,data_x)
                       nop
       i43_assign_end: 
                       nop
    i35_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_3,LOOP_i_3,+,1) with ET=I[3]
                       nop
                       jump (i35_for_loop_cond)
     i35_for_loop_end: 
                       nop
              i44_fun: 
                       fun LOCK_FUN_print_req.lock()
          i44_fun_end: 
                       nop
           i45_assign: 
                       move (ARG_FUN_print_req_k,'W') with ET=L[8]
       i45_assign_end: 
                       nop
           i46_assign: 
                       move (ARG_FUN_print_req_v,addr)
       i46_assign_end: 
                       nop
              i47_fun: 
                       fun FUN_print_req.call()
          i47_fun_end: 
                       nop
              i48_fun: 
                       fun LOCK_FUN_print_req.unlock()
          i48_fun_end: 
                       nop
         i49_for_loop: 
                       move (LOOP_i_4,1) with ET=I[3]
    i49_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],2,>=,LOOP_i_4) with ET=I[3]
                       falsejump ($immed.[1],i58_fun)
           i50_assign: 
                       move (data_c,q_u1_rx)
       i50_assign_end: 
                       nop
              i51_fun: 
                       fun LOCK_FUN_char_to_num.lock()
          i51_fun_end: 
                       nop
           i52_assign: 
                       move (ARG_FUN_char_to_num_x,data_c)
       i52_assign_end: 
                       nop
              i53_fun: 
                       fun FUN_char_to_num.call()
          i53_fun_end: 
                       nop
           i54_assign: 
                       move (data_x,RET_FUN_char_to_num_y)
       i54_assign_end: 
                       nop
              i55_fun: 
                       fun LOCK_FUN_char_to_num.unlock()
          i55_fun_end: 
                       nop
           i56_assign: 
                       expr (data_8,data_8,lsl,4) with ET=L[8]
                       nop
       i56_assign_end: 
                       nop
           i57_assign: 
                       expr (data_8,data_8,+,data_x)
                       nop
       i57_assign_end: 
                       nop
    i49_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_4,LOOP_i_4,+,1) with ET=I[3]
                       nop
                       jump (i49_for_loop_cond)
     i49_for_loop_end: 
                       nop
              i58_fun: 
                       fun ru.write(data_8,addr)
          i58_fun_end: 
                       nop
           i59_assign: 
                       move (q_u1_tx,'O') with ET=L[8]
       i59_assign_end: 
                       nop
           i60_assign: 
                       move (q_u1_tx,'K') with ET=L[8]
       i60_assign_end: 
                       nop
           i61_assign: 
                       move (q_u1_tx,'.') with ET=L[8]
       i61_assign_end: 
                       jump (i2_assign)
           i62_assign: 
                       move (stat_err,1) with ET=L[1]
       i62_assign_end: 
                       nop
           i63_assign: 
                       move (q_u1_tx,'E') with ET=L[8]
       i63_assign_end: 
                       nop
           i64_assign: 
                       move (q_u1_tx,'R') with ET=L[8]
       i64_assign_end: 
                       nop
           i65_assign: 
                       move (q_u1_tx,'R') with ET=L[8]
       i65_assign_end: 
                       nop
        i3_select_end: 
                       jump (i2_assign)
          i1_loop_end: 
end
