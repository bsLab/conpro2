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
  object eth: Mem.mem
  register ARG_FUN_char_to_num_x: L[8]
  register ARG_FUN_print_req_v: L[8]
  object ru: Mem.mem
  register stat_req: L[1]
  object LOCK_FUN_char_to_num: Mutex.mutex
end

data:
begin
  register data_c: L[8]
  register data_x: L[8]
  register addr: L[8]
  register LOOP_i_2: I[3]
  register LOOP_i_3: I[3]
  register LOOP_i_4: I[3]
  register LOOP_i_5: I[3]
  register LOOP_i_6: I[3]
  register LOOP_i_7: I[3]
  register LOOP_i_8: I[3]
  register LOOP_i_9: I[3]
  register data_8: L[8]
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
                       bind (22)
            i3_case_1: 
                       expr ($immed.[1],data_c,=,) with ET=L[8]
                       falsejump ($immed.[1],i3_case_2)
                       jump (i4_assign)
            i3_case_2: 
                       expr ($immed.[1],data_c,=,) with ET=L[8]
                       falsejump ($immed.[1],i3_case_3)
                       jump (i8_assign)
            i3_case_3: 
                       expr ($immed.[1],data_c,=,) with ET=L[8]
                       falsejump ($immed.[1],i3_case_4)
                       jump (i33_assign)
            i3_case_4: 
                       expr ($immed.[1],data_c,=,) with ET=L[8]
                       falsejump ($immed.[1],i3_case_5)
                       jump (i63_assign)
            i3_case_5: 
                       expr ($immed.[1],data_c,=,) with ET=L[8]
                       falsejump ($immed.[1],i3_case_6)
                       jump (i88_assign)
            i3_case_6: 
                       jump (i118_assign)
            i4_assign: 
                       move (stat_req,1) with ET=L[1]
        i4_assign_end: 
                       nop
            i5_assign: 
                       move (q_u1_tx,) with ET=L[8]
        i5_assign_end: 
                       nop
            i6_assign: 
                       move (q_u1_tx,) with ET=L[8]
        i6_assign_end: 
                       nop
            i7_assign: 
                       move (q_u1_tx,) with ET=L[8]
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
                       move (LOOP_i_2,1) with ET=I[3]
    i10_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],2,>=,LOOP_i_2) with ET=I[3]
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
                       expr (LOOP_i_2,LOOP_i_2,+,1) with ET=I[3]
                       nop
                       jump (i10_for_loop_cond)
     i10_for_loop_end: 
                       nop
              i19_fun: 
                       fun LOCK_FUN_print_req.lock()
          i19_fun_end: 
                       nop
           i20_assign: 
                       move (ARG_FUN_print_req_k,) with ET=L[8]
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
                       move (LOOP_i_3,1) with ET=I[3]
    i25_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],2,>=,LOOP_i_3) with ET=I[3]
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
                       expr (LOOP_i_3,LOOP_i_3,+,1) with ET=I[3]
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
                       move (LOOP_i_4,1) with ET=I[3]
    i35_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],2,>=,LOOP_i_4) with ET=I[3]
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
                       expr (LOOP_i_4,LOOP_i_4,+,1) with ET=I[3]
                       nop
                       jump (i35_for_loop_cond)
     i35_for_loop_end: 
                       nop
              i44_fun: 
                       fun LOCK_FUN_print_req.lock()
          i44_fun_end: 
                       nop
           i45_assign: 
                       move (ARG_FUN_print_req_k,) with ET=L[8]
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
           i49_assign: 
                       move (data_8,0) with ET=L[8]
       i49_assign_end: 
                       nop
         i50_for_loop: 
                       move (LOOP_i_5,1) with ET=I[3]
    i50_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],2,>=,LOOP_i_5) with ET=I[3]
                       falsejump ($immed.[1],i59_fun)
           i51_assign: 
                       move (data_c,q_u1_rx)
       i51_assign_end: 
                       nop
              i52_fun: 
                       fun LOCK_FUN_char_to_num.lock()
          i52_fun_end: 
                       nop
           i53_assign: 
                       move (ARG_FUN_char_to_num_x,data_c)
       i53_assign_end: 
                       nop
              i54_fun: 
                       fun FUN_char_to_num.call()
          i54_fun_end: 
                       nop
           i55_assign: 
                       move (data_x,RET_FUN_char_to_num_y)
       i55_assign_end: 
                       nop
              i56_fun: 
                       fun LOCK_FUN_char_to_num.unlock()
          i56_fun_end: 
                       nop
           i57_assign: 
                       expr (data_8,data_8,lsl,4) with ET=L[8]
                       nop
       i57_assign_end: 
                       nop
           i58_assign: 
                       expr (data_8,data_8,+,data_x)
                       nop
       i58_assign_end: 
                       nop
    i50_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_5,LOOP_i_5,+,1) with ET=I[3]
                       nop
                       jump (i50_for_loop_cond)
     i50_for_loop_end: 
                       nop
              i59_fun: 
                       fun ru.write(data_8,addr)
          i59_fun_end: 
                       nop
           i60_assign: 
                       move (q_u1_tx,) with ET=L[8]
       i60_assign_end: 
                       nop
           i61_assign: 
                       move (q_u1_tx,) with ET=L[8]
       i61_assign_end: 
                       nop
           i62_assign: 
                       move (q_u1_tx,) with ET=L[8]
       i62_assign_end: 
                       jump (i2_assign)
           i63_assign: 
                       move (stat_req,1) with ET=L[1]
       i63_assign_end: 
                       nop
           i64_assign: 
                       move (addr,0) with ET=L[8]
       i64_assign_end: 
                       nop
         i65_for_loop: 
                       move (LOOP_i_6,1) with ET=I[3]
    i65_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],2,>=,LOOP_i_6) with ET=I[3]
                       falsejump ($immed.[1],i74_fun)
           i66_assign: 
                       move (data_c,q_u1_rx)
       i66_assign_end: 
                       nop
              i67_fun: 
                       fun LOCK_FUN_char_to_num.lock()
          i67_fun_end: 
                       nop
           i68_assign: 
                       move (ARG_FUN_char_to_num_x,data_c)
       i68_assign_end: 
                       nop
              i69_fun: 
                       fun FUN_char_to_num.call()
          i69_fun_end: 
                       nop
           i70_assign: 
                       move (data_x,RET_FUN_char_to_num_y)
       i70_assign_end: 
                       nop
              i71_fun: 
                       fun LOCK_FUN_char_to_num.unlock()
          i71_fun_end: 
                       nop
           i72_assign: 
                       expr (addr,addr,lsl,4) with ET=L[8]
                       nop
       i72_assign_end: 
                       nop
           i73_assign: 
                       expr (addr,addr,+,data_x)
                       nop
       i73_assign_end: 
                       nop
    i65_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_6,LOOP_i_6,+,1) with ET=I[3]
                       nop
                       jump (i65_for_loop_cond)
     i65_for_loop_end: 
                       nop
              i74_fun: 
                       fun LOCK_FUN_print_req.lock()
          i74_fun_end: 
                       nop
           i75_assign: 
                       move (ARG_FUN_print_req_k,) with ET=L[8]
       i75_assign_end: 
                       nop
           i76_assign: 
                       move (ARG_FUN_print_req_v,addr)
       i76_assign_end: 
                       nop
              i77_fun: 
                       fun FUN_print_req.call()
          i77_fun_end: 
                       nop
              i78_fun: 
                       fun LOCK_FUN_print_req.unlock()
          i78_fun_end: 
                       nop
              i79_fun: 
                       fun eth.read(data_8,addr)
          i79_fun_end: 
                       nop
         i80_for_loop: 
                       move (LOOP_i_7,1) with ET=I[3]
    i80_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],2,>=,LOOP_i_7) with ET=I[3]
                       falsejump ($immed.[1],i2_assign)
              i81_fun: 
                       fun LOCK_FUN_char_of_num.lock()
          i81_fun_end: 
                       nop
           i82_assign: 
                       move (ARG_FUN_char_of_num_x,data_8[4 to 7]) with ET=L[4]
       i82_assign_end: 
                       nop
              i83_fun: 
                       fun FUN_char_of_num.call()
          i83_fun_end: 
                       nop
           i84_assign: 
                       move (data_c,RET_FUN_char_of_num_y)
       i84_assign_end: 
                       nop
              i85_fun: 
                       fun LOCK_FUN_char_of_num.unlock()
          i85_fun_end: 
                       nop
           i86_assign: 
                       move (q_u1_tx,data_c)
       i86_assign_end: 
                       nop
           i87_assign: 
                       expr (data_8,data_8,lsl,4) with ET=L[8]
                       nop
       i87_assign_end: 
                       nop
    i80_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_7,LOOP_i_7,+,1) with ET=I[3]
                       nop
                       jump (i80_for_loop_cond)
     i80_for_loop_end: 
                       jump (i2_assign)
           i88_assign: 
                       move (stat_req,1) with ET=L[1]
       i88_assign_end: 
                       nop
           i89_assign: 
                       move (addr,0) with ET=L[8]
       i89_assign_end: 
                       nop
         i90_for_loop: 
                       move (LOOP_i_8,1) with ET=I[3]
    i90_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],2,>=,LOOP_i_8) with ET=I[3]
                       falsejump ($immed.[1],i99_fun)
           i91_assign: 
                       move (data_c,q_u1_rx)
       i91_assign_end: 
                       nop
              i92_fun: 
                       fun LOCK_FUN_char_to_num.lock()
          i92_fun_end: 
                       nop
           i93_assign: 
                       move (ARG_FUN_char_to_num_x,data_c)
       i93_assign_end: 
                       nop
              i94_fun: 
                       fun FUN_char_to_num.call()
          i94_fun_end: 
                       nop
           i95_assign: 
                       move (data_x,RET_FUN_char_to_num_y)
       i95_assign_end: 
                       nop
              i96_fun: 
                       fun LOCK_FUN_char_to_num.unlock()
          i96_fun_end: 
                       nop
           i97_assign: 
                       expr (addr,addr,lsl,4) with ET=L[8]
                       nop
       i97_assign_end: 
                       nop
           i98_assign: 
                       expr (addr,addr,+,data_x)
                       nop
       i98_assign_end: 
                       nop
    i90_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_8,LOOP_i_8,+,1) with ET=I[3]
                       nop
                       jump (i90_for_loop_cond)
     i90_for_loop_end: 
                       nop
              i99_fun: 
                       fun LOCK_FUN_print_req.lock()
          i99_fun_end: 
                       nop
          i100_assign: 
                       move (ARG_FUN_print_req_k,) with ET=L[8]
      i100_assign_end: 
                       nop
          i101_assign: 
                       move (ARG_FUN_print_req_v,addr)
      i101_assign_end: 
                       nop
             i102_fun: 
                       fun FUN_print_req.call()
         i102_fun_end: 
                       nop
             i103_fun: 
                       fun LOCK_FUN_print_req.unlock()
         i103_fun_end: 
                       nop
          i104_assign: 
                       move (data_8,0) with ET=L[8]
      i104_assign_end: 
                       nop
        i105_for_loop: 
                       move (LOOP_i_9,1) with ET=I[3]
   i105_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],2,>=,LOOP_i_9) with ET=I[3]
                       falsejump ($immed.[1],i114_fun)
          i106_assign: 
                       move (data_c,q_u1_rx)
      i106_assign_end: 
                       nop
             i107_fun: 
                       fun LOCK_FUN_char_to_num.lock()
         i107_fun_end: 
                       nop
          i108_assign: 
                       move (ARG_FUN_char_to_num_x,data_c)
      i108_assign_end: 
                       nop
             i109_fun: 
                       fun FUN_char_to_num.call()
         i109_fun_end: 
                       nop
          i110_assign: 
                       move (data_x,RET_FUN_char_to_num_y)
      i110_assign_end: 
                       nop
             i111_fun: 
                       fun LOCK_FUN_char_to_num.unlock()
         i111_fun_end: 
                       nop
          i112_assign: 
                       expr (data_8,data_8,lsl,4) with ET=L[8]
                       nop
      i112_assign_end: 
                       nop
          i113_assign: 
                       expr (data_8,data_8,+,data_x)
                       nop
      i113_assign_end: 
                       nop
   i105_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_9,LOOP_i_9,+,1) with ET=I[3]
                       nop
                       jump (i105_for_loop_cond)
    i105_for_loop_end: 
                       nop
             i114_fun: 
                       fun eth.write(data_8,addr)
         i114_fun_end: 
                       nop
          i115_assign: 
                       move (q_u1_tx,) with ET=L[8]
      i115_assign_end: 
                       nop
          i116_assign: 
                       move (q_u1_tx,) with ET=L[8]
      i116_assign_end: 
                       nop
          i117_assign: 
                       move (q_u1_tx,) with ET=L[8]
      i117_assign_end: 
                       jump (i2_assign)
          i118_assign: 
                       move (stat_err,1) with ET=L[1]
      i118_assign_end: 
                       nop
          i119_assign: 
                       move (q_u1_tx,) with ET=L[8]
      i119_assign_end: 
                       nop
          i120_assign: 
                       move (q_u1_tx,) with ET=L[8]
      i120_assign_end: 
                       nop
          i121_assign: 
                       move (q_u1_tx,) with ET=L[8]
      i121_assign_end: 
                       nop
        i3_select_end: 
                       jump (i2_assign)
          i1_loop_end: 
end
