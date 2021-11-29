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
  object eth_ev: Event.event
  const ETH_P0: 6
  const ETH_P1: 7
  register eth_stat: L[8]
  signal eth_rst: L[1]
  signal dev_eth_int: L[1]
  const ETH_F0: 9
  object eth_tmo: Process.Process
  register eth_state: L[4]
  object eth_irq: Process.Process
  register eth_int0: L[8]
  register eth_int1: L[8]
  const ETH_S0: 8
  object eth_res_timer: Timer.timer
  object eth: Mem.mem
  register eth_link_stat: L[8]
  const ETH_I0: 1
  const ETH_I1: 2
  const ETH_I2: 3
  const ETH_I3: 4
  const ETH_I4: 5
  register eth_timeout: B
end

temp:
begin
  register TEMP_0: L[19]
end

data:
begin
  register n: I[5]
  register TEMP_0: L[19]
end

types:
begin
  type Event.event: {
      method init ()
      method await ()
      method wakeup ()
    }
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
                       fun eth_res_timer.init()
           i1_fun_end: 
                       nop
            i2_assign: 
                       move (eth_state,1) with ET=L[4]
        i2_assign_end: 
                       jump (i4_select)
              i3_loop: 
                       nop
            i4_select: 
                       bind (36)
            i4_case_1: 
                       expr ($immed.[1],eth_state,=,1) with ET=L[4]
                       falsejump ($immed.[1],i4_case_2)
                       jump (i5_assign)
            i4_case_2: 
                       expr ($immed.[1],eth_state,=,2) with ET=L[4]
                       falsejump ($immed.[1],i4_case_3)
                       jump (i10_assign)
            i4_case_3: 
                       expr ($immed.[1],eth_state,=,3) with ET=L[4]
                       falsejump ($immed.[1],i4_case_4)
                       jump (i13_assign)
            i4_case_4: 
                       expr ($immed.[1],eth_state,=,4) with ET=L[4]
                       falsejump ($immed.[1],i4_case_5)
                       jump (i16_assign)
            i4_case_5: 
                       expr ($immed.[1],eth_state,=,5) with ET=L[4]
                       falsejump ($immed.[1],i4_case_6)
                       jump (i20_assign)
            i4_case_6: 
                       expr ($immed.[1],eth_state,=,6) with ET=L[4]
                       falsejump ($immed.[1],i4_case_7)
                       jump (i28_assign)
            i4_case_7: 
                       expr ($immed.[1],eth_state,=,7) with ET=L[4]
                       falsejump ($immed.[1],i4_case_8)
                       jump (i47_assign)
            i4_case_8: 
                       expr ($immed.[1],eth_state,=,8) with ET=L[4]
                       falsejump ($immed.[1],i4_case_9)
                       jump (i69_assign)
            i4_case_9: 
                       expr ($immed.[1],eth_state,=,9) with ET=L[4]
                       falsejump ($immed.[1],i4_select)
                       jump (i71_assign)
            i5_assign: 
                       move (eth_link_stat,) with ET=L[8]
        i5_assign_end: 
                       nop
            i6_assign: 
                       move (eth_stat,) with ET=L[8]
        i6_assign_end: 
                       nop
               i7_fun: 
                       fun eth_res_timer.start()
           i7_fun_end: 
                       nop
               i8_fun: 
                       fun eth_res_timer.await()
           i8_fun_end: 
                       nop
            i9_assign: 
                       move (eth_state,2) with ET=L[4]
        i9_assign_end: 
                       jump (i4_select)
           i10_assign: 
                       move (eth_stat,) with ET=L[8]
       i10_assign_end: 
                       nop
          i11_waitfor: 
                       bind (2)
                       expr ($immed.[1],eth_rst,=,1) with ET=L[1]
                       falsejump ($immed.[1],i11_waitfor)
      i11_waitfor_end: 
                       nop
           i12_assign: 
                       move (eth_state,3) with ET=L[4]
       i12_assign_end: 
                       jump (i4_select)
           i13_assign: 
                       move (eth_stat,) with ET=L[8]
       i13_assign_end: 
                       nop
          i14_waitfor: 
                       bind (2)
                       expr ($immed.[1],dev_eth_int,=,0) with ET=L[1]
                       falsejump ($immed.[1],i14_waitfor)
      i14_waitfor_end: 
                       nop
           i15_assign: 
                       move (eth_state,4) with ET=L[4]
       i15_assign_end: 
                       jump (i4_select)
           i16_assign: 
                       move (eth_stat,) with ET=L[8]
       i16_assign_end: 
                       nop
              i17_fun: 
                       fun eth.read(eth_int0,118)
          i17_fun_end: 
                       nop
       i18_while_loop: 
                       bind (2)
                       expr ($immed.[1],eth_int0[5],=,0) with ET=L[1]
                       falsejump ($immed.[1],i19_assign)
              i18_fun: 
                       fun eth.read(eth_int0,118)
          i18_fun_end: 
                       jump (i18_while_loop)
   i18_while_loop_end: 
                       nop
           i19_assign: 
                       move (eth_state,5) with ET=L[4]
       i19_assign_end: 
                       jump (i4_select)
           i20_assign: 
                       move (eth_stat,) with ET=L[8]
       i20_assign_end: 
                       nop
              i21_fun: 
                       fun eth_ev.init()
          i21_fun_end: 
                       nop
              i22_fun: 
                       fun eth_irq.start()
          i22_fun_end: 
                       nop
           i23_assign: 
                       move (eth_stat,) with ET=L[8]
       i23_assign_end: 
                       nop
              i24_fun: 
                       fun eth_ev.await()
          i24_fun_end: 
                       nop
              i25_fun: 
                       fun eth.write(48,100)
          i25_fun_end: 
                       nop
              i26_fun: 
                       fun eth.write(61,125)
          i26_fun_end: 
                       nop
           i27_assign: 
                       move (eth_state,6) with ET=L[4]
       i27_assign_end: 
                       jump (i4_select)
           i28_assign: 
                       move (eth_link_stat,) with ET=L[8]
       i28_assign_end: 
                       nop
           i29_assign: 
                       move (eth_stat,) with ET=L[8]
       i29_assign_end: 
                       nop
              i30_fun: 
                       fun eth.write(0,120)
          i30_fun_end: 
                       nop
              i31_fun: 
                       fun eth.write(128,122)
          i31_fun_end: 
                       nop
              i32_fun: 
                       fun eth.write(166,121)
          i32_fun_end: 
                       nop
              i33_fun: 
                       fun eth.write(128,120)
          i33_fun_end: 
                       nop
          i34_waitfor: 
                       move ($tmp.[waitfor_count],185183)
     i34_waitfor_loop: 
                       bind (4)
                       expr ($tmp.[waitfor_count],$tmp.[waitfor_count],-,1)
                       nop
                       expr ($immed.[1],$tmp.[waitfor_count],=,0)
                       falsejump ($immed.[1],i34_waitfor_loop)
  i34_waitfor_loop_end: 
                       jump (i35_fun)
      i34_waitfor_end: 
                       nop
              i35_fun: 
                       fun eth.write(224,120)
          i35_fun_end: 
                       nop
           i36_assign: 
                       move (n,0) with ET=I[5]
       i36_assign_end: 
                       nop
       i37_while_loop: 
                       bind (4)
                       expr ($immed.[1],eth_int1[5],=,0) with ET=L[1]
                       expr ($immed.[2],n,<>,2) with ET=I[5]
                       expr ($immed.[3],$immed.[1],and,$immed.[2])
                       falsejump ($immed.[3],i43_branch)
              i37_fun: 
                       fun eth_tmo.start()
          i37_fun_end: 
                       nop
              i38_fun: 
                       fun eth_ev.await()
          i38_fun_end: 
                       nop
              i39_fun: 
                       fun eth_tmo.stop()
          i39_fun_end: 
                       nop
           i40_branch: 
                       bind (2)
                       expr ($immed.[1],eth_timeout,=,)
                       falsejump ($immed.[1],i37_while_loop)
           i41_assign: 
                       expr (n,n,+,1) with ET=I[5]
                       nop
       i41_assign_end: 
                       nop
           i42_assign: 
                       expr (eth_stat,,+,n:CT=L[0]) with ET=L[8]
                       nop
       i42_assign_end: 
                       nop
       i40_branch_end: 
                       jump (i37_while_loop)
   i37_while_loop_end: 
                       nop
           i43_branch: 
                       bind (2)
                       expr ($immed.[1],eth_int1[5],=,1) with ET=L[1]
                       falsejump ($immed.[1],i46_assign)
           i44_assign: 
                       move (eth_link_stat,) with ET=L[8]
       i44_assign_end: 
                       nop
          i45_waitfor: 
                       move ($tmp.[waitfor_count],4627) with ET=L[14]
     i45_waitfor_loop: 
                       bind (4)
                       expr ($tmp.[waitfor_count],$tmp.[waitfor_count],-,1) with ET=L[14]
                       nop
                       expr ($immed.[1],$tmp.[waitfor_count],=,0) with ET=L[14]
                       falsejump ($immed.[1],i45_waitfor_loop)
  i45_waitfor_loop_end: 
                       jump (i46_assign)
      i45_waitfor_end: 
                       nop
       i43_branch_end: 
                       nop
           i46_assign: 
                       move (eth_state,7) with ET=L[4]
       i46_assign_end: 
                       jump (i4_select)
           i47_assign: 
                       move (eth_stat,) with ET=L[8]
       i47_assign_end: 
                       nop
              i48_fun: 
                       fun eth.write(0,100)
          i48_fun_end: 
                       nop
              i49_fun: 
                       fun eth.write(29,125)
          i49_fun_end: 
                       nop
              i50_fun: 
                       fun eth.write(0,120)
          i50_fun_end: 
                       nop
              i51_fun: 
                       fun eth.write(246,121)
          i51_fun_end: 
                       nop
              i52_fun: 
                       fun eth.write(128,120)
          i52_fun_end: 
                       nop
          i53_waitfor: 
                       move ($tmp.[waitfor_count],185183)
     i53_waitfor_loop: 
                       bind (4)
                       expr ($tmp.[waitfor_count],$tmp.[waitfor_count],-,1)
                       nop
                       expr ($immed.[1],$tmp.[waitfor_count],=,0)
                       falsejump ($immed.[1],i53_waitfor_loop)
  i53_waitfor_loop_end: 
                       jump (i54_fun)
      i53_waitfor_end: 
                       nop
              i54_fun: 
                       fun eth.write(224,120)
          i54_fun_end: 
                       nop
           i55_assign: 
                       move (n,0) with ET=I[5]
       i55_assign_end: 
                       nop
       i56_while_loop: 
                       bind (6)
                       expr ($immed.[1],eth_int1[0],=,0) with ET=L[1]
                       expr ($immed.[2],eth_int1[2],=,0) with ET=L[1]
                       expr ($immed.[3],$immed.[1],and,$immed.[2])
                       expr ($immed.[4],n,<>,6) with ET=I[5]
                       expr ($immed.[5],$immed.[3],and,$immed.[4])
                       falsejump ($immed.[5],i62_fun)
              i56_fun: 
                       fun eth_tmo.start()
          i56_fun_end: 
                       nop
              i57_fun: 
                       fun eth_ev.await()
          i57_fun_end: 
                       nop
              i58_fun: 
                       fun eth_tmo.stop()
          i58_fun_end: 
                       nop
           i59_branch: 
                       bind (2)
                       expr ($immed.[1],eth_timeout,=,)
                       falsejump ($immed.[1],i56_while_loop)
           i60_assign: 
                       expr (n,n,+,1) with ET=I[5]
                       nop
       i60_assign_end: 
                       nop
           i61_assign: 
                       expr (eth_stat,,+,n:CT=L[0]) with ET=L[8]
                       nop
       i61_assign_end: 
                       nop
       i59_branch_end: 
                       jump (i56_while_loop)
   i56_while_loop_end: 
                       nop
              i62_fun: 
                       fun eth.write(12,112)
          i62_fun_end: 
                       nop
           i63_branch: 
                       bind (2)
                       expr ($immed.[1],eth_int1[0],=,1) with ET=L[1]
                       falsejump ($immed.[1],i66_branch)
           i64_assign: 
                       move (eth_link_stat,) with ET=L[8]
       i64_assign_end: 
                       nop
           i65_assign: 
                       move (eth_state,8) with ET=L[4]
       i65_assign_end: 
                       jump (i4_select)
           i66_branch: 
                       bind (2)
                       expr ($immed.[1],eth_int1[2],=,1) with ET=L[1]
                       falsejump ($immed.[1],i68_assign)
           i67_assign: 
                       move (eth_link_stat,) with ET=L[8]
       i67_assign_end: 
                       nop
       i66_branch_end: 
                       nop
           i68_assign: 
                       move (eth_state,9) with ET=L[4]
       i68_assign_end: 
                       nop
       i63_branch_end: 
                       jump (i4_select)
           i69_assign: 
                       move (eth_stat,) with ET=L[8]
       i69_assign_end: 
                       nop
              i70_fun: 
                       fun eth_ev.await()
          i70_fun_end: 
                       jump (i4_select)
           i71_assign: 
                       move (eth_stat,) with ET=L[8]
       i71_assign_end: 
                       nop
              i72_fun: 
                       fun eth_ev.await()
          i72_fun_end: 
                       nop
        i4_select_end: 
                       jump (i4_select)
          i3_loop_end: 
end
