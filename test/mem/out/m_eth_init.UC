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
  register eth_stat: L[8]
  signal eth_rst: L[1]
  signal dev_eth_int: L[1]
  object watch_timer: Timer.timer
  object eth_irq: Process.Process
  register eth_int0: L[8]
  register eth_int1: L[8]
  object eth_res_timer: Timer.timer
  object eth: Mem.mem
  register eth_link_stat: L[8]
end

temp:
begin
  register TEMP_0: L[19]
end

data:
begin
  register n: I[5]
  register r: L[8]
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
      method interface (L[8]:LRHS,L[8]:RHS,L[1]:LHS,L[1]:LHS,L[1]:LHS)
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
                       move (eth_link_stat,) with ET=L[8]
        i2_assign_end: 
                       nop
            i3_assign: 
                       move (eth_stat,) with ET=L[8]
        i3_assign_end: 
                       nop
               i4_fun: 
                       fun eth_res_timer.start()
           i4_fun_end: 
                       nop
               i5_fun: 
                       fun eth_res_timer.await()
           i5_fun_end: 
                       nop
            i6_assign: 
                       move (eth_stat,) with ET=L[8]
        i6_assign_end: 
                       nop
           i7_waitfor: 
                       bind (2)
                       expr ($immed.[1],eth_rst,=,1) with ET=L[1]
                       falsejump ($immed.[1],i7_waitfor)
       i7_waitfor_end: 
                       nop
            i8_assign: 
                       move (eth_stat,) with ET=L[8]
        i8_assign_end: 
                       nop
           i9_waitfor: 
                       bind (2)
                       expr ($immed.[1],dev_eth_int,=,0) with ET=L[1]
                       falsejump ($immed.[1],i9_waitfor)
       i9_waitfor_end: 
                       nop
           i10_assign: 
                       move (eth_stat,) with ET=L[8]
       i10_assign_end: 
                       nop
              i11_fun: 
                       fun eth.read(eth_int0,118)
          i11_fun_end: 
                       nop
       i12_while_loop: 
                       bind (2)
                       expr ($immed.[1],eth_int0[5],=,0) with ET=L[1]
                       falsejump ($immed.[1],i13_assign)
              i12_fun: 
                       fun eth.read(eth_int0,118)
          i12_fun_end: 
                       jump (i12_while_loop)
   i12_while_loop_end: 
                       nop
           i13_assign: 
                       move (eth_stat,) with ET=L[8]
       i13_assign_end: 
                       nop
              i14_fun: 
                       fun eth_ev.init()
          i14_fun_end: 
                       nop
              i15_fun: 
                       fun eth_irq.start()
          i15_fun_end: 
                       nop
              i16_fun: 
                       fun eth_ev.await()
          i16_fun_end: 
                       nop
           i17_assign: 
                       move (eth_link_stat,) with ET=L[8]
       i17_assign_end: 
                       nop
              i18_fun: 
                       fun eth.write(0,120)
          i18_fun_end: 
                       nop
              i19_fun: 
                       fun eth.write(128,122)
          i19_fun_end: 
                       nop
              i20_fun: 
                       fun eth.write(166,121)
          i20_fun_end: 
                       nop
              i21_fun: 
                       fun eth.write(128,120)
          i21_fun_end: 
                       nop
          i22_waitfor: 
                       move ($tmp.[waitfor_count],185183)
     i22_waitfor_loop: 
                       bind (4)
                       expr ($tmp.[waitfor_count],$tmp.[waitfor_count],-,1)
                       nop
                       expr ($immed.[1],$tmp.[waitfor_count],=,0)
                       falsejump ($immed.[1],i22_waitfor_loop)
  i22_waitfor_loop_end: 
                       jump (i23_fun)
      i22_waitfor_end: 
                       nop
              i23_fun: 
                       fun eth.write(224,120)
          i23_fun_end: 
                       nop
           i24_assign: 
                       move (n,1) with ET=I[5]
       i24_assign_end: 
                       nop
       i25_while_loop: 
                       bind (4)
                       expr ($immed.[1],eth_int1[5],=,0) with ET=L[1]
                       expr ($immed.[2],n,<>,3) with ET=I[5]
                       expr ($immed.[3],$immed.[1],and,$immed.[2])
                       falsejump ($immed.[3],i29_fun)
              i25_fun: 
                       fun watch_timer.await()
          i25_fun_end: 
                       nop
           i26_branch: 
                       bind (2)
                       expr ($immed.[1],eth_int1[5],=,1) with ET=L[1]
                       falsejump ($immed.[1],i28_assign)
           i27_assign: 
                       move (eth_link_stat,) with ET=L[8]
       i27_assign_end: 
                       nop
       i26_branch_end: 
                       nop
           i28_assign: 
                       expr (n,n,+,1) with ET=I[5]
                       nop
       i28_assign_end: 
                       jump (i25_while_loop)
   i25_while_loop_end: 
                       nop
              i29_fun: 
                       fun eth.write(0,120)
          i29_fun_end: 
                       nop
              i30_fun: 
                       fun eth.write(246,121)
          i30_fun_end: 
                       nop
              i31_fun: 
                       fun eth.write(128,120)
          i31_fun_end: 
                       nop
          i32_waitfor: 
                       move ($tmp.[waitfor_count],185183)
     i32_waitfor_loop: 
                       bind (4)
                       expr ($tmp.[waitfor_count],$tmp.[waitfor_count],-,1)
                       nop
                       expr ($immed.[1],$tmp.[waitfor_count],=,0)
                       falsejump ($immed.[1],i32_waitfor_loop)
  i32_waitfor_loop_end: 
                       jump (i33_fun)
      i32_waitfor_end: 
                       nop
              i33_fun: 
                       fun eth.write(224,120)
          i33_fun_end: 
                       nop
           i34_assign: 
                       move (eth_stat,) with ET=L[8]
       i34_assign_end: 
                       nop
           i35_assign: 
                       move (r,0) with ET=L[8]
       i35_assign_end: 
                       nop
       i36_while_loop: 
                       bind (2)
                       expr ($immed.[1],r[0],=,0) with ET=L[1]
                       falsejump ($immed.[1],i38_branch)
              i36_fun: 
                       fun eth_ev.await()
          i36_fun_end: 
                       nop
           i37_assign: 
                       move (r,eth_int1)
       i37_assign_end: 
                       jump (i36_while_loop)
   i36_while_loop_end: 
                       nop
           i38_branch: 
                       bind (2)
                       expr ($immed.[1],r[2],=,1) with ET=L[1]
                       falsejump ($immed.[1],i40_assign)
           i39_assign: 
                       move (eth_link_stat,) with ET=L[8]
       i39_assign_end: 
                       jump (i41_assign)
           i40_assign: 
                       move (eth_link_stat,) with ET=L[8]
       i40_assign_end: 
                       nop
       i38_branch_end: 
                       nop
           i41_assign: 
                       move (eth_stat,) with ET=L[8]
       i41_assign_end: 
end
