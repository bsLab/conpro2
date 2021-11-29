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
  signal dev_eth_int: L[1]
  register eth_int0: L[8]
  register eth_int1: L[8]
  object eth: Mem.mem
  register eth_irq_count: L[8]
  register eth_irq_stat: L[8]
end

temp:
begin
  register TEMP_0: L[6]
end

data:
begin
  register r0: L[8]
  register r1: L[8]
  register TEMP_0: L[6]
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
end

code:
begin
            i1_assign: 
                       move (eth_int0,0) with ET=L[8]
        i1_assign_end: 
                       nop
            i2_assign: 
                       move (eth_int1,0) with ET=L[8]
        i2_assign_end: 
                       jump (i4_assign)
              i3_loop: 
                       nop
            i4_assign: 
                       move (eth_irq_stat,) with ET=L[8]
        i4_assign_end: 
                       nop
           i5_waitfor: 
                       bind (2)
                       expr ($immed.[1],dev_eth_int,=,0) with ET=L[1]
                       falsejump ($immed.[1],i5_waitfor)
       i5_waitfor_end: 
                       nop
        i6_while_loop: 
                       bind (2)
                       expr ($immed.[1],dev_eth_int,=,0) with ET=L[1]
                       falsejump ($immed.[1],i4_assign)
               i6_fun: 
                       fun eth.read(r0,99)
           i6_fun_end: 
                       nop
            i7_branch: 
                       bind (2)
                       expr ($immed.[1],r0,<>,0) with ET=L[8]
                       falsejump ($immed.[1],i9_fun)
            i8_assign: 
                       move (eth_int0,r0)
        i8_assign_end: 
                       nop
        i7_branch_end: 
                       nop
               i9_fun: 
                       fun eth.read(r1,127)
           i9_fun_end: 
                       nop
           i10_branch: 
                       bind (2)
                       expr ($immed.[1],r1,<>,0) with ET=L[8]
                       falsejump ($immed.[1],i12_branch)
           i11_assign: 
                       move (eth_int1,r1)
       i11_assign_end: 
                       nop
       i10_branch_end: 
                       nop
           i12_branch: 
                       bind (4)
                       expr ($immed.[1],r0,<>,0) with ET=L[8]
                       expr ($immed.[2],r1,<>,0) with ET=L[8]
                       expr ($immed.[3],$immed.[1],or,$immed.[2])
                       falsejump ($immed.[3],i15_assign)
              i13_fun: 
                       fun eth_ev.wakeup()
          i13_fun_end: 
                       nop
           i14_assign: 
                       expr (eth_irq_count,eth_irq_count,+,1) with ET=L[8]
                       nop
       i14_assign_end: 
                       nop
       i12_branch_end: 
                       nop
           i15_assign: 
                       move (eth_irq_stat,) with ET=L[8]
       i15_assign_end: 
                       nop
          i16_waitfor: 
                       move ($tmp.[waitfor_count],16)
     i16_waitfor_loop: 
                       bind (4)
                       expr ($tmp.[waitfor_count],$tmp.[waitfor_count],-,1)
                       nop
                       expr ($immed.[1],$tmp.[waitfor_count],=,0)
                       falsejump ($immed.[1],i16_waitfor_loop)
  i16_waitfor_loop_end: 
                       jump (i6_while_loop)
      i16_waitfor_end: 
                       jump (i6_while_loop)
    i6_while_loop_end: 
                       jump (i4_assign)
          i3_loop_end: 
end
