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
  register ARG_FUN_div_a: L[16]
  register ARG_FUN_div_b: L[16]
  register RET_FUN_div_z: L[16]
end

data:
begin
  const l0: 0
  register i: L[5]
  register b2: L[32]
  register q: L[32]
  register TEMPS_0: L[32]
end

code:
begin
            i1_assign: 
                       move (q,ARG_FUN_div_a) with ET=L[32]
        i1_assign_end: 
                       nop
            i2_assign: 
                       expr (b2,ARG_FUN_div_b,lsl,16) with ET=L[32]
                       nop
        i2_assign_end: 
                       nop
            i3_assign: 
                       move (i,0) with ET=L[5]
        i3_assign_end: 
                       nop
        i4_while_loop: 
                       bind (2)
                       expr ($immed.[1],i,<,16) with ET=L[5]
                       falsejump ($immed.[1],i9_assign)
         i4_bind_to_6: 
                       bind (7)
                       expr (TEMPS_0,q,lsl,1) with ET=L[32]
                       nop
                       expr ($immed.[2],TEMPS_0,-,b2) with ET=L[32]
                       expr (q,$immed.[2],lor,1) with ET=L[32]
                       nop
                       expr (i,i,+,1) with ET=L[5]
                       nop
     i4_bind_to_6_end: 
                       nop
            i7_branch: 
                       bind (2)
                       expr ($immed.[1],q[31],=,1) with ET=L[1]
                       falsejump ($immed.[1],i4_while_loop)
            i8_assign: 
                       bind (2)
                       expr ($immed.[2],q,+,b2) with ET=L[32]
                       expr (q,$immed.[2],land,4294967294) with ET=L[32]
                       nop
        i8_assign_end: 
                       nop
        i7_branch_end: 
                       jump (i4_while_loop)
    i4_while_loop_end: 
                       nop
            i9_assign: 
                       move (RET_FUN_div_z,q[0 to 15]) with ET=L[16]
        i9_assign_end: 
end
