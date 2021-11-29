modules:
begin
  module Par: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Mutex: IMPORT
end

import:
begin
  register ARG_FUN_parity2_x: L[64]
  register RET_FUN_parity2_p: L[1]
end

data:
begin
  register pl: L[1]
  register LOOP_i_1: I[8]
  register xl: L[64]
end

code:
begin
        BLOCKBOUND5_1: 
                       bind (10)
            i1_assign: 
                       move (xl,ARG_FUN_parity2_x) with ET=L[64]
        i1_assign_end: 
                       nop
            i2_assign: 
                       move (pl,0) with ET=L[1]
        i2_assign_end: 
                       nop
          i3_for_loop: 
                       move (LOOP_i_1,0) with ET=I[8]
     i3_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],63,>=,LOOP_i_1) with ET=I[8]
                       falsejump ($immed.[1],BLOCKBOUND1_1)
        BLOCKBOUND3_1: 
                       bind (10)
            i4_assign: 
                       expr (pl,pl,lxor,xl) with ET=L[1]
                       nop
        i4_assign_end: 
     i3_for_loop_incr: 
                       expr (LOOP_i_1,LOOP_i_1,+,1) with ET=I[8]
                       nop
                       jump (i3_for_loop_cond)
      i3_for_loop_end: 
                       nop
        BLOCKBOUND1_1: 
                       bind (3)
            i5_assign: 
                       move (RET_FUN_parity2_p,pl) with ET=L[1]
        i5_assign_end: 
end
