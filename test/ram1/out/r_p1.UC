modules:
begin
  module R: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module System: IMPORT
  module Ram: IMPORT
end

import:
begin
  register sum: I[16]
  array ar: variable[10] of I[8] in ram1
  block ram1: cells[10] of L[8]
  register dbg: I[8]
end

temp:
begin
  register TEMP_0: I[8]
end

data:
begin
  register TEMPS_0: I[5]
  register TEMP_0: I[8]
  register LOOP_i_0: I[5]
end

code:
begin
        BLOCKBOUND5_1: 
                       bind (10)
            i1_assign: 
                       move (sum,0) with ET=I[16]
        i1_assign_end: 
                       nop
            i2_assign: 
                       move (dbg,-1) with ET=I[8]
        i2_assign_end: 
                       nop
          i3_for_loop: 
                       move (LOOP_i_0,1) with ET=I[5]
     i3_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],10,>=,LOOP_i_0) with ET=I[5]
                       falsejump ($immed.[1],BLOCKBOUND1_1)
        BLOCKBOUND3_1: 
                       bind (8)
            i4_assign: 
                       move (dbg,LOOP_i_0) with ET=I[8]
        i4_assign_end: 
                       nop
            i5_assign: 
                       expr (TEMPS_0,LOOP_i_0,-,1) with ET=I[5]
                       nop
        i5_assign_end: 
        BLOCKBOUND3_2: 
                       bind (3)
                       nop
            i6_assign: 
                       move ($tmp.[ar],ar.[TEMPS_0]) with ET=I[8]
        BLOCKBOUND3_3: 
                       bind (9)
                       expr (sum,sum,+,$tmp.[ar]) with ET=I[16]
                       nop
        i6_assign_end: 
     i3_for_loop_incr: 
                       expr (LOOP_i_0,LOOP_i_0,+,1) with ET=I[5]
                       nop
                       jump (i3_for_loop_cond)
      i3_for_loop_end: 
                       nop
        BLOCKBOUND1_1: 
                       bind (3)
            i7_assign: 
                       move (dbg,-2) with ET=I[8]
        i7_assign_end: 
end
