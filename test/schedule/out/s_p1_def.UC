modules:
begin
  module S: PARENT
  module Core: IMPORT
  module Process: IMPORT
end

import:
begin
  register x: I[8]
end

data:
begin
  register y: I[8]
  register z: I[8]
  register LOOP_i_0: I[5]
end

code:
begin
         i1_bind_to_2: 
                       bind (2)
                       move (y,x) with ET=I[8]
                       move (z,x) with ET=I[8]
     i1_bind_to_2_end: 
                       nop
          i3_for_loop: 
                       move (LOOP_i_0,1) with ET=I[5]
     i3_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],10,>=,LOOP_i_0) with ET=I[5]
                       falsejump ($immed.[1],i6_assign)
            i4_assign: 
                       expr (y,y,+,1) with ET=I[8]
                       nop
        i4_assign_end: 
                       nop
            i5_assign: 
                       expr (z,z,+,1) with ET=I[8]
                       nop
        i5_assign_end: 
                       nop
     i3_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_0,LOOP_i_0,+,1) with ET=I[5]
                       nop
                       jump (i3_for_loop_cond)
      i3_for_loop_end: 
                       nop
            i6_assign: 
                       expr (x,y,+,z) with ET=I[8]
                       nop
        i6_assign_end: 
end
