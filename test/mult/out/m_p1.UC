modules:
begin
  module M: PARENT
  module Core: IMPORT
  module Process: IMPORT
end

import:
begin
  register x: I[8]
end

data:
begin
  register LOOP_i_0: I[5]
  register a: I[8]
end

code:
begin
            i1_assign: 
                       move (a,x) with ET=I[8]
        i1_assign_end: 
                       nop
          i2_for_loop: 
                       move (LOOP_i_0,1) with ET=I[5]
     i2_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],10,>=,LOOP_i_0) with ET=I[5]
                       falsejump ($immed.[1],i4_assign)
            i3_assign: 
                       expr (a,a,*,2) with ET=I[8]
                       nop
        i3_assign_end: 
                       nop
     i2_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_0,LOOP_i_0,+,1) with ET=I[5]
                       nop
                       jump (i2_for_loop_cond)
      i2_for_loop_end: 
                       nop
            i4_assign: 
                       move (x,a) with ET=I[8]
        i4_assign_end: 
end
