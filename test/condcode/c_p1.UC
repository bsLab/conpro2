modules:
begin
  module C: PARENT
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
end

code:
begin
          i1_for_loop: 
                       move (LOOP_i_0,1) with ET=I[5]
     i1_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],10,>=,LOOP_i_0) with ET=I[5]
                       falsejump ($immed.[1],%END)
            i2_assign: 
                       expr (x,x,+,1) with ET=I[8]
                       nop
        i2_assign_end: 
                       nop
     i1_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_0,LOOP_i_0,+,1) with ET=I[5]
                       nop
                       jump (i1_for_loop_cond)
      i1_for_loop_end: 
end
