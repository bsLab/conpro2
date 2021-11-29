modules:
begin
  module L: PARENT
  module Core: IMPORT
  module Process: IMPORT
end

import:
begin
  register x: I[8]
end

data:
begin
  register d1: B
  register d2: B
end

code:
begin
            i1_assign: 
                       move (d1,true) with ET=B
        i1_assign_end: 
                       nop
            i2_assign: 
                       move (d2,true) with ET=B
        i2_assign_end: 
                       nop
        i3_while_loop: 
                       bind (2)
                       expr ($immed.[1],d1,=,true) with ET=B
                       falsejump ($immed.[1],%END)
        i4_while_loop: 
                       bind (2)
                       expr ($immed.[1],d2,=,true) with ET=B
                       falsejump ($immed.[1],i7_assign)
            i5_assign: 
                       expr (x,x,+,1) with ET=I[8]
                       nop
        i5_assign_end: 
                       nop
            i6_assign: 
                       move (d2,false) with ET=B
        i6_assign_end: 
                       jump (i4_while_loop)
    i4_while_loop_end: 
                       nop
            i7_assign: 
                       move (d1,false) with ET=B
        i7_assign_end: 
                       jump (i3_while_loop)
    i3_while_loop_end: 
end
