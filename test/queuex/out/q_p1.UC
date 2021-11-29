modules:
begin
  module Q: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Queuex: IMPORT
  module System: IMPORT
end

import:
begin
  object q: Queuex.queuex
  register d1: L[8]
end

data:
begin
  register d: L[8]
  register LOOP_i_0: I[5]
end

types:
begin
  type Queuex.queuex: {
      method init ()
      method read (L[8]:RHS)
      method write (L[8]:LHS)
    }
end

code:
begin
            i1_assign: 
                       move (d,65) with ET=L[8]
        i1_assign_end: 
                       nop
          i2_for_loop: 
                       move (LOOP_i_0,0) with ET=I[5]
     i2_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],9,>=,LOOP_i_0) with ET=I[5]
                       falsejump ($immed.[1],%END)
            i3_assign: 
                       expr (d,d,+,1) with ET=L[8]
                       nop
        i3_assign_end: 
                       nop
            i4_assign: 
                       move (d1,d) with ET=L[8]
        i4_assign_end: 
                       nop
               i5_fun: 
                       fun q.write(d)
           i5_fun_end: 
                       nop
     i2_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_0,LOOP_i_0,+,1) with ET=I[5]
                       nop
                       jump (i2_for_loop_cond)
      i2_for_loop_end: 
end
