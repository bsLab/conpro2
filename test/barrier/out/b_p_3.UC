modules:
begin
  module B: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Barrier: IMPORT
  module System: IMPORT
end

import:
begin
  array d: register[4] of I[8] :: {
             d_0,d_1,d_2,d_3}
  object b: Barrier.barrier
end

data:
begin
  register LOOP_i_3: I[4]
end

types:
begin
  type Barrier.barrier: {
      method init ()
      method await ()
    }
end

code:
begin
          i1_for_loop: 
                       move (LOOP_i_3,1) with ET=I[4]
     i1_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],5,>=,LOOP_i_3) with ET=I[4]
                       falsejump ($immed.[1],%END)
               i2_fun: 
                       fun b.await()
           i2_fun_end: 
                       nop
            i3_assign: 
                       move (d_3,4) with ET=I[8]
        i3_assign_end: 
                       nop
            i4_assign: 
                       move (d_3,0) with ET=I[8]
        i4_assign_end: 
                       nop
     i1_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_3,LOOP_i_3,+,1) with ET=I[4]
                       nop
                       jump (i1_for_loop_cond)
      i1_for_loop_end: 
end
