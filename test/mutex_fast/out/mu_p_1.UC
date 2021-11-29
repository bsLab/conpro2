modules:
begin
  module Mu: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Mutex: IMPORT
  module System: IMPORT
end

import:
begin
  array d: register[4] of L[1] :: {
             d_0,d_1,d_2,d_3}
  object mu: Mutex.mutex
end

data:
begin
  register LOOP_i_1: I[4]
end

types:
begin
  type Mutex.mutex: {
      method init ()
      method lock ()
      method unlock ()
    }
end

code:
begin
            i1_assign: 
                       move (d_1,0) with ET=L[1]
        i1_assign_end: 
                       nop
          i2_for_loop: 
                       move (LOOP_i_1,1) with ET=I[4]
     i2_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],5,>=,LOOP_i_1) with ET=I[4]
                       falsejump ($immed.[1],%END)
               i3_fun: 
                       fun mu.lock()
           i3_fun_end: 
                       nop
            i4_assign: 
                       move (d_1,1) with ET=L[1]
        i4_assign_end: 
                       nop
               i5_fun: 
                       fun mu.unlock()
           i5_fun_end: 
                       nop
            i6_assign: 
                       move (d_1,0) with ET=L[1]
        i6_assign_end: 
                       nop
     i2_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_1,LOOP_i_1,+,1) with ET=I[4]
                       nop
                       jump (i2_for_loop_cond)
      i2_for_loop_end: 
end
