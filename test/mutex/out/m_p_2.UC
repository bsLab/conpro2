modules:
begin
  module M: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Mutex: IMPORT
  module System: IMPORT
end

import:
begin
  register d: I[8]
  object m: Mutex.mutex
  register x: I[8]
  register y: I[8]
  register z: I[8]
end

data:
begin
  register LOOP_i_2: I[5]
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
          i1_for_loop: 
                       move (LOOP_i_2,1) with ET=I[5]
     i1_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],10,>=,LOOP_i_2) with ET=I[5]
                       falsejump ($immed.[1],%END)
               i2_fun: 
                       fun m.lock()
           i2_fun_end: 
                       nop
            i3_assign: 
                       move (d,3) with ET=I[8]
        i3_assign_end: 
                       nop
            i4_assign: 
                       expr (x,3,+,x) with ET=I[8]
                       nop
        i4_assign_end: 
                       nop
            i5_assign: 
                       expr (y,3,+,y) with ET=I[8]
                       nop
        i5_assign_end: 
                       nop
            i6_assign: 
                       expr (z,3,+,z) with ET=I[8]
                       nop
        i6_assign_end: 
                       nop
               i7_fun: 
                       fun m.unlock()
           i7_fun_end: 
                       nop
     i1_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_2,LOOP_i_2,+,1) with ET=I[5]
                       nop
                       jump (i1_for_loop_cond)
      i1_for_loop_end: 
end
