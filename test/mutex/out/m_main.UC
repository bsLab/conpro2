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
  object m: Mutex.mutex
  array p: object[4] of Process.process
  register x: I[8]
  register y: I[8]
  register z: I[8]
end

data:
begin
  register LOOP_i_4: I[4]
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
               i1_fun: 
                       fun m.init()
           i1_fun_end: 
                       nop
            i2_assign: 
                       move (x,0) with ET=I[8]
        i2_assign_end: 
                       nop
            i3_assign: 
                       move (y,0) with ET=I[8]
        i3_assign_end: 
                       nop
            i4_assign: 
                       move (z,0) with ET=I[8]
        i4_assign_end: 
                       nop
          i5_for_loop: 
                       move (LOOP_i_4,0) with ET=I[4]
     i5_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],3,>=,LOOP_i_4) with ET=I[4]
                       falsejump ($immed.[1],%END)
               i6_fun: 
                       fun sel[|p_0;...|].start()
           i6_fun_end: 
                       nop
     i5_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_4,LOOP_i_4,+,1) with ET=I[4]
                       nop
                       jump (i5_for_loop_cond)
      i5_for_loop_end: 
end
