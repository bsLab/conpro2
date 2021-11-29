modules:
begin
  module S: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Semaphore: IMPORT
  module System: IMPORT
end

import:
begin
  register d: I[8]
  object s: Semaphore.semaphore
  register x: I[8]
  register y: I[8]
  register z: I[8]
end

data:
begin
  register LOOP_i_3: I[5]
end

types:
begin
  type Semaphore.semaphore: {
      method init ()
      method down ()
      method up ()
      method read ()
      method write ()
    }
end

code:
begin
          i1_for_loop: 
                       move (LOOP_i_3,1) with ET=I[5]
     i1_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],10,>=,LOOP_i_3) with ET=I[5]
                       falsejump ($immed.[1],%END)
               i2_fun: 
                       fun s.down()
           i2_fun_end: 
                       nop
            i3_assign: 
                       move (d,4) with ET=I[8]
        i3_assign_end: 
                       nop
            i4_assign: 
                       expr (x,4,+,x) with ET=I[8]
                       nop
        i4_assign_end: 
                       nop
            i5_assign: 
                       expr (y,4,+,y) with ET=I[8]
                       nop
        i5_assign_end: 
                       nop
            i6_assign: 
                       expr (z,4,+,z) with ET=I[8]
                       nop
        i6_assign_end: 
                       nop
               i7_fun: 
                       fun s.up()
           i7_fun_end: 
                       nop
     i1_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_3,LOOP_i_3,+,1) with ET=I[5]
                       nop
                       jump (i1_for_loop_cond)
      i1_for_loop_end: 
end
