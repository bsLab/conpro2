modules:
begin
  module Dining: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Semaphore: IMPORT
  module System: IMPORT
  module Event: IMPORT
end

import:
begin
  object init: Process.process
  array philosopher: object[5] of Process.process
  register stat: C8
  object ev: Event.event
end

data:
begin
  register LOOP_i_1: I[4]
end

types:
begin
  type Event.event: {
      method init ()
      method await ()
      method wakeup ()
    }
  type Process.process: {
      method start ()
      method stop ()
      method call ()
    }
end

code:
begin
        BLOCKBOUND8_1: 
                       bind (2)
            i1_assign: 
                       move (stat,'I') with ET=C8
        i1_assign_end: 
                       nop
               i2_fun: 
                       fun init.call()
        BLOCKBOUND6_1: 
                       bind (8)
           i2_fun_end: 
                       nop
            i3_assign: 
                       move (stat,'S') with ET=C8
        i3_assign_end: 
                       nop
          i4_for_loop: 
                       move (LOOP_i_1,0) with ET=I[4]
     i4_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],4,>=,LOOP_i_1) with ET=I[4]
                       falsejump ($immed.[1],BLOCKBOUND3_1)
               i5_fun: 
                       fun philosopher.[LOOP_i_1].start()
           i5_fun_end: 
                       nop
     i4_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_1,LOOP_i_1,+,1) with ET=I[4]
                       nop
                       jump (i4_for_loop_cond)
      i4_for_loop_end: 
                       nop
        BLOCKBOUND3_1: 
                       bind (2)
            i6_assign: 
                       move (stat,'W') with ET=C8
        i6_assign_end: 
                       nop
               i7_fun: 
                       fun ev.wakeup()
end
