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
  object ev: Event.event
  array fork: object[5] of Semaphore.semaphore
end

data:
begin
  register LOOP_i_0: I[4]
end

types:
begin
  type Event.event: {
      method init ()
      method await ()
      method wakeup ()
    }
  type Semaphore.semaphore: {
      method init (L[8]:RHS)
      method down ()
      method up ()
      method read ()
      method write ()
    }
end

code:
begin
        BLOCKBOUND4_1: 
                       bind (2)
          i1_for_loop: 
                       move (LOOP_i_0,0) with ET=I[4]
     i1_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],4,>=,LOOP_i_0) with ET=I[4]
                       falsejump ($immed.[1],i3_fun)
               i2_fun: 
                       fun fork.[LOOP_i_0].init(1)
           i2_fun_end: 
                       nop
     i1_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_0,LOOP_i_0,+,1) with ET=I[4]
                       nop
                       jump (i1_for_loop_cond)
      i1_for_loop_end: 
                       nop
               i3_fun: 
                       fun ev.init()
           i3_fun_end: 
end
