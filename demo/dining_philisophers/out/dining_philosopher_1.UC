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
  function eat(n)
  object ev: Event.event
  array thinking: register[5] of L[1] :: {
             thinking_0,thinking_1,thinking_2,thinking_3,thinking_4}
  array fork: object[5] of Semaphore.semaphore
  array eating: register[5] of L[1] :: {
             eating_0,eating_1,eating_2,eating_3,eating_4}
end

temp:
begin
  register TEMP_0: L[4]
end

data:
begin
  register TEMP_0: L[4]
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
               i1_fun: 
                       fun ev.await()
           i1_fun_end: 
                       jump (i3_fun)
              i2_loop: 
                       nop
               i3_fun: 
                       fun fork.[1].down()
           i3_fun_end: 
                       nop
               i4_fun: 
                       fun fork.[2].down()
           i4_fun_end: 
                       nop
         i5_bind_to_6: 
                       bind (2)
                       move (eating_1,1) with ET=L[1]
                       move (thinking_1,0) with ET=L[1]
     i5_bind_to_6_end: 
                       nop
             i7_delay: 
                       move ($tmp.[delay_count],3) with ET=L[4]
        i7_delay_loop: 
                       bind (4)
                       expr ($tmp.[delay_count],$tmp.[delay_count],-,1) with ET=L[4]
                       nop
                       expr ($immed.[1],$tmp.[delay_count],=,0) with ET=L[4]
                       falsejump ($immed.[1],i7_delay_loop)
    i7_delay_loop_end: 
                       jump (i8_bind_to_9)
         i7_delay_end: 
                       nop
         i8_bind_to_9: 
                       bind (2)
                       move (eating_1,0) with ET=L[1]
                       move (thinking_1,1) with ET=L[1]
     i8_bind_to_9_end: 
                       nop
              i10_fun: 
                       fun fork.[1].up()
          i10_fun_end: 
                       nop
              i11_fun: 
                       fun fork.[2].up()
          i11_fun_end: 
                       jump (i3_fun)
          i2_loop_end: 
end
