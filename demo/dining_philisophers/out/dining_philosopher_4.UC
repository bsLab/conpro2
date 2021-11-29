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
              i1_loop: 
                       nop
               i2_fun: 
                       fun fork.[4].down()
           i2_fun_end: 
                       nop
               i3_fun: 
                       fun fork.[0].down()
           i3_fun_end: 
                       nop
         i4_bind_to_5: 
                       bind (2)
                       move (eating_4,1) with ET=L[1]
                       move (thinking_4,0) with ET=L[1]
     i4_bind_to_5_end: 
                       nop
             i6_delay: 
                       move ($tmp.[delay_count],3) with ET=L[4]
        i6_delay_loop: 
                       bind (4)
                       expr ($tmp.[delay_count],$tmp.[delay_count],-,1) with ET=L[4]
                       nop
                       expr ($immed.[1],$tmp.[delay_count],=,0) with ET=L[4]
                       falsejump ($immed.[1],i6_delay_loop)
    i6_delay_loop_end: 
                       jump (i7_bind_to_8)
         i6_delay_end: 
                       nop
         i7_bind_to_8: 
                       bind (2)
                       move (eating_4,0) with ET=L[1]
                       move (thinking_4,1) with ET=L[1]
     i7_bind_to_8_end: 
                       nop
               i9_fun: 
                       fun fork.[4].up()
           i9_fun_end: 
                       nop
              i10_fun: 
                       fun fork.[0].up()
          i10_fun_end: 
                       jump (i2_fun)
          i1_loop_end: 
end
