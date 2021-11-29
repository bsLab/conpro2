-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2010 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D148 Genetic size: 2661823
--         Compile date: Fri Apr 23 17:13:19 CEST 2010
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  function eat(n)
  object ev: Event.event
  array thinking: register[5] of L1 :: {
           thinking_0,thinking_1,thinking_2,thinking_3,thinking_4}
  array fork: object[5] of Semaphore.semaphore
  array eating: register[5] of L1 :: {
           eating_0,eating_1,eating_2,eating_3,eating_4}
end

temp:
begin
  register TEMP_0: L4
end

data:
begin
  register TEMP_0: L4
end

code:
begin
             i1_fun: 
                     fun (ev.await,())
         i1_fun_end: 
                     jump (i3_fun)
            i2_loop: 
                     nop
             i3_fun: 
                     fun (sel[|fork_0|].down,())
         i3_fun_end: 
                     nop
             i4_fun: 
                     fun (sel[|fork_1|].down,())
         i4_fun_end: 
                     nop
       i5_bind_to_6: 
                     bind (2)
                     move (eating_0,1)
                     move (thinking_0,0)
   i5_bind_to_6_end: 
                     nop
           i7_delay: 
                     move ($tmp.[delay_count],3)
      i7_delay_loop: 
                     bind (4)
                     expr ($tmp.[delay_count],$tmp.[delay_count],-,1)
                     nop
                     expr ($immed.[1],$tmp.[delay_count],=,0)
                     falsejump ($immed.[1],i7_delay_loop)
  i7_delay_loop_end: 
                     jump (i8_bind_to_9)
       i7_delay_end: 
                     nop
       i8_bind_to_9: 
                     bind (2)
                     move (eating_0,0)
                     move (thinking_0,1)
   i8_bind_to_9_end: 
                     nop
            i10_fun: 
                     fun (sel[|fork_0|].up,())
        i10_fun_end: 
                     nop
            i11_fun: 
                     fun (sel[|fork_1|].up,())
        i11_fun_end: 
                     jump (i3_fun)
        i2_loop_end: 
end
