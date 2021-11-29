-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2010 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D148 Genetic size: 2661823
--         Compile date: Fri Apr 23 17:13:19 CEST 2010
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  object ev: Event.event
  array fork: object[5] of Semaphore.semaphore
end

data:
begin
  register LOOP_i_0: I4
end

code:
begin
      BLOCKBOUND4_1: 
                     bind (2)
        i1_for_loop: 
                     move (LOOP_i_0,0)
   i1_for_loop_cond: 
                     bind (2)
                     expr ($immed.[1],4,>=,LOOP_i_0)
                     falsejump ($immed.[1],i3_fun)
             i2_fun: 
                     fun (sel[|fork_0;...|].init,1)
         i2_fun_end: 
                     nop
   i1_for_loop_incr: 
                     bind (3)
                     expr (LOOP_i_0,LOOP_i_0,+,1)
                     nop
                     jump (i1_for_loop_cond)
    i1_for_loop_end: 
                     nop
             i3_fun: 
                     fun (ev.init,())
         i3_fun_end: 
end
