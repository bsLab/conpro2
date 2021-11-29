-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2010 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D138 Genetic size: 2566603
--         Compile date: Thu Feb 25 11:39:56 CET 2010
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  array d: register[4] of L1 :: {
           d_0,d_1,d_2,d_3}
  object mu: Mutex.mutex
end

data:
begin
  register LOOP_i_3: I4
end

code:
begin
          i1_assign: 
                     move (d_3,0)
      i1_assign_end: 
                     nop
        i2_for_loop: 
                     move (LOOP_i_3,1)
   i2_for_loop_cond: 
                     bind (2)
                     expr ($immed.[1],5,>=,LOOP_i_3)
                     falsejump ($immed.[1],%END)
             i3_fun: 
                     fun (mu.lock,())
         i3_fun_end: 
                     nop
          i4_assign: 
                     move (d_3,1)
      i4_assign_end: 
                     nop
             i5_fun: 
                     fun (mu.unlock,())
         i5_fun_end: 
                     nop
          i6_assign: 
                     move (d_3,0)
      i6_assign_end: 
                     nop
   i2_for_loop_incr: 
                     bind (3)
                     expr (LOOP_i_3,LOOP_i_3,+,1)
                     nop
                     jump (i2_for_loop_cond)
    i2_for_loop_end: 
end
