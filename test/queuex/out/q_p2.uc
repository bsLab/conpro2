-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2011 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D175 Genetic size: 2714497
--         Compile date: Fri Apr  1 18:08:25 CEST 2011
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  object q: Queuex.queuex
  register d2: L8
end

data:
begin
  register d: L8
  register LOOP_i_1: I5
end

code:
begin
          i1_assign: 
                     move (d,0)
      i1_assign_end: 
                     nop
        i2_for_loop: 
                     move (LOOP_i_1,0)
   i2_for_loop_cond: 
                     bind (2)
                     expr ($immed.[1],9,>=,LOOP_i_1)
                     falsejump ($immed.[1],%END)
             i3_fun: 
                     fun (q.read,d)
         i3_fun_end: 
                     nop
          i4_assign: 
                     move (d2,d)
      i4_assign_end: 
                     nop
   i2_for_loop_incr: 
                     bind (3)
                     expr (LOOP_i_1,LOOP_i_1,+,1)
                     nop
                     jump (i2_for_loop_cond)
    i2_for_loop_end: 
end
