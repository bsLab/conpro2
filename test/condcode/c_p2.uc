-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2010 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D161 Genetic size: 2703860
--         Compile date: Wed Jun 30 14:12:50 CEST 2010
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  register x: I8
end

data:
begin
  register LOOP_i_1: I5
end

code:
begin
        i1_for_loop: 
                     move (LOOP_i_1,1)
   i1_for_loop_cond: 
                     bind (2)
                     expr ($immed.[1],10,>=,LOOP_i_1)
                     falsejump ($immed.[1],%END)
          i2_assign: 
                     expr (x,x,-,1)
                     nop
      i2_assign_end: 
                     nop
   i1_for_loop_incr: 
                     bind (3)
                     expr (LOOP_i_1,LOOP_i_1,+,1)
                     nop
                     jump (i1_for_loop_cond)
    i1_for_loop_end: 
end
