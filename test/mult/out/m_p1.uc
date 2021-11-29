-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2009 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D113-M11 Genetic size: 2373756
--         Compile date: Mon Oct 12 14:08:33 CEST 2009
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  register x: I8
end

data:
begin
  register LOOP_i_0: I5
  register a: I8
end

code:
begin
          i1_assign: 
                     move (a,x)
      i1_assign_end: 
                     nop
        i2_for_loop: 
                     move (LOOP_i_0,1)
   i2_for_loop_cond: 
                     bind (2)
                     expr ($immed.[1],10,>=,LOOP_i_0)
                     falsejump ($immed.[1],i4_assign)
          i3_assign: 
                     expr (a,a,*,2)
                     nop
      i3_assign_end: 
                     nop
   i2_for_loop_incr: 
                     bind (3)
                     expr (LOOP_i_0,LOOP_i_0,+,1)
                     nop
                     jump (i2_for_loop_cond)
    i2_for_loop_end: 
                     nop
          i4_assign: 
                     move (x,a)
      i4_assign_end: 
end
