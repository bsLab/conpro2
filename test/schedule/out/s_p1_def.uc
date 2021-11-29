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
  register y: I8
  register z: I8
  register LOOP_i_0: I5
end

code:
begin
       i1_bind_to_2: 
                     bind (2)
                     move (y,x)
                     move (z,x)
   i1_bind_to_2_end: 
                     nop
        i3_for_loop: 
                     move (LOOP_i_0,1)
   i3_for_loop_cond: 
                     bind (2)
                     expr ($immed.[1],10,>=,LOOP_i_0)
                     falsejump ($immed.[1],i6_assign)
          i4_assign: 
                     expr (y,y,+,1)
                     nop
      i4_assign_end: 
                     nop
          i5_assign: 
                     expr (z,z,+,1)
                     nop
      i5_assign_end: 
                     nop
   i3_for_loop_incr: 
                     bind (3)
                     expr (LOOP_i_0,LOOP_i_0,+,1)
                     nop
                     jump (i3_for_loop_cond)
    i3_for_loop_end: 
                     nop
          i6_assign: 
                     expr (x,y,+,z)
                     nop
      i6_assign_end: 
end
