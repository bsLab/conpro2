-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2009 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D112-M4 Genetic size: 2326415
--         Compile date: Wed Sep 30 09:43:14 CEST 2009
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  register x: I8
end

data:
begin
  register d1: B
  register d2: B
end

code:
begin
          i1_assign: 
                     move (d1,$true)
      i1_assign_end: 
                     nop
          i2_assign: 
                     move (d2,$true)
      i2_assign_end: 
                     nop
      i3_while_loop: 
                     bind (2)
                     expr ($immed.[1],d1,=,$true)
                     falsejump ($immed.[1],%END)
      i4_while_loop: 
                     bind (2)
                     expr ($immed.[1],d2,=,$true)
                     falsejump ($immed.[1],i7_assign)
          i5_assign: 
                     expr (x,x,+,1)
                     nop
      i5_assign_end: 
                     nop
          i6_assign: 
                     move (d2,$false)
      i6_assign_end: 
                     jump (i4_while_loop)
  i4_while_loop_end: 
                     nop
          i7_assign: 
                     move (d1,$false)
      i7_assign_end: 
                     jump (i3_while_loop)
  i3_while_loop_end: 
end
