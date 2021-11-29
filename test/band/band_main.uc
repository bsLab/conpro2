-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2010 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D151 Genetic size: 2685546
--         Compile date: Mon May  3 17:06:14 CEST 2010
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

data:
begin
  register x: I8
  register y: I8
end

code:
begin
          i1_assign: 
                     move (x,0)
      i1_assign_end: 
                     nop
          i2_assign: 
                     expr (x,x,+,1)
                     nop
      i2_assign_end: 
                     nop
          i3_assign: 
                     move (y,1)
      i3_assign_end: 
                     nop
          i4_assign: 
                     expr (y,x,+,1)
                     nop
      i4_assign_end: 
                     nop
          i5_branch: 
                     bind (4)
                     expr ($immed.[1],x,=,1)
                     expr ($immed.[2],y,=,0)
                     expr ($immed.[3],$immed.[1],and,$immed.[2])
                     falsejump ($immed.[3],%END)
          i6_assign: 
                     expr (x,x,-,1)
                     nop
      i6_assign_end: 
                     nop
      i5_branch_end: 
end
