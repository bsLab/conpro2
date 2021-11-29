-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2009 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D90 Genetic size: 2230944
--         Compile date: Fri Jul 17 13:28:54 CEST 2009
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  reg jg: I8
  reg PRO_p2_EXCEPTION: I8
end

data:
begin
  reg j: I8
  reg LOOP_i_1: I5
end

code:
begin
          i1_assign: 
                     move (j,0)
      i1_assign_end: 
                     nop
        i2_for_loop: 
                     move (LOOP_i_1,1)
   i2_for_loop_cond: 
                     bind (2)
                     expr ($immed.[1],10,>=,LOOP_i_1)
                     falsejump ($immed.[1],i7_assign)
          i3_assign: 
                     expr (j,j,+,2)
                     nop
      i3_assign_end: 
                     nop
          i4_branch: 
                     bind (2)
                     expr ($immed.[1],j,=,8)
                     falsejump ($immed.[1],i2_for_loop_incr)
           i5_raise: 
                     bind (2)
                     move (PRO_p2_EXCEPTION,1)
                     jump (%END)
       i5_raise_end: 
                     nop
      i4_branch_end: 
                     nop
   i2_for_loop_incr: 
                     bind (3)
                     expr (LOOP_i_1,LOOP_i_1,+,1)
                     nop
                     jump (i2_for_loop_cond)
    i2_for_loop_end: 
                     nop
          i7_assign: 
                     move (jg,33)
      i7_assign_end: 
end
