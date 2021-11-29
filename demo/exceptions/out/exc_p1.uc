-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2009 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D90 Genetic size: 2230944
--         Compile date: Fri Jul 17 13:28:54 CEST 2009
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  reg PRO_p1_EXCEPTION: I8
  reg jg: I8
end

data:
begin
  reg j: I8
  reg LOOP_i_0: I5
end

code:
begin
             i1_try: 
                     nop
          i6_assign: 
                     move (j,0)
      i6_assign_end: 
                     nop
        i7_for_loop: 
                     move (LOOP_i_0,1)
   i7_for_loop_cond: 
                     bind (2)
                     expr ($immed.[1],10,>=,LOOP_i_0)
                     falsejump ($immed.[1],i12_assign)
          i8_assign: 
                     expr (j,j,+,2)
                     nop
      i8_assign_end: 
                     nop
          i9_branch: 
                     bind (2)
                     expr ($immed.[1],j,=,4)
                     falsejump ($immed.[1],i7_for_loop_incr)
          i10_raise: 
                     bind (2)
                     move (PRO_p1_EXCEPTION,1)
                     jump (i2_select)
      i10_raise_end: 
                     nop
      i9_branch_end: 
                     nop
   i7_for_loop_incr: 
                     bind (3)
                     expr (LOOP_i_0,LOOP_i_0,+,1)
                     nop
                     jump (i7_for_loop_cond)
    i7_for_loop_end: 
                     nop
         i12_assign: 
                     move (jg,18)
     i12_assign_end: 
                     jump (%END)
          i2_select: 
                     bind (6)
          i2_case_1: 
                     expr ($immed.[1],PRO_p1_EXCEPTION,=,1)
                     falsejump ($immed.[1],i2_case_2)
                     jump (i3_assign)
          i2_case_2: 
                     jump (i5_raise)
          i3_assign: 
                     move (jg,19)
      i3_assign_end: 
                     nop
          i4_assign: 
                     move (PRO_p1_EXCEPTION,0)
      i4_assign_end: 
                     jump (%END)
           i5_raise: 
                     bind (2)
                     jump (%END)
       i5_raise_end: 
                     nop
      i2_select_end: 
                     jump (%END)
         i1_try_end: 
end
