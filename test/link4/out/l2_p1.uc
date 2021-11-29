-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2011 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D174 Genetic size: 2713135
--         Compile date: Fri Jan 21 09:49:21 CET 2011
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  register PRO_p1_EXCEPTION: I8
  register x: I12
  object ln: Link4.link4
  register xa: I8
end

data:
begin
  register d: L12
  register err: B
  register LOOP_i_0: I5
end

code:
begin
          i1_assign: 
                     move (d,841)
      i1_assign_end: 
                     nop
             i2_try: 
                     nop
        i6_for_loop: 
                     move (LOOP_i_0,1)
   i6_for_loop_cond: 
                     bind (2)
                     expr ($immed.[1],10,>=,LOOP_i_0)
                     falsejump ($immed.[1],i20_assign)
       i7_bind_to_8: 
                     bind (2)
                     move (xa,w)
                     move (x,d:I0)
   i7_bind_to_8_end: 
                     nop
             i9_fun: 
                     fun (ln.write,d,err)
         i9_fun_end: 
                     nop
         i10_branch: 
                     bind (2)
                     expr ($immed.[1],err,=,$true)
                     falsejump ($immed.[1],i13_assign)
          i11_raise: 
                     bind (2)
                     move (PRO_p1_EXCEPTION,1)
                     jump (i3_select)
      i11_raise_end: 
                     nop
     i10_branch_end: 
                     nop
         i13_assign: 
                     move (xa,r)
     i13_assign_end: 
                     nop
            i14_fun: 
                     fun (ln.read,d,err)
        i14_fun_end: 
                     nop
         i15_branch: 
                     bind (2)
                     expr ($immed.[1],err,=,$true)
                     falsejump ($immed.[1],i18_bind_to_19)
          i16_raise: 
                     bind (2)
                     move (PRO_p1_EXCEPTION,1)
                     jump (i3_select)
      i16_raise_end: 
                     nop
     i15_branch_end: 
                     nop
     i18_bind_to_19: 
                     bind (3)
                     move (x,d:I0)
                     expr (d,d,+,1)
                     nop
 i18_bind_to_19_end: 
                     nop
   i6_for_loop_incr: 
                     bind (3)
                     expr (LOOP_i_0,LOOP_i_0,+,1)
                     nop
                     jump (i6_for_loop_cond)
    i6_for_loop_end: 
                     jump (i20_assign)
          i3_select: 
                     bind (4)
          i3_case_1: 
                     expr ($immed.[1],PRO_p1_EXCEPTION,=,1)
                     falsejump ($immed.[1],i20_assign)
                     nop
             i4_fun: 
                     fun (ln.stop,())
         i4_fun_end: 
                     nop
          i5_assign: 
                     move (PRO_p1_EXCEPTION,0)
      i5_assign_end: 
                     nop
      i3_select_end: 
                     jump (i20_assign)
         i2_try_end: 
                     nop
         i20_assign: 
                     move (xa,.)
     i20_assign_end: 
end
