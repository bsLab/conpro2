-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2011 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D174 Genetic size: 2713135
--         Compile date: Fri Jan 21 09:49:21 CET 2011
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  register PRO_p1_EXCEPTION: I8
  register x: I10
  object ln: Link2.link2
  register xa: I8
end

data:
begin
  register d: L10
  register err: B
  register LOOP_i_0: I5
end

code:
begin
             i1_try: 
                     nop
        i5_for_loop: 
                     move (LOOP_i_0,1)
   i5_for_loop_cond: 
                     bind (2)
                     expr ($immed.[1],10,>=,LOOP_i_0)
                     falsejump ($immed.[1],%END)
          i6_assign: 
                     move (xa,r)
      i6_assign_end: 
                     nop
             i7_fun: 
                     fun (ln.read,d,err)
         i7_fun_end: 
                     nop
          i8_assign: 
                     move (xa,.)
      i8_assign_end: 
                     nop
          i9_branch: 
                     bind (2)
                     expr ($immed.[1],err,=,$true)
                     falsejump ($immed.[1],i12_assign)
          i10_raise: 
                     bind (2)
                     move (PRO_p1_EXCEPTION,1)
                     jump (i2_select)
      i10_raise_end: 
                     nop
      i9_branch_end: 
                     nop
         i12_assign: 
                     move (x,d:I0)
     i12_assign_end: 
                     nop
   i5_for_loop_incr: 
                     bind (3)
                     expr (LOOP_i_0,LOOP_i_0,+,1)
                     nop
                     jump (i5_for_loop_cond)
    i5_for_loop_end: 
                     jump (%END)
          i2_select: 
                     bind (4)
          i2_case_1: 
                     expr ($immed.[1],PRO_p1_EXCEPTION,=,1)
                     falsejump ($immed.[1],%END)
                     nop
             i3_fun: 
                     fun (ln.stop,())
         i3_fun_end: 
                     nop
          i4_assign: 
                     move (PRO_p1_EXCEPTION,0)
      i4_assign_end: 
                     nop
      i2_select_end: 
                     jump (%END)
         i1_try_end: 
end
