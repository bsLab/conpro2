-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2009 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D114-M15 Genetic size: 2388587
--         Compile date: Wed Oct 21 14:09:57 CEST 2009
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  register PRO_send_EXCEPTION: I8
  object rnd: Random.random
  object ln: Link.link
end

data:
begin
  register d: L16
  register r8: L8
  register err: B
  register LOOP_i_0: I8
end

code:
begin
             i1_try: 
                     nop
        i5_for_loop: 
                     move (LOOP_i_0,1)
   i5_for_loop_cond: 
                     bind (2)
                     expr ($immed.[1],64,>=,LOOP_i_0)
                     falsejump ($immed.[1],%END)
             i6_fun: 
                     fun (rnd.read,r8)
         i6_fun_end: 
                     nop
          i7_assign: 
                     move (d[0 to 7]:L8,r8)
      i7_assign_end: 
                     nop
          i8_assign: 
                     move (d[8 to 15]:L8,LOOP_i_0:L0)
      i8_assign_end: 
                     nop
             i9_fun: 
                     fun (ln.write,d,err)
         i9_fun_end: 
                     nop
         i10_branch: 
                     bind (2)
                     expr ($immed.[1],err,=,$true)
                     falsejump ($immed.[1],i5_for_loop_incr)
          i11_raise: 
                     bind (2)
                     move (PRO_send_EXCEPTION,1)
                     jump (i2_select)
      i11_raise_end: 
                     nop
     i10_branch_end: 
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
                     expr ($immed.[1],PRO_send_EXCEPTION,=,1)
                     falsejump ($immed.[1],%END)
                     nop
             i3_fun: 
                     fun (ln.stop,())
         i3_fun_end: 
                     nop
          i4_assign: 
                     move (PRO_send_EXCEPTION,0)
      i4_assign_end: 
                     nop
      i2_select_end: 
                     jump (%END)
         i1_try_end: 
end
