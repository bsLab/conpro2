-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (C) 2006-2008 ** BSSLAB, Dr. Stefan Bosse
--         Version 2.1 R1 **  1881579 total genes
--         Compiled on Wed Oct 15 17:09:53 CEST 2008
--         Compiled by sbosse
--         Compiled on SunOS sun203 5.10 Generic_118833-33 sun4u sparc SUNW,A70

import:
begin
  reg d: I10
  reg e: L12
  reg f: I4
  reg g: L4
  sig x: I10
  sig y: I10
  object rnd1: Random.random
end

data:
begin
  reg LOOP_i_0: I5
end

code:
begin
             i1_fun: 
                     fun (rnd1.read,e:L10)
         i1_fun_end: 
                     nop
          i2_assign: 
                     expr (e,e,+,1)
                     nop
      i2_assign_end: 
                     nop
             i3_fun: 
                     fun (rnd1.read,f:L0)
         i3_fun_end: 
                     nop
          i4_assign: 
                     expr (f,f,+,1)
                     nop
      i4_assign_end: 
                     nop
             i5_fun: 
                     fun (rnd1.read,g:L10)
         i5_fun_end: 
                     nop
          i6_assign: 
                     expr (g,g,+,1)
                     nop
      i6_assign_end: 
                     nop
        i7_for_loop: 
                     move (LOOP_i_0,1)
   i7_for_loop_cond: 
                     bind (2)
                     expr ($immed.[1],10,>=,LOOP_i_0)
                     falsejump ($immed.[1],%END)
             i8_fun: 
                     fun (rnd1.read,d:L0)
         i8_fun_end: 
                     nop
      i9_bind_to_10: 
                     bind (2)
                     move (x,LOOP_i_0:I10)
                     move (y,d)
  i9_bind_to_10_end: 
                     nop
   i7_for_loop_incr: 
                     bind (3)
                     expr (LOOP_i_0,LOOP_i_0,+,1)
                     nop
                     jump (i7_for_loop_cond)
    i7_for_loop_end: 
end
