-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (C) 2006-2008 ** BSSLAB, Dr. Stefan Bosse
--         Version 2.1 A8 **  2049142 total genes
--         Compiled on Thu Nov 27 11:07:26 CET 2008
--         Compiled by sbosse
--         Compiled on SunOS sun203 5.10 Generic_118833-33 sun4u sparc SUNW,A70

import:
begin
  reg d: I10
  reg e: L12
  reg f: I4
  reg g: L4
  var v: I8 in  in b[0]
  sig x: I10
  sig y: I10
  object rnd1: Random.random
  block b: cells[1] of L8
end

temp:
begin
  reg TEMP_0: I8
end

data:
begin
  reg TEMP_0: I8
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
          i3_assign: 
                     move ($tmp.[v],v)
                     expr (v:L8,$tmp.[v],+,1)
                     nop
      i3_assign_end: 
                     nop
             i4_fun: 
                     fun (rnd1.read,f:L0)
         i4_fun_end: 
                     nop
          i5_assign: 
                     expr (f,f,+,1)
                     nop
      i5_assign_end: 
                     nop
             i6_fun: 
                     fun (rnd1.read,g:L10)
         i6_fun_end: 
                     nop
          i7_assign: 
                     expr (g,g,+,1)
                     nop
      i7_assign_end: 
                     nop
        i8_for_loop: 
                     move (LOOP_i_0,1)
   i8_for_loop_cond: 
                     bind (2)
                     expr ($immed.[1],10,>=,LOOP_i_0)
                     falsejump ($immed.[1],%END)
             i9_fun: 
                     fun (rnd1.read,d:L0)
         i9_fun_end: 
                     nop
     i10_bind_to_11: 
                     bind (2)
                     move (x,LOOP_i_0:I10)
                     move (y,d)
 i10_bind_to_11_end: 
                     nop
   i8_for_loop_incr: 
                     bind (3)
                     expr (LOOP_i_0,LOOP_i_0,+,1)
                     nop
                     jump (i8_for_loop_cond)
    i8_for_loop_end: 
end
