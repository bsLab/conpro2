-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2009 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D103 Genetic size: 2277029
--         Compile date: Mon Aug 24 17:06:07 CEST 2009
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  object pt1: Ioport.ioport
  object pt2: Ioport.ioport
end

data:
begin
  reg LOOP_i_0: I5
  reg d1: L8
  reg d2: L8
end

code:
begin
             i1_fun: 
                     fun (pt1.dir,00000000)
         i1_fun_end: 
                     nop
             i2_fun: 
                     fun (pt2.dir,11111111)
         i2_fun_end: 
                     nop
             i3_fun: 
                     fun (pt1.read,d1)
         i3_fun_end: 
                     nop
             i4_fun: 
                     fun (pt2.read,d2)
         i4_fun_end: 
                     nop
        i5_for_loop: 
                     move (LOOP_i_0,1)
   i5_for_loop_cond: 
                     bind (2)
                     expr ($immed.[1],8,>=,LOOP_i_0)
                     falsejump ($immed.[1],%END)
          i6_assign: 
                     expr (d1,d1,land,00001111)
                     nop
      i6_assign_end: 
                     nop
          i7_assign: 
                     move (d1[4 to 7]:L4,LOOP_i_0:L0)
      i7_assign_end: 
                     nop
          i8_assign: 
                     move (d2[3]:L1,1)
      i8_assign_end: 
                     nop
             i9_fun: 
                     fun (pt1.write,d1)
         i9_fun_end: 
                     nop
     i10_bind_to_11: 
                     bind (2)
                     fun (pt1.dir,11111000)
                     fun (pt2.write,d2)
 i10_bind_to_11_end: 
                     nop
         i12_assign: 
                     move (d2[3]:L1,0)
     i12_assign_end: 
                     nop
     i13_bind_to_14: 
                     bind (2)
                     fun (pt1.dir,00000000)
                     fun (pt2.write,d2)
 i13_bind_to_14_end: 
                     nop
            i15_fun: 
                     fun (pt1.read,d1)
        i15_fun_end: 
                     nop
     i16_while_loop: 
                     bind (2)
                     expr ($immed.[1],d1[0]:L1,=,0)
                     falsejump ($immed.[1],i5_for_loop_incr)
            i16_fun: 
                     fun (pt1.read,d1)
        i16_fun_end: 
                     jump (i16_while_loop)
 i16_while_loop_end: 
                     nop
   i5_for_loop_incr: 
                     bind (3)
                     expr (LOOP_i_0,LOOP_i_0,+,1)
                     nop
                     jump (i5_for_loop_cond)
    i5_for_loop_end: 
end
