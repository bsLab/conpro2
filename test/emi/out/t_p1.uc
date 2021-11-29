-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2010 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D149 Genetic size: 2681183
--         Compile date: Wed Apr 28 16:29:24 CEST 2010
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  register d: I10
  variable v: L8 in  in b[0]
  signal x: I10
  signal y: I10
  object f1: F.f
  block b: cells[1] of L8
end

temp:
begin
  register TEMP_0: L8
end

data:
begin
  register TEMP_0: L8
  register LOOP_i_0: I5
end

code:
begin
             i1_fun: 
                     fun (f1.time,2000)
         i1_fun_end: 
                     nop
             i2_fun: 
                     fun (f1.time,3000)
         i2_fun_end: 
                     nop
             i3_fun: 
                     fun (f1.time,4000)
         i3_fun_end: 
                     nop
          i4_assign: 
                     move ($tmp.[v],v)
                     expr (v,$tmp.[v],+,1)
                     nop
      i4_assign_end: 
                     nop
        i5_for_loop: 
                     move (LOOP_i_0,1)
   i5_for_loop_cond: 
                     bind (2)
                     expr ($immed.[1],10,>=,LOOP_i_0)
                     falsejump ($immed.[1],%END)
             i6_fun: 
                     fun (f1.read,d:L0)
         i6_fun_end: 
                     nop
       i7_bind_to_8: 
                     bind (2)
                     move (x,LOOP_i_0:I10)
                     move (y,d)
   i7_bind_to_8_end: 
                     nop
   i5_for_loop_incr: 
                     bind (3)
                     expr (LOOP_i_0,LOOP_i_0,+,1)
                     nop
                     jump (i5_for_loop_cond)
    i5_for_loop_end: 
end
