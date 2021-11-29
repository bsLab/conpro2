-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2009 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D104 Genetic size: 2277919
--         Compile date: Tue Sep  1 13:50:50 CEST 2009
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  object pt: Ioport.ioport
end

temp:
begin
  reg TEMP_0: L26
end

data:
begin
  reg d: L8
  reg TEMP_0: L26
  reg LOOP_i_0: I5
end

code:
begin
             i1_fun: 
                     fun (pt.dir,11111111)
         i1_fun_end: 
                     jump (i3_for_loop)
            i2_loop: 
                     nop
        i3_for_loop: 
                     move (LOOP_i_0,0)
   i3_for_loop_cond: 
                     bind (2)
                     expr ($immed.[1],7,>=,LOOP_i_0)
                     falsejump ($immed.[1],i3_for_loop)
          i4_assign: 
                     move (d,0)
      i4_assign_end: 
                     nop
          i5_assign: 
                     move (d:L1,1)
      i5_assign_end: 
                     nop
             i6_fun: 
                     fun (pt.write,d)
         i6_fun_end: 
                     nop
         i7_waitfor: 
                     move ($tmp.[waitfor_count],18518516)
    i7_waitfor_loop: 
                     bind (4)
                     expr ($tmp.[waitfor_count],$tmp.[waitfor_count],-,1)
                     nop
                     expr ($immed.[1],$tmp.[waitfor_count],=,0)
                     falsejump ($immed.[1],i7_waitfor_loop)
i7_waitfor_loop_end: 
                     jump (i3_for_loop_incr)
     i7_waitfor_end: 
                     nop
   i3_for_loop_incr: 
                     bind (3)
                     expr (LOOP_i_0,LOOP_i_0,+,1)
                     nop
                     jump (i3_for_loop_cond)
    i3_for_loop_end: 
                     jump (i3_for_loop)
        i2_loop_end: 
end
