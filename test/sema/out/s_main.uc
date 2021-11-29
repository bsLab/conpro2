-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (C) 2006-2008 ** BSSLAB, Dr. Stefan Bosse
--         Version 2.1 A15 **  2049142 total genes
--         Compiled on Thu Nov 27 11:07:26 CET 2008
--         Compiled by sbosse
--         Compiled on SunOS sun203 5.10 Generic_118833-33 sun4u sparc SUNW,A70

import:
begin
  array p: object[4] of Process.process
  object s: Semaphore.semaphore
  reg x: I8
  reg y: I8
  reg z: I8
end

data:
begin
  reg LOOP_i_4: I4
end

code:
begin
             i1_fun: 
                     fun (s.init,())
         i1_fun_end: 
                     nop
          i2_assign: 
                     move (x,0)
      i2_assign_end: 
                     nop
          i3_assign: 
                     move (y,0)
      i3_assign_end: 
                     nop
          i4_assign: 
                     move (z,0)
      i4_assign_end: 
                     nop
        i5_for_loop: 
                     move (LOOP_i_4,0)
   i5_for_loop_cond: 
                     bind (2)
                     expr ($immed.[1],3,>=,LOOP_i_4)
                     falsejump ($immed.[1],%END)
             i6_fun: 
                     fun (sel[|p_0;...|].start,())
         i6_fun_end: 
                     nop
   i5_for_loop_incr: 
                     bind (3)
                     expr (LOOP_i_4,LOOP_i_4,+,1)
                     nop
                     jump (i5_for_loop_cond)
    i5_for_loop_end: 
end
