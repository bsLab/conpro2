-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (C) 2006-2008 ** BSSLAB, Dr. Stefan Bosse
--         Version 2.1 A16 **  2049142 total genes
--         Compiled on Thu Nov 27 11:07:26 CET 2008
--         Compiled by sbosse
--         Compiled on SunOS sun203 5.10 Generic_118833-33 sun4u sparc SUNW,A70

import:
begin
  array p: object[4] of Process.process
  object b: Barrier.barrier
end

data:
begin
  reg LOOP_i_4: I4
end

code:
begin
             i1_fun: 
                     fun (b.init,())
         i1_fun_end: 
                     nop
        i2_for_loop: 
                     move (LOOP_i_4,0)
   i2_for_loop_cond: 
                     bind (2)
                     expr ($immed.[1],3,>=,LOOP_i_4)
                     falsejump ($immed.[1],%END)
             i3_fun: 
                     fun (sel[|p_0;...|].start,())
         i3_fun_end: 
                     nop
   i2_for_loop_incr: 
                     bind (3)
                     expr (LOOP_i_4,LOOP_i_4,+,1)
                     nop
                     jump (i2_for_loop_cond)
    i2_for_loop_end: 
end
