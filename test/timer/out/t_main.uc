-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (C) 2006-2008 ** BSSLAB, Dr. Stefan Bosse
--         Version 2.1 D18 **  2049142 total genes
--         Compiled on Thu Nov 27 11:07:26 CET 2008
--         Compiled by sbosse
--         Compiled on SunOS sun203 5.10 Generic_118833-33 sun4u sparc SUNW,A70

import:
begin
  array p: object[4] of Process.process
  object t: Timer.timer
end

data:
begin
  reg LOOP_i_4: I4
end

code:
begin
             i1_fun: 
                     fun (t.init,())
         i1_fun_end: 
                     nop
             i2_fun: 
                     fun (t.time,2 Usec)
         i2_fun_end: 
                     nop
        i3_for_loop: 
                     move (LOOP_i_4,0)
   i3_for_loop_cond: 
                     bind (2)
                     expr ($immed.[1],3,>=,LOOP_i_4)
                     falsejump ($immed.[1],i5_fun)
             i4_fun: 
                     fun (sel[|p_0;...|].start,())
         i4_fun_end: 
                     nop
   i3_for_loop_incr: 
                     bind (3)
                     expr (LOOP_i_4,LOOP_i_4,+,1)
                     nop
                     jump (i3_for_loop_cond)
    i3_for_loop_end: 
                     nop
             i5_fun: 
                     fun (t.mode,0)
         i5_fun_end: 
                     nop
             i6_fun: 
                     fun (t.start,())
         i6_fun_end: 
end
