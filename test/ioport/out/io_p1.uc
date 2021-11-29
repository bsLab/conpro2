-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2009 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D110 Genetic size: 2287819
--         Compile date: Sun Sep 20 15:22:22 CEST 2009
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunblk 5.9 Generic_118558-11 sun4u sparc SUNW,Sun-Blade-1500

import:
begin
  object iop: Ioport.ioport
  object rnd: Random.random
end

data:
begin
  reg d: L8
  reg LOOP_i_0: I5
end

code:
begin
        i1_for_loop: 
                     move (LOOP_i_0,1)
   i1_for_loop_cond: 
                     bind (2)
                     expr ($immed.[1],8,>=,LOOP_i_0)
                     falsejump ($immed.[1],%END)
             i2_fun: 
                     fun (rnd.read,d)
         i2_fun_end: 
                     nop
             i3_fun: 
                     fun (iop.write,d)
         i3_fun_end: 
                     nop
   i1_for_loop_incr: 
                     bind (3)
                     expr (LOOP_i_0,LOOP_i_0,+,1)
                     nop
                     jump (i1_for_loop_cond)
    i1_for_loop_end: 
end
