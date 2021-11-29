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
  object p1: Process.Process
end

code:
begin
             i1_fun: 
                     fun (iop.init,())
         i1_fun_end: 
                     nop
             i2_fun: 
                     fun (iop.dir,255)
         i2_fun_end: 
                     jump (%END)
             i4_fun: 
                     fun (rnd.init,())
         i4_fun_end: 
                     nop
             i5_fun: 
                     fun (p1.start,())
         i5_fun_end: 
end
