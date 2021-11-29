-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (C) 2006-2008 ** BSSLAB, Dr. Stefan Bosse
--         Version 2.1 R1 **  1881579 total genes
--         Compiled on Wed Oct 15 17:09:53 CEST 2008
--         Compiled by sbosse
--         Compiled on SunOS sun203 5.10 Generic_118833-33 sun4u sparc SUNW,A70

import:
begin
  object p1: Process.Process
  object rnd1: Random.random
end

code:
begin
             i1_fun: 
                     fun (rnd1.init,())
         i1_fun_end: 
                     nop
             i2_fun: 
                     fun (p1.start,())
         i2_fun_end: 
end
