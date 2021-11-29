-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2011 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D175 Genetic size: 2714497
--         Compile date: Fri Apr  1 18:08:25 CEST 2011
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  object q: Queuex.queuex
  object p1: Process.process
  object p2: Process.process
end

code:
begin
             i1_fun: 
                     fun (q.init,())
         i1_fun_end: 
                     nop
             i2_fun: 
                     fun (p1.start,())
         i2_fun_end: 
                     nop
             i3_fun: 
                     fun (p2.start,())
         i3_fun_end: 
end
