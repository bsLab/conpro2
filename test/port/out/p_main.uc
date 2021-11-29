-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2009 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D103 Genetic size: 2277029
--         Compile date: Mon Aug 24 17:06:07 CEST 2009
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  object p1: Process.Process
  object pt1: Ioport.ioport
  object pt2: Ioport.ioport
end

code:
begin
             i1_fun: 
                     fun (pt1.init,())
         i1_fun_end: 
                     nop
             i2_fun: 
                     fun (pt2.init,())
         i2_fun_end: 
                     nop
             i3_fun: 
                     fun (p1.start,())
         i3_fun_end: 
end
