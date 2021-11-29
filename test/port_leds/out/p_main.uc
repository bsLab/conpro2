-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2009 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D104 Genetic size: 2277919
--         Compile date: Tue Sep  1 13:50:50 CEST 2009
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  object pt: Ioport.ioport
  object p1: Process.Process
end

code:
begin
             i1_fun: 
                     fun (pt.init,())
         i1_fun_end: 
                     nop
             i2_fun: 
                     fun (p1.start,())
         i2_fun_end: 
end
