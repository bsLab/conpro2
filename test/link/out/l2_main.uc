-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2011 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D174 Genetic size: 2713135
--         Compile date: Fri Jan 21 09:49:21 CET 2011
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  object ln: Link.link
  object p1: Process.process
end

code:
begin
             i1_fun: 
                     fun (ln.init,())
         i1_fun_end: 
                     nop
             i2_fun: 
                     fun (ln.start,())
         i2_fun_end: 
                     nop
             i3_fun: 
                     fun (p1.start,())
         i3_fun_end: 
end
