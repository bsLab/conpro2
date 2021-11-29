-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2009 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D114-M15 Genetic size: 2388587
--         Compile date: Wed Oct 21 14:09:57 CEST 2009
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  object send: Process.process
  object rnd: Random.random
  object ln: Link.link
  object recv: Process.process
end

code:
begin
             i1_fun: 
                     fun (rnd.init,())
         i1_fun_end: 
                     nop
             i2_fun: 
                     fun (ln.init,())
         i2_fun_end: 
                     nop
             i3_fun: 
                     fun (ln.start,())
         i3_fun_end: 
                     nop
             i4_fun: 
                     fun (recv.start,())
         i4_fun_end: 
                     nop
             i5_fun: 
                     fun (send.start,())
         i5_fun_end: 
end
