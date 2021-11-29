-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2009 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D115 Genetic size: 2534927
--         Compile date: Fri Oct 23 13:13:50 CEST 2009
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  object send: Process.process
  object rnd: Random.random
  object ln: Uart.uart
  object recv: Process.process
end

code:
begin
             i1_fun: 
                     fun (rnd.init,())
         i1_fun_end: 
                     nop
             i2_fun: 
                     fun (ln.baud,9600)
         i2_fun_end: 
                     nop
             i3_fun: 
                     fun (ln.init,())
         i3_fun_end: 
                     nop
             i4_fun: 
                     fun (ln.start,())
         i4_fun_end: 
                     nop
             i5_fun: 
                     fun (recv.start,())
         i5_fun_end: 
                     nop
             i6_fun: 
                     fun (send.start,())
         i6_fun_end: 
end
