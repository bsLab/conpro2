-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2010 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D161 Genetic size: 2703860
--         Compile date: Wed Jun 30 14:12:50 CEST 2010
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  register x: I8
  object p1: Process.process
  object p2: Process.process
end

code:
begin
          i1_assign: 
                     move (x,0)
      i1_assign_end: 
                     nop
             i2_fun: 
                     fun (p1.start,())
         i2_fun_end: 
                     nop
             i3_fun: 
                     fun (p2.start,())
         i3_fun_end: 
end
