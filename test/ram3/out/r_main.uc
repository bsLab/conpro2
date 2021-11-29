-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2009 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D133 Genetic size: 2558806
--         Compile date: Fri Jan  8 10:17:58 CET 2010
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  register d: I8
  object p1: Process.process
  object p2: Process.process
end

code:
begin
          i1_assign: 
                     move (d,1)
      i1_assign_end: 
                     nop
             i2_fun: 
                     fun (p1.call,())
         i2_fun_end: 
                     nop
          i3_assign: 
                     move (d,2)
      i3_assign_end: 
                     nop
             i4_fun: 
                     fun (p2.call,())
         i4_fun_end: 
                     nop
          i5_assign: 
                     move (d,0)
      i5_assign_end: 
end
