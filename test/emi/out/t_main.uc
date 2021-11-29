-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2010 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D149 Genetic size: 2681183
--         Compile date: Wed Apr 28 16:29:24 CEST 2010
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  signal rl: L10
  object p1: Process.process
  object f1: F.f
end

code:
begin
             i1_fun: 
                     fun (f1.time,1000)
         i1_fun_end: 
                     jump (%END)
             i3_fun: 
                     fun (f1.init,())
         i3_fun_end: 
                     nop
             i4_fun: 
                     fun (p1.start,())
         i4_fun_end: 
end
