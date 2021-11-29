-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2009 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D112-M4 Genetic size: 2326415
--         Compile date: Wed Sep 30 09:43:14 CEST 2009
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  object p1: Process.process
end

code:
begin
             i1_fun: 
                     fun (p1.start,())
         i1_fun_end: 
end
