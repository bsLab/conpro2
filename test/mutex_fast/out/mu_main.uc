-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2010 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D138 Genetic size: 2566603
--         Compile date: Thu Feb 25 11:39:56 CET 2010
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  array p: object[4] of Process.process :: {
           p_0,p_1,p_2,p_3}
  object mu: Mutex.mutex
end

code:
begin
             i1_fun: 
                     fun (mu.init,())
         i1_fun_end: 
                     nop
             i2_fun: 
                     fun (sel[|p_0|].start,())
         i2_fun_end: 
                     nop
             i3_fun: 
                     fun (sel[|p_1|].start,())
         i3_fun_end: 
                     nop
             i4_fun: 
                     fun (sel[|p_2|].start,())
         i4_fun_end: 
                     nop
             i5_fun: 
                     fun (sel[|p_3|].start,())
         i5_fun_end: 
end
