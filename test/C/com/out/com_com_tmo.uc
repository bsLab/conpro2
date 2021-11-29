-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2010 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D149 Genetic size: 2681845
--         Compile date: Wed Apr 28 18:22:40 CEST 2010
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  register com_timeout: B
  object rx_q: Core.Core
end

temp:
begin
  register TEMP_0: L19
end

data:
begin
  register TEMP_0: L19
end

code:
begin
          i1_assign: 
                     move (com_timeout,$false)
      i1_assign_end: 
                     nop
           i2_delay: 
                     move ($tmp.[delay_count],185183)
      i2_delay_loop: 
                     bind (4)
                     expr ($tmp.[delay_count],$tmp.[delay_count],-,1)
                     nop
                     expr ($immed.[1],$tmp.[delay_count],=,0)
                     falsejump ($immed.[1],i2_delay_loop)
  i2_delay_loop_end: 
                     jump (i3_fun)
       i2_delay_end: 
                     nop
             i3_fun: 
                     fun (rx_q.unlock,())
         i3_fun_end: 
                     nop
          i4_assign: 
                     move (com_timeout,$true)
      i4_assign_end: 
end
