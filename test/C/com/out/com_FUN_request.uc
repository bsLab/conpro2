-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2010 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D149 Genetic size: 2681845
--         Compile date: Wed Apr 28 18:22:40 CEST 2010
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  register RET_FUN_request_d2: L8
  register ARG_FUN_request_d1: L8
  queue tx_q: L8
  object rep: Event.event
  register rep_d: L8
end

code:
begin
          i1_assign: 
                     move (tx_q,R)
      i1_assign_end: 
                     nop
          i2_assign: 
                     move (tx_q,ARG_FUN_request_d1)
      i2_assign_end: 
                     nop
             i3_fun: 
                     fun (rep.await,())
         i3_fun_end: 
                     nop
          i4_assign: 
                     move (RET_FUN_request_d2,rep_d)
      i4_assign_end: 
end
