-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2010 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D149 Genetic size: 2681845
--         Compile date: Wed Apr 28 18:22:40 CEST 2010
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  object interpreter: Process.process
  object sys_status_proc: Process.process
  register RET_FUN_request_d2: L8
  register ARG_FUN_request_d1: L8
  function request(d1)
  object LOCK_FUN_request: Mutex.mutex
  object FUN_request: Process.process
end

code:
begin
             i1_fun: 
                     fun (LOCK_FUN_request.init,())
         i1_fun_end: 
                     nop
             i2_fun: 
                     fun (sys_status_proc.start,())
         i2_fun_end: 
                     nop
             i3_fun: 
                     fun (interpreter.start,())
         i3_fun_end: 
                     nop
             i4_fun: 
                     fun (LOCK_FUN_request.lock,())
         i4_fun_end: 
                     nop
          i5_assign: 
                     move (ARG_FUN_request_d1,x)
      i5_assign_end: 
                     nop
             i6_fun: 
                     fun (FUN_request.call,())
         i6_fun_end: 
                     nop
             i7_fun: 
                     fun (LOCK_FUN_request.unlock,())
         i7_fun_end: 
end
