modules:
begin
  module Com: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Uart: IMPORT
  module System: IMPORT
  module Event: IMPORT
  module Timer: IMPORT
  module Mutex: IMPORT
end

import:
begin
  object interpreter: Process.process
  object sys_status_proc: Process.process
  register RET_FUN_request_d2: L[8]
  register ARG_FUN_request_d1: L[8]
  function request(d1:L[8])
  object LOCK_FUN_request: Mutex.mutex
  object FUN_request: Process.process
end

types:
begin
  type Process.process: {
      method start ()
      method stop ()
      method call ()
    }
  type Mutex.mutex: {
      method init ()
      method lock ()
      method unlock ()
    }
end

code:
begin
               i1_fun: 
                       fun LOCK_FUN_request.init()
           i1_fun_end: 
                       nop
               i2_fun: 
                       fun sys_status_proc.start()
           i2_fun_end: 
                       nop
               i3_fun: 
                       fun interpreter.start()
           i3_fun_end: 
                       nop
               i4_fun: 
                       fun LOCK_FUN_request.lock()
           i4_fun_end: 
                       nop
            i5_assign: 
                       move (ARG_FUN_request_d1,'x') with ET=L[8]
        i5_assign_end: 
                       nop
               i6_fun: 
                       fun FUN_request.call()
           i6_fun_end: 
                       nop
               i7_fun: 
                       fun LOCK_FUN_request.unlock()
           i7_fun_end: 
end
