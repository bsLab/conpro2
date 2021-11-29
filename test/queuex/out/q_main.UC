modules:
begin
  module Q: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Queuex: IMPORT
  module System: IMPORT
end

import:
begin
  object q: Queuex.queuex
  object p1: Process.process
  object p2: Process.process
end

types:
begin
  type Process.process: {
      method start ()
      method stop ()
      method call ()
    }
  type Queuex.queuex: {
      method init ()
      method read (L[8]:RHS)
      method write (L[8]:LHS)
    }
end

code:
begin
               i1_fun: 
                       fun q.init()
           i1_fun_end: 
                       nop
               i2_fun: 
                       fun p1.start()
           i2_fun_end: 
                       nop
               i3_fun: 
                       fun p2.start()
           i3_fun_end: 
end
