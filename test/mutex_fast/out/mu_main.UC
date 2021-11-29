modules:
begin
  module Mu: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Mutex: IMPORT
  module System: IMPORT
end

import:
begin
  array p: object[4] of Process.process :: {
             p_0,p_1,p_2,p_3}
  object mu: Mutex.mutex
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
                       fun mu.init()
           i1_fun_end: 
                       nop
               i2_fun: 
                       fun p.[0].start()
           i2_fun_end: 
                       nop
               i3_fun: 
                       fun p.[1].start()
           i3_fun_end: 
                       nop
               i4_fun: 
                       fun p.[2].start()
           i4_fun_end: 
                       nop
               i5_fun: 
                       fun p.[3].start()
           i5_fun_end: 
end
