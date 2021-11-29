modules:
begin
  module Com2: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Uart: IMPORT
  module Random: IMPORT
end

import:
begin
  object send: Process.process
  object rnd: Random.random
  object ln: Uart.uart
  object recv: Process.process
end

types:
begin
  type Process.process: {
      method start ()
      method stop ()
      method call ()
    }
  type Uart.uart: {
      method init ()
      method start ()
      method stop ()
      method baud (N[0]:RHS)
      method write (L[8]:RHS,B:LHS)
      method read (L[8]:LHS,B:LHS)
      method interface (L[1]:RHS,L[1]:LHS)
      method level (N[0]:RHS,N[0]:RHS)
    }
  type Random.random: {
      method init ()
      method read (L[8]:LHS)
      method seed (N[0]:RHS)
    }
end

code:
begin
               i1_fun: 
                       fun rnd.init()
           i1_fun_end: 
                       nop
               i2_fun: 
                       fun ln.baud(9600)
           i2_fun_end: 
                       nop
               i3_fun: 
                       fun ln.init()
           i3_fun_end: 
                       nop
               i4_fun: 
                       fun ln.start()
           i4_fun_end: 
                       nop
               i5_fun: 
                       fun recv.start()
           i5_fun_end: 
                       nop
               i6_fun: 
                       fun send.start()
           i6_fun_end: 
end
