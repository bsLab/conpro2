modules:
begin
  module Com1: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Mutex: IMPORT
  module Uart: IMPORT
  module Timer: IMPORT
  module System: IMPORT
end

import:
begin
  object tty_rx: Process.Process
  object tty_link: Uart.uart
  object tty_tx: Process.Process
  object interp: Process.Process
end

types:
begin
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
  type Process.Process: {
      method start ()
      method stop ()
      method call ()
    }
end

code:
begin
               i1_fun: 
                       fun tty_link.init()
           i1_fun_end: 
                       nop
               i2_fun: 
                       fun tty_link.start()
           i2_fun_end: 
                       nop
               i3_fun: 
                       fun tty_tx.start()
           i3_fun_end: 
                       nop
               i4_fun: 
                       fun tty_rx.start()
           i4_fun_end: 
                       nop
               i5_fun: 
                       fun interp.start()
           i5_fun_end: 
end
