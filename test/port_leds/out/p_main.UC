modules:
begin
  module P: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Ioport: IMPORT
  module System: IMPORT
end

import:
begin
  object pt: Ioport.ioport
  object p1: Process.Process
end

types:
begin
  type Process.Process: {
      method start ()
      method stop ()
      method call ()
    }
  type Ioport.ioport: {
      method init ()
      method read (L[8]:LHS)
      method write (L[8]:RHS)
      method dir (L[8]:RHS)
      method interface (L[8]:LRHS)
    }
end

code:
begin
               i1_fun: 
                       fun pt.init()
           i1_fun_end: 
                       nop
               i2_fun: 
                       fun p1.start()
           i2_fun_end: 
end
