modules:
begin
  module P: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Ioport: IMPORT
end

import:
begin
  object p1: Process.Process
  object pt1: Ioport.ioport
  object pt2: Ioport.ioport
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
                       fun pt1.init()
           i1_fun_end: 
                       nop
               i2_fun: 
                       fun pt2.init()
           i2_fun_end: 
                       nop
               i3_fun: 
                       fun p1.start()
           i3_fun_end: 
end
