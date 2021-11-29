modules:
begin
  module L2: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Link4: IMPORT
  module System: IMPORT
end

import:
begin
  object ln: Link4.link4
  object p1: Process.process
end

types:
begin
  type Process.process: {
      method start ()
      method stop ()
      method call ()
    }
  type Link4.link4: {
      method init ()
      method start ()
      method stop ()
      method write (L[12]:RHS,B:LHS)
      method read (L[12]:LHS,B:LHS)
      method interface (L[8]:RHS,L[1]:LHS,L[8]:LHS,L[1]:RHS)
      method select (L[1]:LHS)
    }
end

code:
begin
               i1_fun: 
                       fun ln.init()
           i1_fun_end: 
                       nop
               i2_fun: 
                       fun ln.start()
           i2_fun_end: 
                       nop
               i3_fun: 
                       fun p1.start()
           i3_fun_end: 
end
