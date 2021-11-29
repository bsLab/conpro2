modules:
begin
  module L: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Link: IMPORT
  module System: IMPORT
end

import:
begin
  object ln: Link.link
  object p1: Process.Process
  object p2: Process.Process
end

types:
begin
  type Process.Process: {
      method start ()
      method stop ()
      method call ()
    }
  type Link.link: {
      method init ()
      method start ()
      method stop ()
      method write (L[10]:RHS,B:LHS)
      method read (L[10]:LHS,B:LHS)
      method interface (L[20]:RHS,L[1]:LHS,L[20]:LHS,L[1]:RHS)
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
                       nop
               i4_fun: 
                       fun p2.start()
           i4_fun_end: 
end
