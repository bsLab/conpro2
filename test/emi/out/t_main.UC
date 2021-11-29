modules:
begin
  module T: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module F: IMPORT
  module System: IMPORT
  module Ram: IMPORT
end

import:
begin
  signal rl: L[10]
  object p1: Process.process
  object f1: F.f
end

types:
begin
  type Process.process: {
      method start ()
      method stop ()
      method call ()
    }
  type F.f: {
      method init ()
      method read (L[10]:LHS)
      method time (N[0]:RHS)
      method set (L[10]:RHS)
    }
end

code:
begin
               i1_fun: 
                       fun f1.time(1000)
           i1_fun_end: 
                       jump (%END)
               i3_fun: 
                       fun f1.init()
           i3_fun_end: 
                       nop
               i4_fun: 
                       fun p1.start()
           i4_fun_end: 
end
