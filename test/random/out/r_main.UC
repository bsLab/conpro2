modules:
begin
  module R: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Random: IMPORT
  module System: IMPORT
end

import:
begin
  object p1: Process.Process
  object rnd1: Random.random
end

types:
begin
  type Process.Process: {
      method start ()
      method stop ()
      method call ()
    }
  type Random.random: {
      method init ()
      method read (L[10]:LHS)
      method seed (N[0]:RHS)
    }
end

code:
begin
               i1_fun: 
                       fun rnd1.init()
           i1_fun_end: 
                       nop
               i2_fun: 
                       fun p1.start()
           i2_fun_end: 
end
