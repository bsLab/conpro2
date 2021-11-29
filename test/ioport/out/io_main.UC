modules:
begin
  module Io: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Ioport: IMPORT
  module Random: IMPORT
end

import:
begin
  object iop: Ioport.ioport
  object rnd: Random.random
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
  type Random.random: {
      method init ()
      method read (L[8]:LHS)
      method seed (N[0]:RHS)
    }
end

code:
begin
               i1_fun: 
                       fun iop.init()
           i1_fun_end: 
                       nop
               i2_fun: 
                       fun iop.dir(255)
           i2_fun_end: 
                       jump (%END)
               i4_fun: 
                       fun rnd.init()
           i4_fun_end: 
                       nop
               i5_fun: 
                       fun p1.start()
           i5_fun_end: 
end
