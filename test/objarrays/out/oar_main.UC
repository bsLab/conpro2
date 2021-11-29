modules:
begin
  module Oar: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Mutex: IMPORT
  module Ioport: IMPORT
  module Ram: IMPORT
end

import:
begin
  array iop: object[2] of Ioport.ioport
  array ma: object[2] of Mutex.mutex
  object p1: Process.Process
  object p2: Process.Process
  object p3: Process.Process
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
  type Mutex.mutex: {
      method init ()
      method lock ()
      method unlock ()
    }
end

code:
begin
               i1_fun: 
                       fun ma.[0].init()
           i1_fun_end: 
                       nop
               i2_fun: 
                       fun ma.[1].init()
           i2_fun_end: 
                       nop
               i3_fun: 
                       fun iop.[0].init()
           i3_fun_end: 
                       nop
               i4_fun: 
                       fun iop.[1].init()
           i4_fun_end: 
                       nop
               i5_fun: 
                       fun p1.start()
           i5_fun_end: 
                       nop
               i6_fun: 
                       fun p2.start()
           i6_fun_end: 
                       nop
               i7_fun: 
                       fun p3.start()
           i7_fun_end: 
end
