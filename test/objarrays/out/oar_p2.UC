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
  array xa: register[2] of I[8]
end

data:
begin
  register d: L[8]
  register TEMPS_0: I[8]
end

types:
begin
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
                       fun ma.[0].lock()
           i1_fun_end: 
                       nop
            i2_assign: 
                       move (TEMPS_0,xa.[0]) with ET=I[8]
        i2_assign_end: 
                       nop
            i3_assign: 
                       expr (xa.[0],TEMPS_0,+,1) with ET=I[8]
                       nop
        i3_assign_end: 
                       nop
               i4_fun: 
                       fun ma.[0].unlock()
           i4_fun_end: 
                       nop
               i5_fun: 
                       fun ma.[1].lock()
           i5_fun_end: 
                       nop
            i6_assign: 
                       move (TEMPS_0,xa.[1]) with ET=I[8]
        i6_assign_end: 
                       nop
            i7_assign: 
                       expr (xa.[1],TEMPS_0,+,1) with ET=I[8]
                       nop
        i7_assign_end: 
                       nop
               i8_fun: 
                       fun ma.[1].unlock()
           i8_fun_end: 
                       nop
               i9_fun: 
                       fun iop.[0].read(d)
           i9_fun_end: 
                       nop
           i10_assign: 
                       expr (d,d,-,1) with ET=L[8]
                       nop
       i10_assign_end: 
                       nop
              i11_fun: 
                       fun iop.[0].write(d)
          i11_fun_end: 
end
