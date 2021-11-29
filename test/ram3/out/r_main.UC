modules:
begin
  module R: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Ram: IMPORT
end

import:
begin
  register d: I[8]
  object p1: Process.process
  object p2: Process.process
end

types:
begin
  type Process.process: {
      method start ()
      method stop ()
      method call ()
    }
end

code:
begin
            i1_assign: 
                       move (d,1) with ET=I[8]
        i1_assign_end: 
                       nop
               i2_fun: 
                       fun p1.call()
           i2_fun_end: 
                       nop
            i3_assign: 
                       move (d,2) with ET=I[8]
        i3_assign_end: 
                       nop
               i4_fun: 
                       fun p2.call()
           i4_fun_end: 
                       nop
            i5_assign: 
                       move (d,0) with ET=I[8]
        i5_assign_end: 
end
