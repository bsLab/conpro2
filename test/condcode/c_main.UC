modules:
begin
  module C: PARENT
  module Core: IMPORT
  module Process: IMPORT
end

import:
begin
  register x: I[8]
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
                       move (x,0) with ET=I[8]
        i1_assign_end: 
                       nop
               i2_fun: 
                       fun p1.start()
           i2_fun_end: 
                       nop
               i3_fun: 
                       fun p2.start()
           i3_fun_end: 
end
