modules:
begin
  module S: PARENT
  module Core: IMPORT
  module Process: IMPORT
end

import:
begin
  register x: I[8]
  object p1_opt: Process.process
  object p1_def: Process.process
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
                       move (x,1) with ET=I[8]
        i1_assign_end: 
                       nop
               i2_fun: 
                       fun p1_def.call()
           i2_fun_end: 
                       nop
               i3_fun: 
                       fun p1_opt.call()
           i3_fun_end: 
end
