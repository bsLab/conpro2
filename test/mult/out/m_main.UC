modules:
begin
  module M: PARENT
  module Core: IMPORT
  module Process: IMPORT
end

import:
begin
  object p1: Process.process
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
               i1_fun: 
                       fun p1.start()
           i1_fun_end: 
end
