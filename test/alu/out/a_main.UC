modules:
begin
  module A: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module System: IMPORT
end

import:
begin
  object p2: Process.Process
end

types:
begin
  type Process.Process: {
      method start ()
      method stop ()
      method call ()
    }
end

code:
begin
               i1_fun: 
                       fun p2.start()
           i1_fun_end: 
end
