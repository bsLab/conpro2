modules:
begin
  module Com: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Link: IMPORT
  module Random: IMPORT
end

import:
begin
  object send: Process.process
  object rnd: Random.random
  object ln: Link.link
  object recv: Process.process
end

types:
begin
  type Process.process: {
      method start ()
      method stop ()
      method call ()
    }
  type Random.random: {
      method init ()
      method read (L[8]:LHS)
      method seed (N[0]:RHS)
    }
  type Link.link: {
      method init ()
      method start ()
      method stop ()
      method write (L[16]:RHS,B:LHS)
      method read (L[16]:LHS,B:LHS)
      method interface (L[32]:RHS,L[1]:LHS,L[32]:LHS,L[1]:RHS)
    }
end

code:
begin
               i1_fun: 
                       fun rnd.init()
           i1_fun_end: 
                       nop
               i2_fun: 
                       fun ln.init()
           i2_fun_end: 
                       nop
               i3_fun: 
                       fun ln.start()
           i3_fun_end: 
                       nop
               i4_fun: 
                       fun recv.start()
           i4_fun_end: 
                       nop
               i5_fun: 
                       fun send.start()
           i5_fun_end: 
end
