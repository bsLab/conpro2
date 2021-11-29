modules:
begin
  module C: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Link: IMPORT
  module System: IMPORT
  module Clock: IMPORT
end

import:
begin
  register d: L[8]
  object ln: Link.link
  object p1: Process.Process
  object p2: Process.Process
  signal KEY_kb_init: L[1]
  signal KEY_kb_data: L[8]
end

temp:
begin
  register TEMP_0: L[8]
end

data:
begin
  register TEMP_0: L[8]
end

types:
begin
  type Process.Process: {
      method start ()
      method stop ()
      method call ()
    }
  type Link.link: {
      method init ()
      method start ()
      method stop ()
      method write (L[10]:RHS,B:LHS)
      method read (L[10]:LHS,B:LHS)
      method interface (L[20]:RHS,L[1]:LHS,L[20]:LHS,L[1]:RHS)
    }
end

code:
begin
               i1_fun: 
                       fun ln.init()
           i1_fun_end: 
                       nop
               i2_fun: 
                       fun ln.start()
           i2_fun_end: 
                       nop
               i3_fun: 
                       fun p1.start()
           i3_fun_end: 
                       nop
               i4_fun: 
                       fun p2.start()
           i4_fun_end: 
                       nop
           i5_waitfor: 
                       move ($tmp.[waitfor_count],98) with ET=L[8]
      i5_waitfor_loop: 
                       bind (5)
                       move (KEY_kb_init,1) with ET=L[1]
                       expr ($tmp.[waitfor_count],$tmp.[waitfor_count],-,1) with ET=L[8]
                       nop
                       expr ($immed.[1],$tmp.[waitfor_count],=,0) with ET=L[8]
                       falsejump ($immed.[1],i5_waitfor_loop)
  i5_waitfor_loop_end: 
                       jump (i8_assign)
       i5_waitfor_end: 
                       jump (i8_assign)
              i7_loop: 
                       nop
            i8_assign: 
                       move (d,KEY_kb_data) with ET=L[8]
        i8_assign_end: 
                       jump (i8_assign)
          i7_loop_end: 
end
