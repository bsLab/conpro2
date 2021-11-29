modules:
begin
  module E: PARENT
  module Core: IMPORT
  module Process: IMPORT
end

import:
begin
  register jg: I[8]
  register PRO_p2_EXCEPTION: I[8]
  object p1: Process.Process
  object p2: Process.Process
  register PRO_main_EXCEPTION: I[8]
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
            i1_assign: 
                       move (jg,0) with ET=I[8]
        i1_assign_end: 
                       nop
               i2_fun: 
                       fun p1.start()
           i2_fun_end: 
                       jump (i9_fun)
               i3_try: 
               i9_fun: 
                       fun p2.call()
                       move (PRO_main_EXCEPTION,PRO_p2_EXCEPTION) with ET=I[8]
                       bind (2)
                       expr ($immed.[1],PRO_main_EXCEPTION,=,0) with ET=I[8]
                       falsejump ($immed.[1],i4_select)
           i9_fun_end: 
                       jump (%END)
            i4_select: 
                       bind (6)
            i4_case_1: 
                       expr ($immed.[1],PRO_main_EXCEPTION,=,1) with ET=I[8]
                       falsejump ($immed.[1],i4_case_2)
                       jump (i5_assign)
            i4_case_2: 
                       jump (i7_assign)
            i5_assign: 
                       move (jg,1) with ET=I[8]
        i5_assign_end: 
                       nop
            i6_assign: 
                       move (PRO_main_EXCEPTION,0) with ET=I[8]
        i6_assign_end: 
                       jump (%END)
            i7_assign: 
                       move (jg,2) with ET=I[8]
        i7_assign_end: 
                       nop
            i8_assign: 
                       move (PRO_main_EXCEPTION,0) with ET=I[8]
        i8_assign_end: 
                       nop
        i4_select_end: 
                       jump (%END)
           i3_try_end: 
end
