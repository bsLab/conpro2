import:
begin
  object p1: Process.Process
  object rnd1: Random.random
end

code:
begin
               i1_fun: 
                       fun (rnd1.init,)
           i1_fun_end: 
                       nop
               i2_fun: 
                       fun (p1.start,)
           i2_fun_end: 
end
