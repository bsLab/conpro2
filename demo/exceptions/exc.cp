open Core;
open Process;

exception Exit;

reg jg: int[8];
export jg;

process p1:
begin
  reg j: int[8];
  try
  begin
    j <- 0;
    for i = 1 to 10 do
    begin
      j <- j + 2;
      if j = 4 then
        raise Exit;
    end;
    jg <- 0x12;
  end
  with 
  begin
    when Exit: jg <- 0x13; 
  end;
end;

process p2:
begin
  reg j: int[8];
  j <- 0;
  for i = 1 to 10 do
  begin
    j <- j + 2;
    if j = 8 then
      raise Exit;
  end;
  jg <- 0x21;
end;


process main:
begin
  jg <- 0;
  p1.start ();
  try 
  begin
    p2.call ();
  end
  with
  begin
    when Exit: jg <- 0x01;
    when others: jg <- 0x02;
  end;
end;
