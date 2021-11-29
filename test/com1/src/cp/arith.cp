const div_n: value := 16;
const div_n1: value := 15;
const div2_n: value := 32;
const div2_n1: value := 31;

--
-- optimized fast sequentiel division
--
function div (a:logic[div_n],b:logic[div_n]) return(z:logic[div_n]):
begin
  reg q,b2: logic[div2_n];
  reg i: logic[5];
  const l0: logic[1] := 0;

  q <- a;
  b2 <- b lsl div_n;
  i <- 0;

  while i < div_n do
  begin
    begin
      q <- ((q lsl 1)-b2) lor 1;
      i <- i + 1;
    end with bind;
    if q[div2_n1] = 1 then
      q <- (q + b2) land 0xFFFFFFFE;
  end;
  z <- q[0 to div_n1];
end;
