(*
** ConPro ML implementation library
** Version 2.2
*)

let conpro_err msg = print_string msg; print_newline (); raise Exit 
let conpro_print msg = print_string msg; print_newline ()

let printf fmt = 
  let print chan fmt =
      let fmt = (Obj.magic fmt : string) in
      let len = String.length fmt in
      let fl () = Pervasives.flush chan in
      let rec doprn i =
          if i >= len then fl ();
          if i >= len then 
          begin
            output_char chan '\n';
            Pervasives.flush chan;            
            Obj.magic ();
          end else
          match String.unsafe_get fmt i with
          | '%'  -> Printf.scan_format fmt i cont_s cont_a cont_t
          | '\n' -> output_char chan '\n';
                    Pervasives.flush chan;
                    doprn (succ i);
          |  c   -> output_char chan c; 
                    doprn (succ i)
        and cont_s s i =
          output_string chan s; doprn i
        and cont_a printer arg i =
          printer chan arg; doprn i
        and cont_t printer i =
          printer chan; doprn i
        in 
        doprn 0 in
  print Pervasives.stdout fmt
      
  
let bit a b o =
  let m = String.make (b+1) '0' in
  for i = a to b 
  do
    m.[b-i] <- '1';
  done;
  (o land (int_of_string ("0b"^m))) lsr a
  

let bit64 a b o =
  let m = String.make (b+1) '0' in
  for i = a to b 
  do
    m.[b-i] <- '1';
  done;
  Int64.shift_right (Int64.logand o (Int64.of_string ("0b"^m))) a
  

let bitw a b o e =
  let m = String.make (b+1) '0' in
  let m' = String.make (b-a+1) '1' in
  for i = a to b 
  do
    m.[b-i] <- '1';
  done;
  let m = int_of_string ("0b"^m) in
  let m' = int_of_string ("0b"^m') in
  (o land (lnot m)) lor ((e land m') lsl a)
  
let bitw64 a b o e =
  let m = String.make (b+1) '0' in
  let m' = String.make (b-a+1) '1' in
  for i = a to b 
  do
    m.[b-i] <- '1';
  done;
  let m = Int64.of_string ("0b"^m) in
  let m' = Int64.of_string ("0b"^m') in
  Int64.logor (Int64.logand o (Int64.lognot m)) (Int64.shift_left (Int64.logand e m') a)

let sign n e =
  if (bit (n-1) (n-1) e) = 1 then
  begin
    let v = bit 0 (n-2) e in
    -(bit 0 (n-2) (lnot(v-1)))
  end
  else
    e 
    
let delay millisec = Thread.mdelay millisec
