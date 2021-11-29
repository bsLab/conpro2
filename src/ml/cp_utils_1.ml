(*
**      ==================================
**      OOOO   OOOO OOOO  O      O   OOOO
**      O   O  O    O     O     O O  O   O
**      O   O  O    O     O     O O  O   O
**      OOOO   OOOO OOOO  O     OOO  OOOO
**      O   O     O    O  O    O   O O   O
**      O   O     O    O  O    O   O O   O
**      OOOO   OOOO OOOO  OOOO O   O OOOO
**      ================================== 
**      BSSLAB, Dr. Stefan Bosse, http://www.bsslab.de
**
**      COPYRIGHT: THIS SOFTWARE, EXECUTABLE AND SOURCE CODE IS OWNED BY BSSLAB.
**                 THIS SOURCE CODE MAY NOT BE COPIED, EXTRACTED,
**                 MODIFIED, OR OTHERWISE USED IN A CONTEXT
**                 OUTSIDE OF THE SOFTWARE SYSTEM.
**
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2006-2009 BSSLAB
**    $CREATED:     16.4.2006
**    $VERSION:     2.09
**
**    $INFO:
**
**  Utils
**
**    $ENDOFINFO
**
*)

open Unix
open Printf

open Cp_syntax
open Cp_types
open Cp_symbol
open Cp_common
open Cp_vhdl
open Cp_print
let rec print_value va =
  let ast_sprint_timeunit vunit =
      match vunit with
      | Nsec -> "Nsec";
      | Usec -> "Usec";
      | Msec -> "Msec";
      | Sec -> "Sec";
      | Cycles -> "Cycles" in

  let ast_sprint_frequnit vunit =
      match vunit with
      | Ghz -> "Ghz";
      | Mhz -> "Mhz";
      | Khz -> "Khz";
      | Hz -> "Hz" in
      
  match va with
  | V_int i64 -> sprintf "V_int(%s)" (Int64.to_string i64)
  | V_float flt -> sprintf "V_float(%f)" flt
  | V_string str -> sprintf "V_string(%s)" str
  | V_char ch -> sprintf "V_char(%c)" ch
  | V_logic lstr -> sprintf "V_logic(%s)" lstr
  | V_bool bo -> sprintf "V_bool(%b)" bo
  | V_list vl -> sprintf "V_list[%s]" (hlist print_value vl);
  | V_z -> "V_z"
  | V_time (i64,timeunit) -> sprintf "V_time(%s %s)" (Int64.to_string i64) (ast_sprint_timeunit timeunit)  
  | V_freq (i64,frequnit) -> sprintf "V_freq(%s %s)" (Int64.to_string i64) (ast_sprint_frequnit frequnit)
  | V_null -> "V_null"

let expr_type_of_str str =
  match str with
  | "shared" -> EXPR_alu
  | "binary" -> EXPR_binary
  | "flat" -> EXPR_flat
  | "top" -> EXPR_top
  | _ -> error 0 (sprintf "Unknown expression type <%s> found [flat,binary,shared]." str)
  
let str_of_expr_type et =
  match et with 
  | EXPR_alu -> "alu"
  | EXPR_binary -> "binary"
  | EXPR_flat -> "flat"
  | EXPR_top -> "top"
  
  
let sprint_dt dt =
    match dt with
    | DT_logic n -> sprintf "DT_logic %d" n;
    | DT_int n -> sprintf "DT_int %d" n;
    | DT_string n -> sprintf "DT_string %d" n;
    | DT_char -> "DT_char";
    | DT_bool -> "DT_bool";
    | DT_object str -> sprintf "DT_object %s" str;
    | DT_lneg -> "DT_lneg";
    | DT_aneg -> "DT_aneg";
    | DT_bneg -> "DT_bneg";
    | DT_natural  n -> sprintf "DT_natural %d" n
    
let print_dt dt = out (sprint_dt dt)

let rec sum l =
    match l with
    | hd::tl -> hd + (sum tl);
    | [] -> 0

let suma a = sum (Array.to_list a)

let rec prod l =
    match l with
    | hd::tl -> hd * (prod tl);
    | [] -> 1

let proda a = prod (Array.to_list a)



let source () = an.a_curpos
let line src = 
    debug "line" with (print_src an.a_curpos);
    an.a_curpos <- src

let i64 = Int64.of_int
let neg_i64 x =
    (Int64.to_string x).[0] = '-'

let pos_i64 x = not (neg_i64 x)

(*
** Compute clock cycles from time value
*)
let cyc_of_time time64 unit =
    let z64 = Int64.zero in
    let time = Int64.to_string time64 in
    match unit with
    | Cycles -> time64;
    | Nsec -> 
        let t64 = time64 in
        let p64 = Int64.div (Int64.of_string "1000000000") (get_env_int64 "clock") in
        let c = Int64.div t64 p64 in
        if c = z64 then error 0 
                    (sprintf "Time value %s ns results in 0 clock cycles! (clock period=%d ns)"
                             time (Int64.to_int p64));
        c
    | Usec -> 
        let t64 = Int64.mul time64 (Int64.of_int 1000) in
        let p64 = Int64.div (Int64.of_string "1000000000") (get_env_int64 "clock") in
        let c = Int64.div t64 p64 in
        if c = z64 then error 0 
                    (sprintf "Time value %s us results in 0 clock cycles! (clock period=%d ns)"
                             time (Int64.to_int p64));
        c
    | Msec -> 
        let t64 = Int64.mul time64 (Int64.of_int 1000000) in
        let p64 = Int64.div (Int64.of_string "1000000000") (get_env_int64 "clock") in
        let c = Int64.div t64 p64 in
        if c = z64 then error 0 
                    (sprintf "Time value %s ms results in 0 clock cycles! (clock period=%d ns)"
                             time (Int64.to_int p64));
        c
    | Sec -> 
        let t64 = Int64.mul  time64 (Int64.of_int 1000000000) in
        let p64 = Int64.div (Int64.of_string "1000000000") (get_env_int64 "clock") in
        let c = Int64.div t64 p64 in
        if c = z64 then error 0 
                    (sprintf "Time value %s ms results in 0 clock cycles! (clock period=%d ns)"
                             time (Int64.to_int p64));
        c

(*
** Return clock period in ns
*)
let per_of_freq hz64 unit =
  match unit with
  | Hz ->
    let p64 = Int64.div (Int64.of_string "1000000000") hz64 in
    p64;
  | Khz ->
    let p64 = Int64.div (Int64.of_string "1000000000") 
                        (Int64.mul hz64 (Int64.of_string "1000"))in
    p64;    
  | Mhz ->
    let p64 = Int64.div (Int64.of_string "1000000000") 
                        (Int64.mul hz64 (Int64.of_string "1000000"))in
    p64;    
  | Ghz ->
    let p64 = Int64.div (Int64.of_string "1000000000") 
                        (Int64.mul hz64 (Int64.of_string "1000000000"))in
    p64


(*
** Return binary size (# of bits) of constant value
*)
let rec const_width v = 
  let log2_64 x =
    let rec log2 x y = 
     if x < 0L1 then y 
      else log2 (Int64.div x 0L2) (Int64.add y 0L1) in
   log2 (Int64.sub x 0L1) 0L0 in
   
    match v with
    | V_int d ->
        let d = Int64.abs d in
        (*
        ** Signed value requires 1 additional bit!
        *)
        if d > 0L0 then ((Int64.to_int (log2_64 (Int64.add d 0L1)))+1) else 2
    | V_float _ -> 32;      (* ?? *)
    | V_char _ -> 8;
    | V_bool _ -> 1;
    | V_logic str -> (String.length str);
    | V_null -> 0;
    | V_z -> 0;
    | V_time (v64,u) -> const_width (V_int (cyc_of_time v64 u));
    | V_freq (v64,u) -> const_width (V_int (per_of_freq v64 u));
    | V_string str -> (String.length str)*8     (* ?? *)
    | V_list cl -> let n = ref 0 in List.iter (fun v -> n := !n + const_width v) cl; !n 

let rec dt_of_val v =
    match v with
    | V_int _ -> DT_int (const_width v);
    | V_logic _ -> DT_logic (const_width v);
    | V_string s -> DT_string (String.length s);
    | V_char _ -> DT_char;
    | V_bool _ -> DT_bool
    | V_z -> DT_logic 1;
    | V_list vl ->
    begin
      (*
      ** All list values should be of same type!
      *)
      let ldt = ref None in
      let err () = error 0 (sprintf "Unsupported type in value list.") in
      let add dt' =
        match !ldt with
        | Some dt'' -> 
        begin
          match dt'' with
          | DT_logic n'' ->
          begin
            match dt' with
            | DT_logic n' -> ldt := Some (DT_logic (max n' n''));
            | _ -> err (); 
          end; 
          | DT_int n'' ->
          begin
            match dt' with
            | DT_int n' -> ldt := Some (DT_int (max n' n''));
            | _ -> err ();
          end;
          | DT_string n'' ->
          begin
            match dt' with
            | DT_string n' -> ldt := Some (DT_string (max n' n''));
            | _ -> err ();
          end;
          | DT_char  ->
          begin
            match dt' with
            | DT_char  -> ldt := Some (DT_char);
            | _ -> err ();
          end;
          | DT_bool  ->
          begin
            match dt' with
            | DT_bool -> ldt := Some (DT_bool);
            | _ -> err ();
          end;
          | _ -> err ();  
        end;
        | None -> ldt := Some dt' in
        
      List.iter (fun v' ->
        let dt' = dt_of_val v' in
        add dt';
        ) vl;
      get_some !ldt   
    end;
    | V_time _ | V_freq _ -> DT_natural (const_width v)
    | _ -> error 0 "dt_of_val: unexpected value type"

    
let int_of_val v =
    match v with
    | V_int i64 -> Int64.to_int i64;
    | V_char c -> int_of_char c
    | V_logic s -> (* only positive values supported *)
      let n = String.length s in
      let mutable iv = 0 in
      for i = n-1 downto 0
      do
        if s.[i] = '1' then iv <- iv lor 1;
        iv <- iv lsl 1;
      done;  
      iv
    | _ -> error 0 "int_of_val: unexpected value type"

let rec str_of_val v =
  match v with
  | V_int i64 -> Int64.to_string i64;
  | V_float f -> string_of_float f;
  | V_string str -> str;
  | V_char c -> sprintf "%c" c;
  | V_logic str -> str;
  | V_bool b -> if b then "1" else "0";
  | V_list strl -> 
    let s = ref "" in
    List.iter (fun s' -> s := sprintf "%s%s" !s (str_of_val s')) strl;
    !s 
  | V_z -> "Z";
  | V_time (i64,tu) -> Int64.to_string i64;
  | V_freq (i64,fu) -> Int64.to_string i64;
  | V_null -> ""

(*
** Create zeros in logic (vector) format
*)
let zeros n =
  if n = 1 then
    "'0'"
  else
  begin
    let str = String.make (n+2) '0' in
    str.[0] <- '"';
    str.[n+1] <- '"';
    str
  end
(*
** Create ones in logic (vector) format
*)
let ones n =
  if n = 1 then
    "'1'"
  else
  begin
    let str = String.make (n+2) '1' in
    str.[0] <- '"';
    str.[n+1] <- '"';
    str
  end

(*
** Convert CONPRO value to VHDL value depending on object data type.
*)
let rec val_str tp v =
    let min_val,max_val = (ref 0),(ref 0) in

    let bits n v =
        if n > 64 then error 756841 "val_str: data width exceeds 64 bit";
        let n' = min n 63 in
        let mask = Int64.sub (Int64.shift_left Int64.one n') Int64.one in
        let v = Int64.logand v mask in
        let s = String.create (n+2) in
        if n <> 1 then
        begin
            s.[0] <- '"';  
            s.[n+1] <- '"';
        end
        else
        begin
            s.[0] <- '\'';  
            s.[n+1] <- '\'';
        end;

        let res = ref v in
        for i = n-1 downto 0
        do
            let div = Int64.shift_left (Int64.one) i in
            let b = Int64.div !res  div in
            if b = (Int64.one) then s.[n-i] <- '1' 
                               else s.[n-i] <- '0';
            res := Int64.sub !res (Int64.mul b div);   
        done;
        s
        in
        
    let bits_2comp bs =
      let carry = ref '1' in
      let bs2 = String.copy bs in
      let n = String.length bs in
      for i = 1 to n-2 
      do
        if bs2.[i] = '1' 
          then bs2.[i] <- '0' 
          else bs2.[i] <- '1';
      done; 
      for i = n-2 downto 1 
      do
        if bs2.[i] = '0' then 
        begin
          bs2.[i] <- !carry;
          carry := '0';
        end
        else
        begin
          if !carry = '1' then
            bs2.[i] <- '0';
        end;
      done;
      bs2 in
      
    let zets n =
        if n > 64 then error 756842 "val_str: data width exceeds 64 bit";
        let s = String.create (n+2) in
        if n <> 1 then
        begin
            s.[0] <- '"';  
            s.[n+1] <- '"';
        end
        else
        begin
            s.[0] <- '\'';  
            s.[n+1] <- '\'';
        end;
        for i = 1 to n
        do
            s.[i] <- 'Z';
        done;
        s
        in

    let bit_str n v =
        match v with
        | V_int v -> 
          if v >= Int64.zero then bits n v else
          begin
            let v' = Int64.abs v in
            let b = bits n v' in
            (*
            ** Build two-complement coding...
            *)
            bits_2comp b;
          end;
        | V_char c -> bits n (i64 (int_of_char c));
        | V_logic s -> 
          let sn = String.length s in
          if sn = n then
          begin
            if sn > 1 then sprintf "\"%s\"" s
                      else sprintf "'%s'" s;
          end else if sn < n then
          begin
            if sn > 1 then sprintf "%s & \"%s\"" (bits (n-sn) Int64.zero) s
                      else sprintf "%s & '%s'" (bits (n-sn) Int64.zero) s;
          end else 
          begin
            if n > 1 then sprintf "\"%s\"" (String.sub s (sn-n) n)
                     else sprintf "'%s'" (String.sub s (sn-n) n)
          end;
            
        | V_bool b -> if b then bits n (Int64.one) else bits n (Int64.zero);
        | V_null -> bits n (Int64.zero);
        | V_z -> zets n;
        | V_string s -> s;
        | _ -> error 857292 "";
        in

    match tp with
    | DT_logic n -> bit_str n v;
    | DT_int n -> 
    begin
        match v with
        | V_int v -> sprintf "%s(%s,%d)" 
                             (vhdl_map "to_signed")
                             (Int64.to_string v) n;
        | V_null -> sprintf "%s(%d,%d)" 
                            (vhdl_map "to_signed")
                            0 n;
        | V_logic s -> bit_str n v;
        | _ -> error 114615 "";
    end;
    | DT_bool -> if v = (V_bool true) then "'1'" else "'0'"; 
    | DT_char -> bit_str 8 v;
    | DT_natural n -> 
    begin
      match v with
      | V_int v -> Int64.to_string v;
      | V_char c -> string_of_int (int_of_char c);
      | V_logic s -> (* convert to integer *)
          string_of_int (int_of_val v)
      | _ -> print_dt tp; error 872522 "" 
    end;
    | DT_object "%V" ->
      let dt = dt_of_val v in
      val_str dt v;
    | _ -> out_ (print_value v); print_dt tp; error 872521 ""

let sel_width sel_size = 
    let w = const_width (V_int (Int64.of_int sel_size)) in
    let s' = 1 lsl (w-1) in
    if s' = sel_size then (w-1) else w 

(*
** OT_struct behind uc_object? For example UC_queue can be!
*)
let is_uo_struct uo =
    let rec iter ufl =
        match ufl with
        | uf :: tl ->
        begin
            match uf with
            | UO_ot (OT_struct _) -> true;
            | _ -> iter tl;
        end;
        | [] -> false in
    iter uo.uo_flags

let get_uo_struct uo =
    let rec iter ufl =
        match ufl with
        | uf :: tl ->
        begin
            match uf with
            | UO_ot (OT_struct st) -> OT_struct st;
            | _ -> iter tl;
        end;
        | [] -> error 119573 "" in
    iter uo.uo_flags
        
