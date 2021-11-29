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
**    $CREATED:     16.8.2007
**    $VERSION:     2.02
**
**    $INFO:
**
**      1. Resource calculations
**        Timing informations
**
**      2. Device support  
**
**      Format: xc2s100-tq144-6
**              a3p250-fg144-4
**              <Vendor><Familiy><Part>-<Pack>-<Speed>
**
**    $ENDOFINFO
**
*)

open Cp_types
open Cp_data
open Cp_common
open Cp_utils
open Cp_syntax
open Printf


(*
** Default resource time table. The rt_fun function returns a signal processing time 
** weight factor (in clock_cycle units) between 0.0 and 1.0! A value greater 1.0 indicates a
** signal processing time greater than the system clock cycle time.
*)
let op_time_table = 
  ref [
    {
      rt_ops=[OP_add;OP_sub];
      rt_td=T_logic;
      rt_fun=(fun n -> 
            let sysclk = Int64.to_float (get_env_int64 "clock") in
            let syscyc = 1.0 /. sysclk in
            let one = 2E-9 in
            ((float_of_int n) *. one) /. syscyc);
    };
    {
      rt_ops=[OP_land;OP_lor;OP_lxor;OP_lnot];
      rt_td=T_logic;
      rt_fun=(fun n -> 
            let sysclk = Int64.to_float (get_env_int64 "clock") in
            let syscyc = 1.0 /. sysclk in
            let one = 1E-9 in
            (one /. syscyc));
    };
  ]

(*
** Add new op_time calculation function for given data type and
** operator list. The function is supplied in string format with
** two function parameters: DW (data_width), CT (cycle_time)
*) 
let op_add_time_table opl td fun_str =
  let rec compile te dw ct =
    match te with
    | TE_ident (fpos,str) ->
    begin
      try 
        float_of_string str
      with
        _ -> error 0 (sprintf "op_time: invalid argument <%s> found." str);  
    end;
    | TE_unit (tev,unit) ->
    begin
      (compile tev dw ct) *. (float_of_string unit);
    end;
    | TE_param (fpos,p) ->
    begin
      match p with
      | "DW" -> dw;
      | "CT" -> ct;
      | _ -> error 736596 "";
    end;
    | TE_expr (op,op1,op2) ->
    begin
      let op1' = compile op1 dw ct in
      let op2' = compile op2 dw ct in
      match op with
      | "+" -> op1' +. op2';
      | "-" -> op1' -. op2';
      | "*" -> op1' *. op2';
      | "/" -> op1' /. op2';
      | _ -> error 0 (sprintf "op_time: invalid expression operation <%s> found." op);
    end;
    | _ -> error 0 "op_time: unexpected expression found." in 

  let lb = Lexing.from_string fun_str in
  let fun_syn =
    try
      Cp_parser_expr.main Cp_lexer_expr.token lb
    with
    | _ ->
        let pos = lb.Lexing.lex_curr_pos in
        error 0 (sprintf "op_add_time_table: Syntax error in function source, position %d:\n %s."
                          pos fun_str) in
  op_time_table := {
      rt_ops=opl;
      rt_td=td;
      rt_fun=(fun n -> 
            let fn = float_of_int n in
            let sysclk = Int64.to_float (get_env_int64 "clock") in
            let syscyc = 1.0 /. sysclk in
            compile fun_syn fn syscyc);
    } :: !op_time_table
  
(*
** Returns scaled resource signal processing time value (weight).
** Range:  t=0.0 .. 1.0
** Timing mismatch: t > 1.0! (processing time larger than 
**                            system clock cycle time) 
*)

let op_time op dt =
  let td = td_of_dt dt in
  let n = size_of_dt dt in
  let rec search ttl =
    match ttl with
    | tt :: tl -> if List.mem op tt.rt_ops && tt.rt_td=td then tt 
                  else search tl;
    | [] -> 
      (*
      ** Default average record - only one expression within
      ** one time unit.
      *)
      {rt_ops=[];rt_td=td;rt_fun=(fun n -> 1.0)};
    in 
  let tt = search !op_time_table in
  tt.rt_fun n 
  

(*
** Device table
*)
let defaults dev =
  dev.dev_speed <- (
  match dev.dev_speed with
  | "" ->
  begin
    match dev.dev_class with
    | "xc2s" ->
    begin
      match dev.dev_package with
      | "tq144" -> "6";
      | _ -> "6";
    end; 
    | "xc3s" ->
    begin
      match dev.dev_package with
      | "ft256" -> "5";
      | _ -> "5";
    end; 
    | "a3p" | "a3pe" ->
    begin
      match dev.dev_package with
      | "fbga144" -> "2";
      | "fbga256" -> "2";
      | "fbga324" -> "2";
      | _ -> "2";
    end;
    | _ -> "6";
  end;
  | _ -> dev.dev_speed)

let get_dev_info name =
  let last str =
        let n = String.length str in
        if n > 0 then (String.sub str (n-1) 1).[0] else '*' in
  let without_last str =
        let n = String.length str in
        if n > 0 then (String.sub str 0 (n-1)) else "*" in

  let vendor,fam,part,pack,speed =
     let vend,fam,part,pack,speed = ref "", ref "", ref "", ref "", ref "" in
     let in_vend, in_fam,in_part,in_pack,in_speed= ref true, ref false,
                                          ref false,
                                          ref false,
                                          ref false in

     String.iter (fun c ->
       if !in_vend then
       begin
        match c with
        | 'x' -> vend := "xilinx"; fam := sprintf "%s%c" !fam c;
        | 'a' -> vend := "actel"; fam := sprintf "%s%c" !fam c;
        | '0' .. '9' -> in_vend := false;
                        in_fam := true;
                        fam := sprintf "%s%c" !fam c;
        | _ -> ();
       end 
       else if !in_fam then  
       begin
        match c with
        | '0' .. '9' -> 
        begin
          match last !fam with
          | '0' .. '9' ->
            fam := sprintf "%s%c" !fam c;
          | 'a' .. 'z' -> 
            in_fam := false;
            in_part := true;
            part := sprintf "%c" c;
          | _ -> ();
        end;
        | 'a' .. 'z' ->
          fam := sprintf "%s%c" !fam  c;
        | _ -> ();
       end 
       else if !in_part then  
       begin
        match c with
        | 'a' .. 'z' 
        | '0' .. '9' -> 
            part := sprintf "%s%c" !part c;
        | '-' ->
          in_part := false;
          in_pack := true;
        | _ -> ();
       end 
       else if !in_pack then  
       begin
        match c with
        | '0' .. '9' 
        | 'a' .. 'z' -> 
          pack := sprintf "%s%c" !pack  c;
        | '-' ->
          in_pack := false;
          in_speed := true;
        | _ -> ();
       end 
       else if !in_speed then  
       begin
        match c with
        | '0' .. '9' ->
          speed := sprintf "%s%c" !speed  c;
        | _ ->
          in_speed := false;
       end;
       ) name;
     !vend, !fam, !part, !pack, !speed in
                                          

  let dev = {
        dev_name = name;
        dev_vendor = "";
        dev_family = "";
        dev_class = "";
        dev_subclass = "";
        dev_ext = "";
        dev_package = "";
        dev_speed = "";
        dev_library = "";
    } in
  dev.dev_vendor <- vendor;
  dev.dev_family <- (
     match fam,(last part) with
     | "x2s",('0'..'9') | "xc2s",('0' .. '9') -> "spartan2";
     | "x3s",('0'..'9') | "xc3s",('0' .. '9') -> "spartan3";
     | "x2s",'e' | "xc2s",'e' -> "spartan2e";
     | "a3pe",_ -> "proasic3e";
     | "a3p",_ -> "proasic3";
     | _ -> "unknown";
    );
  dev.dev_ext <- (
     match (last part) with
     | '0'..'9' -> "";
     | 'e' -> "e";
     | _ -> "unknown";
    );
  dev.dev_class <- (
     match fam with
     | "x2s" | "xc2s" -> "xc2s";
     | "x3s" | "xc3s" -> "xc3s";
     | "a3pe" -> "a3pe";
     | "a3p" -> "a3p";
     | _ -> "unknown";
    );
  dev.dev_library <- (
     match dev.dev_family,dev.dev_ext with
     | "spartan2","" -> "xis2";
     | "spartan3","" -> "xis3";
     | "proasic3","" -> "proasic3";
     | "proasic3e","" -> "proasic3e";
     | _ -> "unknown";
    );
  dev.dev_subclass <- (
     match (last part) with
     | '0'..'9' -> part;
     | 'e' -> without_last part;
     | _ -> "unknown";
    );
  dev.dev_package <- (
    match dev.dev_class with
    | "xc2s" -> pack;
    | "xc3s" -> pack;
    | "a3pe" -> 
    begin
      match pack with
      | "fg144" -> "fbga144";
      | "fg256" -> "fbga256";
      | "fg324" -> "fbga324";
      | _ -> pack;
    end;
    | "a3p" -> 
    begin
      match pack with
      | "fg144" -> "fbga144";
      | "fg256" -> "fbga256";
      | "fg324" -> "fbga324";
      | _ -> pack;
    end;
    | _ -> pack;
    );
  dev.dev_speed <- speed;
  defaults dev;
  dev
