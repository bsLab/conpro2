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
**      BSSLAB, Dr. Stefan Bosse http://www.bsslab.de
**
**      COPYRIGHT: THIS SOFTWARE, EXECUTABLE AND SOURCE CODE IS OWNED BY BSSLAB.
**                 THIS SOURCE CODE MAY NOT BE COPIED, EXTRACTED,
**                 MODIFIED, OR OTHERWISE USED IN A CONTEXT
**                 OUTSIDE OF THE SOFTWARE SYSTEM.
**
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2006-2010 BSSLAB
**    $CREATED:     7.8.2006
**    $VERSION:     2.03
**
**    $INFO:
**
**  Clock manager module.
**
**    $ENDOFINFO
**
*)

open Cp_common
open Cp_types
open Cp_symbol
open Cp_utils
open Cp_data
open Cp_analysis
open Cp_ucode
open Cp_expr
open Cp_syntax
open Cp_fun
open Printf


let (self:(rl_module option) ref) = ref None

(*
** Return true if object is handled by this rule module.
*)
let rec my sym =
  match sym with
  | Sym_obj obj ->
  begin
    match obj with
    | OT_object ao -> ao.ao_type.ta_name = "clock" 
    | _ -> false;
  end;
  | _ -> false

let get_flags ao =
  let src = ref [] in    
  let dst   = ref [] in
  let div = ref 0 in
  let freq = ref "" in
  let freq' = ref "" in
  let err = ref "" in
  let orig = ref false in
  List.iter (fun (n,v) ->
      match n with
      | "source" -> src := !src @ [v];
      | "clock" -> 
        let div' = int_of_string v in
        if div' = 1 then orig := true;
        div := div';
      | "add" -> 
        dst := !dst @ [v];
      | "freq" -> freq := v;
      | "freq'" -> freq' := v;
      | "err" -> err := v;
      | _ -> ();
     ) ao.ao_flags;
  !src, !dst, !div, !orig, !freq, !freq', !err
  
(*
** Object port declaration in entity header.
*)
let rec obj_port sym modu pro =
  let is_mon,is_debug,sym = get_mon sym in
  match sym with
  | Sym_obj obj ->
  begin
    match obj with
    | OT_object ao ->
    begin
        match pro with
        | Some pro ->
            if is_mon then
                error 0 "No clock monitors allowed."
            else 
                error 0 "No process clock domains allowed."
        | None -> 
            if is_mon then
                error 0 "No clock monitors allowd."
            else if not (List.mem Mod_main modu.mod_flags) then
            begin
                (*
                ** Route temporary signals up.
                *)
                [], []
            end
            else
            begin
                (*
                ** Create original signal. 
                *)
                [],[]
            end;
    end;
    | _ -> 
        error 617763 "";
  end;
  | _ -> error 485543 ""


(*
** Object port mapping.
*)
let rec obj_map sym modu pro =
  let is_mon,is_debug,sym = get_mon sym in
  match sym with
  | Sym_obj obj ->
  begin
    match obj with
    | OT_object ao ->
    begin
        match pro with
        | Some pro ->
            if is_mon then
                error 0 "No clock monitors allowed."
            else
                error 0 "No process clock domains allowed.";
        | None -> 
            if is_mon then
                error 0 "No clock monitors allowed."
            else if not (List.mem Mod_main modu.mod_flags) then
            begin
                (*
                ** Route temporary signals up.
                *)
                []
            end
            else
            begin
                []; (* can't happen *)
            end;
    end;
    | _ -> 
        error 894296 "";
  end;
  | _ -> error 364219 ""


(*
** Object declaration in architecture header.
** pro = None -> mod_objs (used by processes of this module...)
** pro <> None -> pro_objs (process local objects)
*)
let rec obj_decl sym modu pro =
  match sym with
  | Sym_obj obj ->
  begin
    match obj with
    | OT_object ao ->
    begin
        (*
        ** Process port
        *)
        let src,dst,div,orig,freq,freq',err = get_flags ao in
        match pro with
        | Some pro ->
            error 0 "No process clock domains allowed.";
        | None ->
            let modu = ao.ao_module in
            let ob_name = ao.ao_name in
            
            (*
            ** Check object place.
            *)
            if sym_check_obj modu.mod_export ao.ao_name then
            begin
                (*
                ** Object is somewhere externally. All 
                ** signals are routed to the module port.
                *)
                [],[]
            end
            else if src <> [] then
            begin
              (
                if orig then
                [
                    sprintf "signal CLOCK_%s: std_logic;"
                            ao.ao_name;
                ]
                else []
              ) @
              (
                if div > 1 then
                [
                    sprintf "signal CLOCK_%s_div: %s;"
                            ao.ao_name 
                            (obj_decl_type (DT_logic (max 2 (log2 div))));
                ]
                else []
              ),[];
            end
            else [],[];
    end;
    | _ -> 
        error 448652 "";
  end;
  | _ -> error 271840 ""
    

(*
** Object implementation in architecture body.
*)
let rec obj_code sym modu pro =
  let vhdl_ind = ref 0 in
  let strl = ref [] in
  let vhdl str =
    let spaces n = String.make n ' ' in
    strl := !strl @ [(spaces !vhdl_ind)^str] 
    in

  match sym with
  | Sym_obj obj ->
  begin
    if (is_mon sym) then    
        error 0 "No clock monitors allowed."
    else
    match obj with
    | OT_object ao ->
    begin
        let name = ao.ao_name in
        let src,dst,div,orig,freq,freq',err = get_flags ao in
        let src =
          match src with
          | [src] -> src;
          | _ -> error 0 (sprintf "Clock: no source for clock <%s> specified" name); in
          
        if div > 1 then
        begin
          out (sprintf "Creating local clock <%s> [div=%d freq=%s freq'=%s err=%s]..."
                       name
                       div
                       freq
                       freq'
                       err);

          vhdl "";
          vhdl "-- Clock divider";
          vhdl (sprintf 
                  "IMPL_CLOCK_%s_div: process("
                  name);
          vhdl_ind := 6;
          vhdl (sprintf "%s)" src);
          vhdl_ind := 0;
          vhdl "begin";
          vhdl_ind := 2;
          vhdl (sprintf "if %s'event and %s='%d' then"
                        src src
                        (get_env_int "clock_level"));
          vhdl_ind := 4;
          vhdl (sprintf "CLOCK_%s_div <= CLOCK_%s_div + 1;"
                        name name);
          vhdl_ind := 2;
          vhdl "end if;";
          vhdl_ind := 0;
          vhdl (sprintf "end process IMPL_CLOCK_%s_div;"
                        name);
          vhdl "";
        end;        
        !strl
    end;
    | _ ->  []
  end;
  | _ -> error 420016 ""
  
(*
** Synthesize one instruction or instructions from a block list
** and create linear MicroCode list. 
*)

let rec instr_ucode instr id pro =
    []

(*
** Emit monitor object
*)
let emit_mon instr modu pro sym =
    []



(*
** Synthesize a function into state list.
*)
let fun_scode instr label next modu pro =
    []


(*
** Toplevel instructions of module modu or process pro.
*)
let emit_top_code instr modu pro =
    let pro = 
        match pro with
        | Some pro -> pro;
        | None -> error 698601 "" in

    let top_expr = ref [] in
    let top_def = ref [] in
    begin
      match instr with
      | PI_fun (src,(opl,ot),sel,args) ->
      begin
        let ao = 
            match ot with
            | OT_object ao -> ao;
            | _ -> error 157452 ""; in    
        let name = ao.ao_name in

        let src,dst,div,orig,freq,freq',err = get_flags ao in
        let src =
          match src with
          | [src] -> src;
          | _ -> error 0 (sprintf "Clock: no source for clock <%s> specified" name); in

        match sel with
        | "source" ->
          if orig then
            top_expr := !top_expr @ [
                          sprintf "CLOCK_%s <= %s;" 
                                  name src;
                        ];
        | "add" ->
            (*
            ** Args: <clock signal>
            *)
            let lhs = fun_get_ud (Some pro) "Clock" name args 1 None false in
            let vdl = vhdl_of_ud pro None lhs None in
            top_expr := [];
            if div = 1 then
                top_expr := !top_expr @ [
                          sprintf "%s <= CLOCK_%s;" 
                                  vdl.dp_sig 
                                  name;
                          ]
            else
            begin
              let dn = log2 div in
              top_expr := !top_expr @ [
                          sprintf "%s <= CLOCK_%s_div(%d);" 
                                  vdl.dp_sig 
                                  name 
                                  (dn-1);
                          ];
            end;
        | "clock" -> ();
        | "action" -> ();
        | _ -> error 0 (sprintf "Clock: unknown object method <%s.%s>"
                                ao.ao_name sel);
      end;  
      | _ -> ();
    end;
    !top_expr, !top_def

let fun_compile modu pro instr top =
    match instr with
    | PI_fun (src,(opl,ot),sel,args) ->
    begin
      let ao = 
            match ot with
            | OT_object ao -> ao;
            | _ -> error 157453 ""; in    
      let name = ao.ao_name in

      let src,dst,div,orig,freq,freq',err = get_flags ao in

      match sel with
      | "source" -> 
            (*
            ** Add source to ao flags list.
            *)
            if src <> [] then 
                error 0 "Clock: more than one source specified.";
            let src_name = fun_get_str "Clock" name args 1 in
            ao.ao_flags <- ao.ao_flags @ ["source",src_name];
      | "add" -> 
            let new_dst = fun_get_ud pro "Clock" name args 1 None false in
            let dst_name = ud_name new_dst in
            if List.mem dst_name dst then
                error 0 (sprintf "Clock: clock destination <%s> already added." dst_name); 
            ao.ao_flags <- ao.ao_flags @ ["add",dst_name];
      | "action" ->
            let dn = fun_get_int "Clock" name args 1 in
            if dn < 0 or dn > 1 then 
                error 0 (sprintf "Clock: invalid action value [0,1].");
            ao.ao_flags <- ao.ao_flags @ ["action",sprintf "%d" dn];
      | "clock" ->
      begin
        if div > 1 then
          error 0 (sprintf "Clock: clock frequency already specified.");
        let v,u = fun_get_freq "Clock" name args 1 in
        let vs = Int64.to_string v in
        let clk = 
            match u with
            | Ghz -> Int64.mul v (Int64.of_string "1000000000");
            | Mhz -> Int64.mul v (Int64.of_string "1000000");
            | Khz -> Int64.mul v (Int64.of_string "1000");
            | Hz -> v in
            
        let freq = sprintf "%s Hz" (Int64.to_string clk) in
        let div_n = Int64.div (get_env_int64 "clock") clk in
        let div = Int64.to_string div_n in
        let clk' = (Int64.div (get_env_int64 "clock") 
                    (Int64.of_int (1 lsl (log2 (Int64.to_int div_n))))) in
        let freq' = sprintf "%s Hz" (Int64.to_string clk') in
        let err =  
          sprintf "%2.1f %%" (((Int64.to_float clk) -. (Int64.to_float clk')) 
                                  /. (Int64.to_float clk) *. 100.0) in
        out (sprintf "Clock: deriving local clock <%s> (%s %s) with divider %s from system clock."
                     name
                     (Int64.to_string v) (print_frequnit u) 
                     div);
        ao.ao_flags <- ao.ao_flags @ ["clock",div;"freq",freq;"freq'",freq';"err",err];
      end;
      | _ -> error 0 (sprintf "Clock: unknown object method <%s.%s>"
                                ao.ao_name sel);
    end;
    | _ -> error 454190 ""                                                  

let bf_time modu proo f =
    FT_min 1


let rec rules = {
    rl_name = "Clock";
    rl_my = my;
    rl_obj_port = obj_port;
    rl_obj_map = obj_map;
    rl_obj_decl = obj_decl;
    rl_obj_code = obj_code;
    rl_instr_ucode = instr_ucode;
    rl_types = [{
        ta_name = "clock";
        ta_rules = rules; 
        ta_flags = [];
    }];
    rl_methods = [
      "source",[new_arg_desc Arg_rhs];
      "add",[new_arg_desc_dt Arg_lhs (DT_logic 1)];
      "clock",[new_arg_desc_dt Arg_rhs (DT_natural 0)];
      "action",[new_arg_desc_dt Arg_rhs (DT_natural 0)];
    ];
    rl_fun_compile = fun_compile;
    rl_fun_scode = fun_scode;
    rl_top_vcode = emit_top_code;
    rl_time = bf_time;
    rl_new_obj = (fun _ _ _ -> rules);
    rl_interp = (fun _ -> "");
    rl_child=None;
}

let init () =
    out "Init: Clock.";
    self := Some rules;
    sym_add top_syms (Sym_rule rules)
