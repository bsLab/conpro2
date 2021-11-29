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
**    $INITIAL:     (C) 2006-2010 BSSLAB
**    $CREATED:     30.10.2006
**    $VERSION:     2.03
**
**    $INFO:
**
**  Reset manager module.
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
    | OT_object ao -> ao.ao_type.ta_name = "reset" 
    | _ -> false;
  end;
  | _ -> false

    
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
                error 0 "No reset monitors allowed."
            else 
                error 0 "No process reset domains allowed."
        | None -> 
            if is_mon then
                error 0 "No reset monitors allowd."
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
        error 617769 "";
  end;
  | _ -> error 485549 ""


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
                error 0 "No reset monitors allowed."
            else
                error 0 "No process reset domains allowed.";
        | None -> 
            if is_mon then
                error 0 "No reset monitors allowed."
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
        error 894299 "";
  end;
  | _ -> error 364216 ""


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
        let src = ref [] in    
        let dst   = ref [] in
        List.iter (fun (n,v) ->
            match n with
            | "source" -> src := !src @ [v];
            | "add" -> 
                dst := !dst @ [v];
            | _ -> ();
            ) ao.ao_flags;
        match pro with
        | Some pro ->
            error 0 "No process reset domains allowed.";
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
            else
                [
                    sprintf "signal RESET_%s: %s;"
                            ao.ao_name 
                            (obj_decl_type (DT_logic 1));
                ],[];

    end;
    | _ -> 
        error 448657 "";
  end;
  | _ -> error 271841 ""
    

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
        error 0 "No reset monitors allowed."
    else
    match obj with
    | OT_object ao ->
    begin
        let name = ao.ao_name in
        let src = ref "" in    
        let dst   = ref [] in
        List.iter (fun (n,v) ->
            match n with
            | "source" -> src := v;
            | "add" -> 
                dst := !dst @ [v];
            | _ -> ();
            ) ao.ao_flags;
        !strl
    end;
    | _ ->  []
  end;
  | _ -> error 420017 ""
  
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
        | None -> error 98601 "" in

    let top_expr = ref [] in
    let top_def = ref [] in
    
    begin
      match instr with
      | PI_fun (src,(opl,ot),sel,args) ->
      begin
        let ao = 
            match ot with
            | OT_object ao -> ao;
            | _ -> error 127452 ""; in    
        let name = ao.ao_name in
        let source = ref "" in
        let action = ref 1 in
        let addl = ref [] in
        List.iter (fun (n,v) -> 
                    match n with
                    | "source" -> source := v;
                    | "add" -> addl := !addl @ [v];
                    | "action" -> action := int_of_string v;
                    | _ -> ()) ao.ao_flags;

        if !source = "" then
            error 0 (sprintf "Reset: no source signal specified for <%s>!"
                             ao.ao_name);
        match sel with
        | "source" ->
            let top_reset = !source = "RESET" in
            let top_reset_action = get_env_int "reset_level" in
            
            if top_reset && top_reset_action <> !action then
            begin
              top_expr := !top_expr @ [
                            sprintf "RESET_%s <= '%d' when %s = '%d' else '%d';" 
                                  name !action !source top_reset_action 
                                  (if !action = 1 then 0 else 1)];                          
            end
            else
              top_expr := !top_expr @ [
                            sprintf "RESET_%s <= %s;" 
                                  name !source;            
            ];
        | "add" ->
            (*
            ** Args: <reset signal>
            *)
            let lhs = fun_get_ud (Some pro) "Reset" name args 1 None false in
            let vdl = vhdl_of_ud pro None lhs None in
            top_expr := !top_expr @ [
                          sprintf "%s <= RESET_%s;" 
                                  vdl.dp_sig name;
                          ]
        | "action" -> ();
        | _ -> error 0 (sprintf "Reset: unknown object method <%s.%s>"
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
            | _ -> error 157452 ""; in    
      let name = ao.ao_name in
      let source = ref "" in
      let addl = ref [] in
      List.iter (fun (n,v) -> 
                    match n with
                    | "source" -> source := v;
                    | "add" -> addl := !addl @ [v];
                    | _ -> ()) ao.ao_flags;


      match sel with
      | "source" -> 
            (*
            ** Add source to ao flags list.
            *)
            if !source <> "" then 
                error 0 "Reset: more than one source specified.";
            let src_name = fun_get_str "Reset" name args 1 in
            ao.ao_flags <- ao.ao_flags @ ["source",src_name];
      | "add" -> 
            let dst = fun_get_ud pro "Reset" name args 1 None false in
            let dst_name = ud_name dst in
            if List.mem dst_name !addl then
                error 0 (sprintf "Reset: duplicated reset destination <%s>"
                                 dst_name); 
            ao.ao_flags <- ao.ao_flags @ ["add",dst_name];
      | "action" ->
            let dn = fun_get_int "Reset" name args 1 in
            if dn < 0 or dn > 1 then 
                error 0 (sprintf "Reset: invalid action value [0,1].");
            ao.ao_flags <- ao.ao_flags @ ["action",sprintf "%d" dn];
      | _ -> error 0 (sprintf "Reset: unknown object method <%s.%s>"
                                ao.ao_name sel);
    end;
    | _ -> error 454191 ""                                                  

let bf_time modu proo f =
    FT_min 1


let rec rules = {
    rl_name = "Reset";
    rl_my = my;
    rl_obj_port = obj_port;
    rl_obj_map = obj_map;
    rl_obj_decl = obj_decl;
    rl_obj_code = obj_code;
    rl_instr_ucode = instr_ucode;
    rl_types = [{
        ta_name = "reset";
        ta_rules = rules; 
        ta_flags = [];
    }];
    rl_methods = [
      "source",[new_arg_desc Arg_rhs];
      "add",[new_arg_desc_dt Arg_lhs (DT_logic 1)];
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
    out "Init: Reset.";
    self := Some rules;
    sym_add top_syms (Sym_rule rules)
