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
**    $INITIAL:     (C) 2006-2008 BSSLAB
**    $CREATED:     23.10.2007
**    $VERSION:     2.03
**
**    $INFO:
**
**  Function utils
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
open Printf


let fun_get_time modname name il num =
      let arg = 
          try
            (List.nth il (num-1))
          with
            | _ -> error 0 (sprintf "%s: too few function arguments in <%s>"
                                    modname name);
            in    
      match arg with
      | PI_obj (opl,ot) ->
      begin
        match ot with
        | OT_named_value (_,V_time (v,u))
        | OT_value (V_time (v,u)) -> v,u;
        | _ -> error 0 (sprintf "%s: unexpected function argument in <%s>"
                                modname name);

      end;
      | _ -> error 0 (sprintf "%s: unexpected function argument in <%s>"
                              modname name)
                                                                                                                                                                                                                
let fun_get_str modname name il num =
      let arg = 
          try
            (List.nth il (num-1))
          with
            | _ -> error 0 (sprintf "%s: too few function arguments in <%s>"
                                    modname name);
            in    
      match arg with
      | PI_obj (opl,ot) ->
      begin
        match ot with
        | OT_named_value (_,V_string str)
        | OT_value (V_string str) -> str;
        | _ -> error 0 (sprintf "%s: unexpected function argument in <%s>"
                                modname name);

      end;
      | _ -> error 0 (sprintf "%s: unexpected function argument in <%s>"
                              modname name)

(*
** same as above, but convert integer value to string
*)
let fun_get_str2 modname name il num =
      let arg = 
          try
            (List.nth il (num-1))
          with
            | _ -> error 0 (sprintf "%s: too few function arguments in <%s>"
                                    modname name);
            in    
      match arg with
      | PI_obj (opl,ot) ->
      begin
        match ot with
        | OT_named_value (_,V_string str)
        | OT_value (V_string str) -> str;
        | OT_named_value (_,V_int w) 
        | OT_value (V_int w) -> Int64.to_string w;
        | _ -> error 0 (sprintf "%s: unexpected function argument in <%s>"
                                modname name);

      end;
      | _ -> error 0 (sprintf "%s: unexpected function argument in <%s>"
                              modname name)


let fun_get_int modname name il num =
      let arg = 
          try
            (List.nth il (num-1))
          with
            | _ -> error 0 (sprintf "%s: too few function arguments in <%s>"
                                    modname name);
            in    
      match arg with
      | PI_obj (opl,ot) ->
      begin
        match ot with
        | OT_named_value (_,V_int w) 
        | OT_value (V_int w) -> Int64.to_int w;
        | _ -> error 0 (sprintf "%s: unexpected function argument in <%s>"
                                modname name);

      end;
      | _ -> error 0 (sprintf "%s: unexpected function argument in <%s>"
                              modname name)

let fun_get_bool modname name il num =
      let arg = 
          try
            (List.nth il (num-1))
          with
            | _ -> error 0 (sprintf "%s: too few function arguments in <%s>"
                                    modname name);
            in    
      match arg with
      | PI_obj (opl,ot) ->
      begin
        match ot with
        | OT_named_value (_,V_bool s)
        | OT_value (V_bool s) -> s;
        | _ -> error 0 (sprintf "%s: unexpected function argument in <%s>"
                                modname name);

      end;
      | _ -> error 0 (sprintf "T%s: unexpected function argument in <%s>"
                              modname name)

let fun_get_freq modname name il num =
      let arg = 
          try
            (List.nth il (num-1))
          with
            | _ -> error 0 (sprintf "%s: too few function arguments in <%s>"
                                    modname name);
            in    
      match arg with
      | PI_obj (opl,ot) ->
      begin
        match ot with
        | OT_named_value (_,V_freq (v,u))
        | OT_value (V_freq (v,u)) -> v,u;
        | _ -> error 0 (sprintf "%s: unexpected function argument in <%s>"
                                modname name);

      end;
      | _ -> error 0 (sprintf "%s: unexpected function argument in <%s>"
                              modname name)


(*
** Return UD of instr argument
*)
let rec fun_get_arg pro modname argname il flags num expr_dt =
  let pro_modu pro =
      let len = String.length pro.pro_name in
      len > 4 && (String.sub pro.pro_name 0 4) = "MOD_" in
  let is_local ot =
      match ot with
      | OT_var co ->
          (*
          ** Place of data block decides!
          *)
          let db = 
              match co.co_block with
              | Some db -> db;
              | None -> error 374846 ""; in
          sym_check_sym pro.pro_objs (Sym_block db)
      | _ ->
          not (pro_modu pro) && (sym_check_sym pro.pro_objs (Sym_obj ot)) in

  match il with
  | arg::tl ->
  begin
      if num = 1 then
      begin
          let arg' = expr_fold arg in
          match arg' with
          | PI_obj (opl,ot) ->
          begin
              let flags = if is_local ot then flags@[UO_loc]
                          else flags in
              let arg_dt = dt_of_ot ot in
              let is_val = is_value ot in
              let rec conv_dt opl = 
                match opl with
                | (OD_conv dt) :: _ -> Some dt;
                | _ :: tl -> conv_dt tl;
                | [] -> None in
                
              if arg_dt = None then error 0 (sprintf "Strange object in function method call <%s.%s>, argument #%d, found." 
                                                     modname argname num );
              
              if expr_dt <> None then
              begin
                let expr_dt = get_some expr_dt in
                
                let arg_dt = 
                  let dt' = get_some arg_dt in
                  match conv_dt opl with
                  | Some dt -> 
                  begin
                    match dt with
                      (*
                      ** Match data width, too
                      *)
                    | DT_logic _ | DT_int _ -> dt_of_td (td_of_dt dt) (obj_size expr_dt)  
                    | _ -> dt 
                  end
                  | None -> dt' in
                
                if (td_of_dt expr_dt) <> (td_of_dt arg_dt) && not is_val then
                  error 0 (sprintf "Incompatible data types in function method call <%s.%s> argument #%d, found." 
                                   modname argname num);
              end;
              let expr_dt = if expr_dt <> None then get_some expr_dt
                            else get_some arg_dt in
              let ud = ud_of_ot ot opl flags expr_dt in
              UA_data ud
          end;
          | PI_arithm (op,op1,op2) ->
          begin
              UA_expr (
                  let ud,uil = expr_synth pro arg' (sprintf "%s.%s"  modname argname) expr_dt in
                  uil
              );
           end;
          | _ -> error 0 
                (sprintf "Unknown function method call argument [%d] in <%s.%s>"
                          num modname argname);
      end
      else 
          fun_get_arg pro modname argname tl flags (num-1) expr_dt;
  end;
  | [] -> error 0 (sprintf "fun_get_arg: too few arguments in <%s.%s>" modname argname)

(*
** Return OT of instr argument 
*)
let rec fun_get_arg_ot modname argname il num  =
  match il with
  | arg::tl ->
  begin
      if num = 1 then
      begin
          match arg with
          | PI_obj (opl,ot) -> ot;
          | _ -> error 0 
                (sprintf "fun_get_arg_ot: unknown function argument [%d] in <%s.%s>"
                          num modname argname);
      end
      else 
          fun_get_arg_ot modname argname tl (num-1);
  end;
  | [] -> error 0 (sprintf "fun_get_arg_ot: too few arguments in <%s.%s>" modname argname)
