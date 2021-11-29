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
**    $INITIAL:     (C) 2006-2017 BSSLAB
**    $CREATED:     10.10.2007
**    $VERSION:     2.30
**
**    $INFO:
**
** Data management (microcode,vhdl): Transformation and conversion utilities.
**
**
**    $ENDOFINFO
**
*)

open Unix
open Printf

open Cp_syntax
open Cp_types
open Cp_common
open Cp_symbol
open Cp_utils
open Cp_data_core
open Cp_print
open Cp_printtypes
open Cp_vhdl
open Cp_stat


(*
************************************
** OT to UD
************************************
*)


(*
** Generate context specific UD from object OT, range and 
** conversion parameters. The type conversion parameter must already reflect
** the target type of an expression in the case different from object
** type.
*)
let rec ud_of_ot ot opl flags expr_dt =
    let full_array = (is_array ot) && 
                     not (is_sel opl) && 
                     not (is_sel_obj opl) in

    (*
    ** Explcit type conversion, e.g. to_logic ...
    *)
    let conv =
        if is_conv opl then
            Some (obj_conv opl)
        else None in

    debug "ud_of_ot" with (sprintf "ud_of_ot: ot <%s> with opl=[%s]"
                         (name_of_ot ot)
                         (sprint_opl opl));
    let range =
        if is_sub opl && not full_array then
            Some (obj_sub opl)        
        else if is_index opl && not full_array then
            Some (let a = obj_index opl in a,a)   
        else None in

    let index =
        if is_index_obj opl && not full_array then
        begin
          match obj_index_obj opl with
          | PI_obj (_,ot) -> Some (UA_data (ud_of_ot ot [] [] expr_dt));
          | _ -> error 0 "Unexpected expression in dynamic bitselector";
        end
        else None in

    let sub =
        if is_sub opl && (is_array ot) then
            Some (obj_sub opl)        
        else None in

    let conv' = conv in

    let lhs = List.mem UO_lhs flags in
    let nulladdr = (fun _ -> "") in

    (*
    ** Object data type
    *)
    let data_type ot =
      match dt_of_ot ot with
      | Some dt -> dt;
      | None -> error 907084 "" in
    
    (*
    ** Expression type
    *)
    let expr_type ot = expr_dt in
 
    let var_type co =
      let db = 
        match co.co_block with
        | Some db -> db;
        | None -> error 726208 "";
        in
      DT_logic (db.db_width)
      in
      
    (*
    ** Logical data type - expression dependency
    *)
    let log_type ot =
      match co_of_ot ot with
      | Some co ->
      begin
        let dt = co.co_type in
        match range with
        | Some (a,b)  ->
              let n'=b-a+1 in
              Some (dt_of_td (td_of_dt dt) n');
        | None -> 
        begin
          match index with
          | Some _ -> Some (dt_of_td (td_of_dt dt) 1);
          | None -> None;
        end;
      end;
      | None -> None;
      in
    (*
    ** Physical data type - object dependency
    *)
    let rec phy_type ot =
      match ot with
      | OT_var co -> Some (var_type co);
      | OT_reg co
      | OT_signal co ->
      begin
        let dt = co.co_type in
        match dt with
        | DT_char -> Some (DT_logic 8);
        | DT_bool -> Some (DT_logic 1);
        | _ -> None
      end;
      | OT_channel ch ->
      begin
        let dt = ch.ch_obj.co_type in
        match dt with
        | DT_char -> Some (DT_logic 8);
        | DT_bool -> Some (DT_logic 1);
        | _ -> None
      end;
      | OT_queue qu ->
      begin
        let dt = qu.qu_obj.co_type in
        match dt with
        | DT_char -> Some (DT_logic 8);
        | DT_bool -> Some (DT_logic 1);
        | _ -> None
      end;
      | OT_array at ->
      begin
        let is_block = List.mem AT_block at.at_flags in
        let is_dyn = List.mem AT_dyn at.at_flags in
        if full_array || is_block || (is_sel_obj opl) then
            phy_type at.at_objs.(0)
        else if is_sel opl then
        begin
          let i = at_index at (obj_sel opl) in
          phy_type at.at_objs.(i)
        end
        else
          error 499042 ""; 
      end;
      | _ -> None
      in

    (*
    ** Subrange type
    *)
    let sub_type ot =
      match co_of_ot ot with
      | Some co ->
      begin      
        match range with
        | Some range -> Some (range_dt co.co_type range);
        | None ->
        begin
          match index with
          | Some _ -> Some (range_dt co.co_type (0,0));
          | None -> None;
        end;
      end;
      | None -> None in
     
      
    let eval_const v =
        (*
        ** Evaluate some special value conversions here.
        *)
        if List.mem OD_lneg opl then
        begin
            match v with
            | V_int w ->
                V_int (Int64.lognot w);
            | _ -> error 284856 "";
        end
        else if List.mem OD_aneg opl then
        begin
            match v with
            | V_int w ->
                V_int (Int64.neg w);
            | _ -> error 284857 "";
        end
        else if List.mem OD_bneg opl then
        begin
            match v with
            | V_int w ->
                V_int (Int64.lognot w);
            | _ -> error 284857 "";
        end
        else v in
        

    let i2s  = string_of_int in
    let uo_addr_fun dbs ad =
        let db = get_some dbs in
          (fun off ->
              val_str (DT_logic 
                   (const_width (V_int (i64 (max 2 (db.db_size-1))))))
                   (V_int (Int64.add ad (i64 off)))
          );
        in 
        
    let dt = data_type ot in
    let et = expr_type ot in
    let pt = phy_type ot in
    let lt = log_type ot in
    let st = sub_type ot in
    let ct = conv in
    let sg =
      if List.mem OD_lneg opl then Some DT_lneg 
      else if List.mem OD_aneg opl then Some DT_aneg 
      else if List.mem OD_bneg opl then Some DT_bneg else
      None in

    let uo_type = {
      uo_data_type = dt;
      uo_expr_type = et; 
      uo_phys_type = pt;
      uo_sub_type = st;     
      uo_log_type = lt;
      uo_conv = ct;
      uo_sign = sg; } in
    match ot with
    | OT_reg co ->
        UC_reg {
                uo_name = co.co_name;
                uo_obj = Some co;
                uo_type = uo_type;
                uo_range = range;
                uo_index = index;
                uo_addr = nulladdr;
                uo_sel = [];
                uo_block = None;
                uo_flags = flags;
            };
    | OT_var co ->
        UC_var {
                uo_name = co.co_name;
                uo_obj = Some co;
                uo_type = uo_type;
                uo_range = range;
                uo_index = index;
                uo_addr = uo_addr_fun co.co_block (i64 co.co_index);
                uo_sel = [];
                uo_block = co.co_block;
                uo_flags = flags;
            };
    | OT_signal co ->
        UC_sig {
                uo_name = co.co_name;
                uo_obj = Some co;
                uo_type = uo_type;
                uo_range = range;
                uo_index = index;
                uo_addr = nulladdr;
                uo_sel = [];
                uo_block = None;
                uo_flags = flags;
            };
    | OT_channel ch ->
            UC_chan {
                uo_name = ch.ch_obj.co_name;
                uo_obj = Some ch.ch_obj;
                uo_type = uo_type;
                uo_range = range;
                uo_index = index;
                uo_addr = nulladdr;
                uo_sel = [];
                uo_block = None;
                uo_flags = flags @ (
                    match ch.ch_ot with
                    | Some ot -> [UO_ot ot];
                    | None -> []);
            };
    | OT_queue qu ->
            UC_queue {
                uo_name = qu.qu_obj.co_name;
                uo_obj = Some qu.qu_obj;
                uo_type = uo_type;
                uo_range = range;
                uo_index = index;
                uo_addr = nulladdr;
                uo_sel = [];
                uo_block = None;
                uo_flags = flags @ (
                    match qu.qu_ot with
                    | Some ot -> [UO_ot ot];
                    | None -> []);
            };
    | OT_array at ->
    begin

        let sel_size = proda at.at_dim in
        let sel_width = 
                    let w = const_width (V_int (Int64.of_int sel_size)) in
                    let s' = 1 lsl (w-1) in
                    if s' = sel_size then (w-1) else w in

        
        let is_block = List.mem AT_block at.at_flags in
        let is_dyn = List.mem AT_dyn at.at_flags in

        if full_array then
        begin
            UC_array {
                    uat_name  = at.at_name;
                    uat_type = uo_type;
                    uat_range = sub;
                    uat_obj = at;
                    uat_flags = flags;
                };
        end
        else if is_block then
        begin
          let co = 
                match co_of_ot at.at_objs.(0) with
                | Some co -> co;
                | None -> error 528989 "" in
          let ud = UC_var {
                uo_name = co.co_name;
                uo_obj = Some co;
                uo_type = {
                        uo_data_type = dt;
                        uo_expr_type = et; 
                        uo_phys_type = pt;
                        uo_sub_type = st;
                        uo_log_type = lt;
                        uo_conv = ct;
                        uo_sign = sg; };
                uo_range = range;
                uo_index = index;
                uo_addr = nulladdr;
                uo_sel = [];
                uo_block = co.co_block;
                uo_flags = flags;
                } in
          if is_sel opl then
          begin
              let i = at_index at (obj_sel opl) in
              let sel_ot = OT_value (V_int (i64 i)) in
              set_addr_sel at ud [sel_ot];  
          end
          else 
          begin
              let sel_otl = 
                match obj_sel_obj opl with
                | PI_obj (opl',ot) -> [ot];
                | PI_list isl -> List.map (fun sel ->
                    match sel with
                    | PI_obj (opl',ot) -> ot;
                    | _ -> error 857331 "") isl;
                | _ -> error 418987 ""; in
               set_addr_sel at ud sel_otl
          end;
          ud
        end
        else if not is_dyn && (is_sel opl) then
        begin
          let i = at_index at (obj_sel opl) in
          let ot = at.at_objs.(i) in
          ud_of_ot ot opl flags expr_dt; 
        end
        else if is_dyn && (is_sel opl) then
        begin
          let i = at_index at (obj_sel opl) in
          let vi = V_int (i64 i) in
          let sel_ot = OT_value vi in 
          let sel_dt = DT_int (const_width vi) in
          UC_sel {
              us_name = at.at_name;
              us_obj = ud_of_ot at.at_objs.(i) opl flags expr_dt;
              us_array = at;
              us_sel = [ud_of_ot sel_ot [] [UO_rhs] sel_dt];
          };
        end
        else
        begin
          let sel_otl = 
            match obj_sel_obj opl with
            | PI_obj (opl',ot) -> [ot];
            | PI_list isl -> List.map (fun sel ->
                    match sel with
                    | PI_obj (opl',ot) -> ot;
                    | _ -> error 857331 "") isl;
            | _ -> error 418988 ""; in

          let i = ref 0 in
          let sw = ref 0 in
          let sel_params = 
            Array.map (fun d ->
                incr i;
                if !i > 1 then
                begin
                    let sw' = !sw in
                    let w = const_width (V_int (Int64.of_int d)) in
                    let w = w-1 in
                    sw := !sw + w;
                    [OD_conv (DT_logic sw')]
                end
                else
                begin
                    let w = const_width (V_int (Int64.of_int d)) in
                    let w = w-1 in
                    sw := !sw + w;
                    [OD_conv (DT_logic sel_width)];
                end;
             ) at.at_dim in
          let i = ref 0 in
          UC_sel {
              us_name = at.at_name;
              us_obj = ud_of_ot at.at_objs.(0) opl flags expr_dt;
              us_array = at;
              us_sel = List.map (fun sel_ot ->
                    let sel_dt =
                      match dt_of_ot sel_ot with
                      | Some dt -> dt;
                      | None -> error 0 (sprintf "Can't determine data type of array selector.") in
                    let sel_flags =[UO_rhs] @ (if (is_ot_local (pro_of_ot sel_ot) sel_ot) then [UO_loc] else []) in
                    incr i;
                    ud_of_ot sel_ot sel_params.(!i-1) sel_flags sel_dt) sel_otl;
            };
        end;
    end;
    | OT_named_value (_,v)
    | OT_value v -> 
    begin
          UC_val {uv_type={
                        uo_data_type = dt_of_val v;
                        uo_expr_type = et; 
                        uo_phys_type = None;
                        uo_sub_type = None;
                        uo_log_type = None;
                        uo_conv = None;
                        uo_sign = None;};
                  uv_val=eval_const v}
    end;
    | OT_const co ->
    begin
        UC_val {uv_type={
                        uo_data_type = co.co_type;
                        uo_expr_type = et; 
                        uo_phys_type = None;
                        uo_sub_type = None;
                        uo_log_type = None;
                        uo_conv = None;
                        uo_sign = None;};
                uv_val=co.co_init};
    end;
    | OT_struct str ->
    begin
      let ul = {
        ul_type = {
           uo_data_type = dt;
           uo_expr_type = et; 
           uo_phys_type = pt;
           uo_sub_type = st;
           uo_log_type = lt;
           uo_conv = None;
           uo_sign = None; };
        ul_list = List.map (fun ot -> ud_of_ot ot [] flags expr_dt) str.st_objs;
        ul_flags = flags@[UO_ot (OT_struct str)];
        } in
      (*
      ** Fix uo_expr_type, must be min (uo_data_type | uo_log_type | uo_phys_type)
      *)
      ul.ul_list <- List.map (fun ud ->
        match uo_of_ud ud with
        | Some uo ->
            uo.uo_type.uo_expr_type <- (
                if uo.uo_type.uo_log_type <> None then (get_some uo.uo_type.uo_log_type)
                else if uo.uo_type.uo_phys_type <> None then (get_some uo.uo_type.uo_phys_type)
                else uo.uo_type.uo_data_type;  
              );
            ud;
        | None -> ud;
          ) ul.ul_list;
      UC_list ul
    end;
    | _ -> UC_immed 0

let ud_of_ot ot opl flags expr_dt =
  let uc = ud_of_ot ot opl flags expr_dt in
  debug "ud_of_ot" with (ui_sprint_uc uc);
  uc 


(*
** Function arguments
*)
let fun_get_ud pro modname name argli argnum aodt rhs =
    let pro_main pro =
      let len = String.length pro.pro_name in
      len > 4 && (String.sub pro.pro_name 0 4) = "MOD_" in
      
    let rec is_local ot =
      match pro with
      | Some pro when (not (pro_main pro))->
      begin
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
        | OT_array at ->
        begin
          match at.at_objs.(0) with
          | OT_var co -> is_local (OT_var co);
          | _ -> sym_check_sym pro.pro_objs (Sym_obj ot);
        end;
        | _ ->
            sym_check_sym pro.pro_objs (Sym_obj ot);
      end;
      | _ -> false in
    let arg = 
          try
            (List.nth argli (argnum-1))
          with
            | _ -> error 0 (sprintf "%s: too few function arguments in <%s>"
                                    modname name);
            in    
      let rec uo_arg arg = 
          match arg with
          | UC_reg uo
          | UC_var uo
          | UC_sig uo -> 
            arg;
          | UC_sel us -> 
            let at = us.us_array in
            let is_dyn = List.mem AT_dyn at.at_flags in
            if is_dyn then
                error 1668 "";
            uo_arg us.us_obj; 
          | UC_val uv ->
            arg;
          | UC_array at -> arg;
          | _ -> error 0 (sprintf "%s: unexpected function argument in <%s>"
                                  modname name);
          in
      match arg with
      | PI_obj (opl,ot) ->
        let dt = dt_of_ot ot in
        let expr_dt =  match aodt with
                       | Some dt -> dt;
                       | None -> 
                       begin
                        match dt with
                        | Some dt -> dt;
                        | None -> DT_object name;
                       end in
        let flags = 
          (if rhs then [UO_rhs] else [UO_lhs])@
          (if is_local ot then [UO_loc] else []) in
        let opl' = opl in
        uo_arg (ud_of_ot ot opl' flags expr_dt);
      | _ -> error 0 (sprintf "%s: unexpected function argument in <%s>"
                              modname name)



(*
*******************************
** UD to VHDL
*******************************
*)

let vhdl_rhs_rules,
    vhdl_lhs_rules = ref (fun dt pt et st nt ct range index to_ot -> []), 
                     ref (fun dt pt et st nt ct range index to_ot -> [])

let type_name dt =
  match dt with
  | DT_logic n -> sprintf "L%d" n;
  | DT_int n -> sprintf "I%d" n;
  | DT_string n -> sprintf "S%d" n;
  | DT_char -> "C";
  | DT_bool -> "B";
  | DT_object s -> sprintf "%s" s;
  | DT_lneg -> "(-)"
  | DT_aneg -> "(-)"
  | DT_bneg -> "(-)"
  | DT_natural n -> sprintf "NAT%d" n

(*
** Type and size conversion. DT_XX 0 -> only type conversion.
*)
let vhdl_convert src dst =
  match src with
  | DT_logic n ->
  begin
      match dst with
      | DT_logic n' -> 
        let n = if n = 0 then n' else n in
        let n' = if n' = 0 then n else n' in
        if n' > n && n > 1 then "Ln_to_Lm((",sprintf "),%d,%d)" n n'
        else if n' > n then "Ln_1_to_Lm((",sprintf "),%d,%d)" n n'
        else if n' < n && n' > 1 then "Lm_to_Ln((",sprintf "),%d,%d)" n n'
        else if n' < n then "Lm_to_Ln_1((",sprintf "),%d,%d)" n n'
        else "",""
      | DT_int n' ->
        let n = if n = 0 then n' else n in
        let n' = if n' = 0 then n else n' in
        if n' > n then "Ln_to_Im((",sprintf "),%d,%d)" n n'
        else if n' < n then "Lm_to_In((",sprintf "),%d,%d)" n n'
        else "L_to_I((","))"
      | DT_bool ->
        let n = if n = 0 then 1 else n in
        if n = 1 then "",""
        else "Lm_to_Ln_1(",sprintf ",%d,1)" n
      | DT_lneg -> "not(",")";
      | DT_bneg -> "not(",")";
      | DT_aneg -> error 0 "arithemtic negation not allowed with type logic.";
      | DT_char -> 
        let n' = 8 in
        let n = if n = 0 then n' else n in
        if n' > n && n > 1 then "Ln_to_Lm((",sprintf "),%d,%d)" n n' 
        else if n' > n then "Ln_1_to_Lm((",sprintf "),%d,%d)" n n' 
        else if n' < n then "Lm_to_Ln((",sprintf "),%d,%d)" n n' 
        else "",""
      | DT_natural n' ->
        (sprintf "L_%sto_N(" (if n = 1 then "1_" else "")),")"
      | _ -> error 4098 "";
  end;
  | DT_int n ->
  begin
      match dst with
      | DT_logic n' -> 
        let n = if n = 0 then n' else n in
        let n' = if n' = 0 then n else n' in
        if n' > n then "In_to_Lm((",sprintf "),%d,%d)" n n'
        else if n' < n && n' > 1 then "Im_to_Ln((",sprintf "),%d,%d)" n n'
        else if n' < n then "Im_to_Ln_1((",sprintf "),%d,%d)" n n'
        else "I_to_L(",")"
      | DT_int n' ->
        let n = if n = 0 then n' else n in
        let n' = if n' = 0 then n else n' in
        if n' <> 9999 then
        begin
          if n' > n then "In_to_Im((",sprintf "),%d,%d)" n n'
          else if n' < n then "Im_to_In((",sprintf "),%d,%d)" n n'
          else "",""
        end
        else
        begin
          (*
          ** really type integer!!! 
          *)
          if n' <> n then "I_to_N((","))"
          else "",""
        end;          
      | DT_bool -> 
        let n = if n = 0 then 1 else n in
        "In_to_Lm_1(",sprintf",%d,1)" n
      | DT_char -> "",""
      | DT_aneg -> "-(",")";
      | DT_lneg -> error 0 "logical negation not allowed with type integer.";
      | DT_bneg -> error 0 "boolean negation not allowed with type integer.";
      | DT_natural n' ->
        "I_to_N((","))"
      | _ -> error 90907 "";
  end;
  | DT_bool ->
  begin
    match dst with
    | DT_logic 1 -> "","" 
    | _ -> "",""
  end;
  | DT_char ->
  begin
      let n = 8 in
      match dst with
      | DT_logic n' -> 
        let n = if n = 0 then n' else n in
        let n' = if n' = 0 then n else n' in
        if n' > n && n > 1 then "Ln_to_Lm((",sprintf "),%d,%d)" n n'
        else if n' > n then "Ln_1_to_Lm((",sprintf "),%d,%d)" n n'
        else if n' < n && n' > 1 then "Lm_to_Ln((",sprintf "),%d,%d)" n n'
        else if n' < n then "Lm_to_Ln_1((",sprintf "),%d,%d)" n n'
        else "",""
      | DT_int n' ->
        let n = if n = 0 then n' else n in
        let n' = if n' = 0 then n else n' in
        if n' > n then "Ln_to_Im((",sprintf "),%d,%d)" n n'
        else if n' < n then "Lm_to_In((",sprintf "),%d,%d)" n n'
        else "L_to_I(",")"
     | DT_bool ->
        let n = if n = 0 then 1 else n in
        "Ln_to_B((",sprintf "),%d)" n
      | DT_lneg -> "not(",")";
      | DT_bneg -> "not(",")";
      | DT_aneg -> error 0 "arithemtic negation not allowed with type logic.";
      | DT_char -> "",""
      | DT_natural n' ->
        "L_to_N((","))"
      | _ -> error 4098 "";
  end;
  | _ -> "",""

(*
** VHDL type conversion from (uo_type,range,index), both LHS and RHS
*)

let vhdl_conv_neg dt nt name =
  match nt with
  | Some DT_lneg -> 
  begin
    match dt with
    | DT_int n -> sprintf "L_to_I(not(I_to_L(%s)))" name
    | _ -> sprintf "not(%s)" name
  end;
  | Some DT_bneg -> sprintf "not(%s)" name
  | Some DT_aneg -> sprintf "-(%s)" name
  | _ -> name 


let vhdl_conv_range st it s dt = 
    match st with
    | Some (a,b) ->
    begin
        (*
        ** Static bit range
        *)
        match dt with
        | DT_logic n -> 
          sprintf "%s%s" s (if a <> b then 
                              sprintf "(%d downto %d)" b a
                            else
                              sprintf "(%d)" a 
                           ),
          DT_logic (b-a+1)
        | DT_int n -> 
          sprintf "%s%s" s (if a <> b then 
                              sprintf "(%d downto %d)" b a
                            else
                              sprintf "(%d)" a 
                           ),
          DT_int (b-a+1)
        | _ -> error 0 "Bitselection type not supported.";
    end;
    | None -> 
    begin
      (*
      ** Dynamic bit selector
      *)
      match it with
      | Some (UA_data da) ->
      begin
        match dt with
        | DT_logic n -> 
          let sel = 
            match da with
            | UC_reg uo -> uo.uo_name 
            | UC_sig uo -> uo.uo_name 
            | UC_temp ut -> ut.ut_name 
            | _ -> "" 
            in
          sprintf "%s(%s(unsigned(%s)))"
                  s
                  (vhdl_map "to_integer")
                  sel,
          DT_logic 1
        | _ ->  error 0 "Bitselection type not supported.";
      end;
      | _ ->
        s,dt
    end

(*
** Create RHS mask for LHS subrange
** access. Requires LHS_RD object
** on RHS!
*)
let vhdl_conv_lhs_range st it s_wr dt = 
  let s_rd =
    let n = String.length s_wr in
    if n > 3 then
    begin
      let suf = String.sub s_wr (n-3) 3 in
      if suf = "_WR" then
        sprintf "%s_RD" (String.sub s_wr 0 (n-3)) (* global *)
      else
        s_wr  (* local *)
    end
    else s_wr (* local *) 
    in
     
    match st with
    | Some (a,b) ->
    begin
        (*
        ** Static bit range
        *)
      match dt with
      | DT_logic n -> 
        let lsb =
          if a > 1 then sprintf " & %s(%d downto 0)" s_rd (a-1)
          else if a = 1 then sprintf " & %s(0)" s_rd
          else "" in
        let msb =
          if (b+1) < (n-1) then 
                sprintf "%s(%d downto %d) & " s_rd
                        (n-1) (b+1)
          else if (b+1) = (n-1) then
                sprintf "%s(%d) & " s_rd 
                        (b+1)
                   else "" in
        let lp,rp = if lsb <> "" or msb <> "" then "(",")" else "","" in
        sprintf "%s %%s %s" (msb^lp) (rp^lsb), DT_logic (b-a+1)
      | DT_int n -> 
        let lsb =
          if a > 1 then sprintf " & %s(%d downto 0)" s_rd (a-1)
          else if a = 1 then sprintf " & %s(0)" s_rd
          else "" in
        let msb =
          if (b+1) < (n-1) then 
                sprintf "%s(%d downto %d) & " s_rd
                        (n-1) (b+1)
          else if (b+1) = (n-1) then
                sprintf "%s(%d) & " s_rd 
                        (b+1)
                   else "" in
        let lp,rp = if lsb <> "" or msb <> "" then "(",")" else "","" in
        sprintf "%s %%s %s" (msb^lp) (rp^lsb), DT_int (b-a+1)
      | _ -> error 0 "Bitselection type not supported.";
    end;
    | None -> 
    begin
      (*
      ** Dynamic bit selector
      *)
      match it with
      | Some (UA_data da) ->
      begin
        match dt with
        | DT_logic n -> 
          let sel = 
            match da with
            | UC_reg uo -> uo.uo_name 
            | UC_sig uo -> uo.uo_name 
            | UC_temp ut -> ut.ut_name 
            | _ -> "" 
            in
          sprintf "%s <= %s; %s(%s(unsigned(%s))) <= %%s;"
                  s_wr
                  s_rd
                  s_wr
                  (vhdl_map "to_integer")
                  sel,
          DT_logic 1
        | _ ->  error 0 "Bitselection type not supported.";
      end;
      | _ ->
        s_wr,dt
    end


let vhdl_conv_dt s dt dt' =
  if dt <> dt' then
  begin
    let l,r = vhdl_convert dt dt' in 
    (sprintf "%s%s%s" l s r),dt'
  end
  else s,dt 



let vhdl_convert_ud uo_name uo_type uo_flags range index to_ot =
  let data_type = uo_type.uo_data_type in
  let is_rhs = List.mem UO_rhs uo_flags in
  let is_neg = uo_type.uo_sign <> None in

  let dt = uo_type.uo_data_type in
  let et = uo_type.uo_expr_type in
  let pt = uo_type.uo_phys_type in
  let st = uo_type.uo_sub_type in  
  let lt = uo_type.uo_log_type in
  let ct = uo_type.uo_conv in
  let nt = uo_type.uo_sign in

  let std_of_dt sdt =
    match sdt with
    | Some dt -> Some (td_of_dt dt);
    | None -> None in

   
  let conv_find pat rules =
    let dt,spt,et,sst,nt,sct = pat in
    try
       List.find (fun (p,c) -> p = pat) rules 
    with
      | _ -> error 0 (sprintf "\nNo matching type conversion pattern found for <%s>,RHS=%b:\n(DT=%s,PT=%s,ET=%s,ST=%s,NT=%b,TC=%s)" 
                        uo_name
                        is_rhs
                        (sprint_td dt)
                        (if spt <> None then sprint_td (get_some spt) else "*")
                        (sprint_td et)
                        (if sst <> None then sprint_td (get_some sst) else "*")
                        nt
                        (if sct <> None then sprint_td (get_some sct) else "*")) in
      
  let pat = (td_of_dt dt,
             std_of_dt pt,
             td_of_dt et,
             std_of_dt st,
             is_neg,
             std_of_dt ct) in
  if is_rhs then
  begin
    let _,conv = conv_find pat (!vhdl_rhs_rules dt pt et st nt ct range index to_ot) in
    conv uo_name
  end
  else
  begin
    let _,conv = conv_find pat (!vhdl_lhs_rules dt pt et st nt ct range index to_ot) in
    conv uo_name       
  end
  
   
(*
** VHDL vd structure of constant value. Depending on data type with or without
** intermediate constant object (toplevel).
*)
let vhdl_of_val dt v =
  let vd = {
        dp_sig = "";
        dp_def = [];
        dp_sen = [];
        dp_aux = [];
        dp_aux_def = [];
        dp_aux_sen = [];
        cp_sig = "";
        cp_sen = [];
        dp_conv = "%s";
		top_expr = [];
		top_def = [];
        } in
  match dt with
  | DT_int n ->
    let cn = sprintf "CONST_%s_%s" (type_name dt) 
                                   (let s = val_str (DT_natural 0) v in
                                    if s.[0] = '-' then s.[0] <- 'N'; s) in
    vd.top_def <- [
      sprintf "constant %s: signed(%d downto 0) := %s;"
               cn (n-1) (val_str (DT_logic n) v);
      ];
    vd.dp_sig <- cn;
    vd
  | _ -> 
    vd.dp_sig <- val_str dt v;
    vd
  

(*
** Returns formatted VHDL code of UD:
**  vhdl descriptor

*)
let rec vhdl_of_ud proc alu ud lhs_guard =
    debug "vhdl_of_ud" with (Cp_printtypes.ui_sprint_uc ud);
  
    let err str = error 883157 (sprintf "vhdl_of_ud: %s" str) in

    let vd = {
        dp_sig = "";
        dp_def = [];
        dp_sen = [];
        dp_aux = [];
        dp_aux_def = [];
        dp_aux_sen = [];
        cp_sig = "";
        cp_sen = [];
        dp_conv = "%s";
		top_expr = [];
		top_def = [];
        } in

    let obj cos =
        match cos with  
        | Some co -> co;
        | None -> error 408864 "";
        in
    let is_local uo = List.mem UO_loc uo.uo_flags in 
    let pro uo = 
        match uo.uo_obj with
        | Some co -> 
        begin
            match co.co_process with
            | Some pro -> pro.pro_name;
            | None -> "";
        end;
        | None -> err "missing object";
        in
    let block uo = 
        match uo.uo_block with
        | Some db -> db;
        | None -> err "missing DB";
        in
    let lhs uo = List.mem UO_lhs uo.uo_flags in

    let guard_wr ud =
        let gd_rd,gd_wr=ud_guard ud in
        gd_wr
        in
    let guard_rd ud =
        let gd_rd,gd_wr=ud_guard ud in
        gd_rd
        in

    let inout ud = 
      match co_of_ud ud with
      | Some co -> (List.mem Obj_port_in co.co_flags) &&
                   (List.mem Obj_port_out co.co_flags) 
      | None -> false in
    
    let addr ud =
        match ud with
        | UC_var uo -> uo.uo_addr 0;
        | _ -> "" in        

    let addr_def ud = 
        match ud with
        | UC_var uo ->
        begin
            let db = block uo in
            val_str (DT_logic 
                        (const_width (V_int (i64 (max 2 (db.db_size-1))))))
                        (V_int Int64.zero); 
        end;
        | _ -> "" in        

    let real_dt ud =
        (*
        ** Maybe uo_data_type is restricted to subrange, bu we need
        ** original data type of data object
        *)
        match ud with
        | UC_sig uo
        | UC_var uo
        | UC_chan uo 
        | UC_queue uo 
        | UC_reg uo ->
        begin
            match uo.uo_obj with
            | Some co -> co.co_type;
            | None -> error 335446 "";
        end;
        | UC_temp ut ->
        begin
            match ut.ut_obj with
            | Some co -> co.co_type;
            | None -> error 335446 "";
        end;
        | _ -> error 545344 "" in

    let to_ot = to_of_ud ud in

    let conv_rhs signal uot range index = 
      let vdc = vhdl_convert_ud signal uot [UO_rhs] range index to_ot in
      vdc in
    let conv_lhs signal uot range index = 
      let vdc = vhdl_convert_ud signal uot [UO_lhs] range index to_ot in
      vdc in
    let vn dopt =
      match dopt with
      | Some (desc,dt) -> desc;
      | None -> "XXX" in
    let vt dopt =
      match dopt with
      | Some (desc,dt) -> dt;
      | None -> DT_logic 0 in
      
    let eval =
        let is_bus = inout ud in
        match ud with
        | UC_sig uo -> 
            let local = is_local uo in
            let lhs = lhs uo in
            if local then
            begin
                if not lhs then
                begin
                  let vdc = conv_rhs uo.uo_name
                                     uo.uo_type
                                     uo.uo_range
                                     uo.uo_index in
                    vd.dp_sig <- vdc.vdc_sig;
                    vd.dp_sen <- vd.dp_sen @ [uo.uo_name];
                end
                else
                begin
                    let vdc  = conv_lhs uo.uo_name
                                        uo.uo_type
                                        uo.uo_range
                                        uo.uo_index in
                    vd.dp_sig <- vdc.vdc_sig; 
                    vd.dp_conv <- vdc.vdc_mask;
                    vd.dp_def <- vd.dp_def @ 
                                        [uo.uo_name,
                                         val_str (real_dt ud)
                                         (ud_default ud)];
                end;
            end
            else if lhs && not is_bus then
            begin
                let vdc  = conv_lhs (sprintf "%s_WR" uo.uo_name) 
                                    uo.uo_type
                                    uo.uo_range
                                    uo.uo_index in
                                    
                vd.dp_sig <- vdc.vdc_sig; 
                vd.dp_conv <- vdc.vdc_mask;
                vd.dp_def <- vd.dp_def @ [sprintf "%s_WR" uo.uo_name,
                                     val_str (real_dt ud)
                                     (ud_default ud)];
            end
            else if not lhs && not is_bus then
            begin
                let vdc = conv_rhs (sprintf "%s_RD" uo.uo_name) 
                                   uo.uo_type
                                   uo.uo_range
                                   uo.uo_index in
                vd.dp_sig <- vdc.vdc_sig;
                vd.dp_sen <- vd.dp_sen @ [sprintf "%s_RD" uo.uo_name];
            end
            else
            begin
                let vdc = conv_rhs (sprintf "%s" uo.uo_name) 
                                      uo.uo_type
                                      uo.uo_range
                                      uo.uo_index in
                vd.dp_sig <- vdc.vdc_sig;
                vd.dp_sen <- vd.dp_sen @ [sprintf "%s" uo.uo_name];
            end;

        | UC_reg uo -> 
            let pro = pro uo in
            let lhs = lhs uo in
            let local = is_local uo in    
                   
            let name = uo.uo_name in
            
            if lhs && local then
            begin
                let vdc  = conv_lhs (sprintf "%s" name)
                                    uo.uo_type
                                    uo.uo_range
                                    uo.uo_index in
                vd.dp_sig <- vdc.vdc_sig; 
                vd.dp_conv <- vdc.vdc_mask;
                vd.dp_def <- vd.dp_def @ [sprintf "%s" name,
                                     val_str (real_dt ud)
                                     (ud_default ud)];
            end
            else if not lhs && local then
            begin
              let s = name in
              let vdc = conv_rhs s 
                                 uo.uo_type
                                 uo.uo_range 
                                 uo.uo_index in 
              if vdc.vdc_desc <> None then
              begin
                let sc = sprintf "%s_CONV_%s" s (vn vdc.vdc_desc) in
                vd.top_def <- [sprintf "signal %s: %s;" sc (obj_decl_type (vt vdc.vdc_desc))];
                vd.top_expr <- [sprintf "%s <= %s;" sc vdc.vdc_sig];
                vd.dp_sig <- sc;
                vd.dp_sen <- vd.dp_sen @ [sc];
              end
              else
              begin
               vd.dp_sig <- vdc.vdc_sig;
               vd.dp_sen <- vd.dp_sen @ [s]; 
              end;
            end
            else if lhs && not local then
            begin
                let vdc  = conv_lhs (sprintf "REG_%s_WR" name)
                                    uo.uo_type
                                    uo.uo_range
                                    uo.uo_index in
                vd.dp_sig <- vdc.vdc_sig; 
                vd.dp_conv <- vdc.vdc_mask;
                vd.dp_aux <- vd.dp_aux @ [sprintf "REG_%s_WE <= %s;" name
                                    (
                                        match lhs_guard with
                                        | Some (op,gs) -> 
                                            vd.dp_sen <- vd.dp_sen @ [gs];
                                            sprintf "%s %s" op gs;
                                        | None -> "'1'";
                                    )
                                    ];
                vd.dp_def <- vd.dp_def @ [sprintf "REG_%s_WR" name,
                                     val_str (real_dt ud) (ud_default ud);
                                     ];
                vd.dp_aux_def <- vd.dp_def @ [
                                     sprintf "REG_%s_WE" name,
                                     "'0'"];
                if guard_wr ud then
                begin
                    vd.cp_sig <- sprintf "REG_%s_GD = '1'" name;
                    vd.cp_sen <- [sprintf "REG_%s_GD" name];
                end;
            end
            else if not lhs && not local then
            begin
              let s = sprintf "REG_%s_RD" name in
              let vdc = conv_rhs s 
                                 uo.uo_type
                                 uo.uo_range 
                                 uo.uo_index in 
              if vdc.vdc_desc <> None then
              begin
                let sc = sprintf "%s_CONV_%s" s (vn vdc.vdc_desc) in
                vd.top_def <- [sprintf "signal %s: %s;" sc (obj_decl_type (vt vdc.vdc_desc))];
                vd.top_expr <- [sprintf "%s <= %s;" sc vdc.vdc_sig];
                vd.dp_sig <- sc;
                vd.dp_sen <- vd.dp_sen @ [sc];
              end
              else
              begin
               vd.dp_sig <- vdc.vdc_sig;
               vd.dp_sen <- vd.dp_sen @ [s]; 
              end;
            end;

        | UC_chan uo -> 
          let pro = pro uo in
          let lhs = lhs uo in
          let name = uo.uo_name in
          if not (is_uo_struct uo) then
          begin        
            if lhs then
            begin
                let vdc  = conv_lhs (sprintf "CHAN_%s_WR" name)
                                    uo.uo_type
                                    uo.uo_range
                                    uo.uo_index in
                vd.dp_sig <- vdc.vdc_sig; 
                vd.dp_conv <- vdc.vdc_mask;
                vd.dp_aux <- vd.dp_aux @ [sprintf "CHAN_%s_WE <= %s;" name
                                    (
                                        match lhs_guard with
                                        | Some (op,gs) -> 
                                            vd.dp_sen <- vd.dp_sen @ [gs];
                                            sprintf "%s %s" op gs;
                                        | None -> "'1'";
                                    )
                                    ];
                vd.dp_def <- vd.dp_def @ [sprintf "CHAN_%s_WR" name,
                                     val_str (real_dt ud) 
                                             (ud_default ud);
                                     ];
                vd.dp_aux_def <- vd.dp_def @ [
                                     sprintf "CHAN_%s_WE" name,
                                     "'0'"];
                if guard_wr ud then
                begin
                    vd.cp_sig <- sprintf "CHAN_%s_GD = '1'" name;
                    vd.cp_sen <- [sprintf "CHAN_%s_GD" name];
                end;
            end
            else 
            begin
                vd.dp_aux <- vd.dp_aux @ [sprintf "CHAN_%s_RE <= %s;" name
                                    (
                                        match lhs_guard with
                                        | Some (op,gs) -> 
                                            vd.dp_sen <- vd.dp_sen @ [gs];
                                            sprintf "%s %s" op gs;
                                        | None -> "'1'";
                                    )
                                    ];
                let vdc =  conv_rhs (sprintf "CHAN_%s_RD" name) 
                                     uo.uo_type
                                     uo.uo_range
                                     uo.uo_index in
                vd.dp_sig <- vdc.vdc_sig;
                vd.dp_sen <- vd.dp_sen @ [sprintf "CHAN_%s_RD" name];
                vd.dp_aux_def <- vd.dp_def @ [
                                     sprintf "CHAN_%s_RE" name,
                                     "'0'"];
                if guard_rd ud then
                begin
                    vd.cp_sig <- sprintf "CHAN_%s_GD = '1'" name;
                    vd.cp_sen <- [sprintf "CHAN_%s_GD" name];
                end;
            end;
          end
          else
          begin
            let st = 
                match get_uo_struct uo with
                | OT_struct st -> st;
                | _ -> error 638314 "" in
            if lhs then
            begin
                vd.dp_aux <- vd.dp_aux @ [sprintf "CHAN_%s_WE <= %s;" name
                                    (
                                        match lhs_guard with
                                        | Some (op,gs) -> 
                                            vd.dp_sen <- vd.dp_sen @ [gs];
                                            sprintf "%s %s" op gs;
                                        | None -> "'1'";
                                    )
                                    ];
                vd.dp_aux_def <- vd.dp_def @ [
                                     sprintf "CHAN_%s_WE" name,
                                     "'0'"];
                if guard_wr ud then
                begin
                    vd.cp_sig <- sprintf "CHAN_%s_GD = '1'" name;
                    vd.cp_sen <- [sprintf "CHAN_%s_GD" name];
                end;
                List.iter (fun ot ->
                    let name = name_of_ot ot in
                    let expr_dt = get_some (dt_of_ot ot) in
                    let ud = ud_of_ot ot [] [] expr_dt in
                    let vdc = conv_lhs (sprintf "CHAN_%s_WR" name)
                                        uo.uo_type
                                        uo.uo_range
                                        uo.uo_index in
                    vd.dp_sig <- sprintf "%s%s#" vd.dp_sig vdc.vdc_sig; 
                    vd.dp_conv <- sprintf "%s%s#" vd.dp_conv vdc.vdc_mask;
                    vd.dp_def <- vd.dp_def @ [sprintf "CHAN_%s_WR" name,
                                     val_str (real_dt ud) 
                                             (ud_default ud);
                                     ];
                    ) st.st_objs;
            end
            else 
            begin
                vd.dp_aux <- vd.dp_aux @ [sprintf "CHAN_%s_RE <= %s;" name
                                    (
                                        match lhs_guard with
                                        | Some (op,gs) -> 
                                            vd.dp_sen <- vd.dp_sen @ [gs];
                                            sprintf "%s %s" op gs;
                                        | None -> "'1'";
                                    )
                                    ];
                vd.dp_aux_def <- vd.dp_def @ [
                                     sprintf "CHAN_%s_RE" name,
                                     "'0'"];
                if guard_rd ud then
                begin
                    vd.cp_sig <- sprintf "CHAN_%s_GD = '1'" name;
                    vd.cp_sen <- [sprintf "CHAN_%s_GD" name];
                end;
                List.iter (fun ot ->
                    let name = name_of_ot ot in
                    let ud = ud_of_ot ot [] [] in

                    let vdc = conv_rhs (sprintf "CHAN_%s_RD" name) 
                                        uo.uo_type
                                        uo.uo_range
                                        uo.uo_index in
                    vd.dp_sig <- sprintf "%s%s#" vd.dp_sig vdc.vdc_sig;
                    vd.dp_sen <- vd.dp_sen @ [sprintf "CHAN_%s_RD" name];
                    ) st.st_objs;
            end;
          end;

        | UC_queue uo -> 
          let pro = pro uo in
          let lhs = lhs uo in
          let name = uo.uo_name in
          if not (is_uo_struct uo) then
          begin
            if lhs then
            begin
                let vdc  = conv_lhs (sprintf "QUEUE_%s_WR" name)
                                    uo.uo_type
                                    uo.uo_range
                                    uo.uo_index in
                vd.dp_sig <- vdc.vdc_sig; 
                vd.dp_conv <- vdc.vdc_mask;
                vd.dp_aux <- vd.dp_aux @ [sprintf "QUEUE_%s_WE <= %s;" name
                                    (
                                        match lhs_guard with
                                        | Some (op,gs) -> 
                                            vd.dp_sen <- vd.dp_sen @ [gs];
                                            sprintf "%s %s" op gs;
                                        | None -> "'1'";
                                    )
                                    ];
                vd.dp_def <- vd.dp_def @ [sprintf "QUEUE_%s_WR" name,
                                     val_str (real_dt ud) 
                                             (ud_default ud);
                                     ];
                vd.dp_aux_def <- vd.dp_def @ [
                                     sprintf "QUEUE_%s_WE" name,
                                     "'0'"];
                if guard_wr ud then
                begin
                    vd.cp_sig <- sprintf "QUEUE_%s_GD = '1'" name;
                    vd.cp_sen <- [sprintf "QUEUE_%s_GD" name];
                end;
            end
            else 
            begin
                vd.dp_aux <- vd.dp_aux @ [sprintf "QUEUE_%s_RE <= %s;" name
                                    (
                                        match lhs_guard with
                                        | Some (op,gs) -> 
                                            vd.dp_sen <- vd.dp_sen @ [gs];
                                            sprintf "%s %s" op gs;
                                        | None -> "'1'";
                                    )
                                    ];
                let vdc =   conv_rhs (sprintf "QUEUE_%s_RD" name) 
                                      uo.uo_type
                                      uo.uo_range
                                      uo.uo_index in
                vd.dp_sig <- vdc.vdc_sig;
                vd.dp_sen <- vd.dp_sen @ [sprintf "QUEUE_%s_RD" name];
                vd.dp_aux_def <- vd.dp_def @ [
                                     sprintf "QUEUE_%s_RE" name,
                                     "'0'"];
                if guard_wr ud then
                begin
                    vd.cp_sig <- sprintf "QUEUE_%s_GD = '1'" name;
                    vd.cp_sen <- [sprintf "QUEUE_%s_GD" name];
                end;
            end;
          end
          else
          begin
            let st = 
                match get_uo_struct uo with
                | OT_struct st -> st;
                | _ -> error 638313 "" in
            if lhs then
            begin
                vd.dp_aux <- vd.dp_aux @ [sprintf "QUEUE_%s_WE <= %s;" name
                                    (
                                        match lhs_guard with
                                        | Some (op,gs) -> 
                                            vd.dp_sen <- vd.dp_sen @ [gs];
                                            sprintf "%s %s" op gs;
                                        | None -> "'1'";
                                    )
                                    ];
                vd.dp_aux_def <- vd.dp_def @ [
                                     sprintf "QUEUE_%s_WE" name,
                                     "'0'"];
                if guard_wr ud then
                begin
                    vd.cp_sig <- sprintf "QUEUE_%s_GD = '1'" name;
                    vd.cp_sen <- [sprintf "QUEUE_%s_GD" name];
                end;
                
                List.iter (fun ot ->
                    let name = name_of_ot ot in
                    let expr_dt = get_some (dt_of_ot ot) in
                    let ud = ud_of_ot ot [] [] expr_dt in
                    let vdc  = conv_lhs (sprintf "QUEUE_%s_WR" name)
                                        uo.uo_type
                                        uo.uo_range
                                        uo.uo_index in
                    vd.dp_sig <- sprintf "%s%s#" vd.dp_sig vdc.vdc_sig; 
                    vd.dp_conv <- sprintf "%s%s#" vd.dp_conv vdc.vdc_mask;
                    vd.dp_def <- vd.dp_def @ [sprintf "QUEUE_%s_WR" name,
                                     val_str (real_dt ud) 
                                             (ud_default ud);
                                     ];
                    ) st.st_objs;
            end
            else 
            begin
                vd.dp_aux <- vd.dp_aux @ [sprintf "QUEUE_%s_RE <= %s;" name
                                    (
                                        match lhs_guard with
                                        | Some (op,gs) -> 
                                            vd.dp_sen <- vd.dp_sen @ [gs];
                                            sprintf "%s %s" op gs;
                                        | None -> "'1'";
                                    )
                                    ];
                vd.dp_aux_def <- vd.dp_def @ [
                                     sprintf "QUEUE_%s_RE" name,
                                     "'0'"];
                if guard_rd ud then
                begin
                    vd.cp_sig <- sprintf "QUEUE_%s_GD = '1'" name;
                    vd.cp_sen <- [sprintf "QUEUE_%s_GD" name];
                end;
                List.iter (fun ot ->
                    let name = name_of_ot ot in
                    let ud = ud_of_ot ot [] [] in

                    let vdc = conv_rhs (sprintf "QUEUE_%s_RD" name) 
                                        uo.uo_type
                                        uo.uo_range
                                        uo.uo_index in
                    vd.dp_sig <- sprintf "%s%s#" vd.dp_sig vdc.vdc_sig; 
                    vd.dp_sen <- vd.dp_sen @ [sprintf "QUEUE_%s_RD" name];
                    ) st.st_objs;
            end;
          end;

        | UC_temp ut -> 
            let lhs = List.mem UO_lhs ut.ut_flags in
            if lhs then
            begin
                let vdc = conv_lhs (obj ut.ut_obj).co_name
                                    ut.ut_type
                                    ut.ut_range
                                    None in
                vd.dp_sig <- vdc.vdc_sig; 
                vd.dp_conv <- vdc.vdc_mask;
                vd.dp_def <- vd.dp_def @ [(obj ut.ut_obj).co_name,
                                          val_str (real_dt ud)
                                          (ud_default ud)]
            end
            else
            begin
                let vdc =   conv_rhs (obj ut.ut_obj).co_name
                                      ut.ut_type
                                      ut.ut_range
                                      None in
                vd.dp_sig <- vdc.vdc_sig;
                vd.dp_sen <- vd.dp_sen @ [(obj ut.ut_obj).co_name];
            end;

        | UC_var uo -> 
            let db = block uo in
            let pro = pro uo in
            let lhs = lhs uo in
            let local = is_local uo in
            if lhs && local then
            begin
                let vdc = conv_lhs (sprintf "RAM_%s_WR" db.db_name)
                                    uo.uo_type
                                    uo.uo_range
                                    uo.uo_index in
                vd.dp_sig <- vdc.vdc_sig; 
                vd.dp_conv <- vdc.vdc_mask;
                vd.dp_aux <- vd.dp_aux @ 
                                (if guard_wr ud then
                                    [sprintf "RAM_%s_WE <= RAM_%s_GD;" db.db_name db.db_name;]
                                  else
                                    [sprintf "RAM_%s_WE <= '1';"db.db_name;])@ 
                                [sprintf "RAM_%s_ADDR <= %s; -- %s"  
                                         db.db_name
                                         (addr ud)
                                         uo.uo_name];
                vd.dp_aux_def <- vd.dp_aux_def @ [sprintf "RAM_%s_WR" 
                                             db.db_name,   
                                             val_str (DT_logic db.db_width)
                                                 (V_int Int64.zero);
                                          sprintf "RAM_%s_ADDR"  
                                            db.db_name,
                                            (addr_def ud);
                                          sprintf "RAM_%s_WE" db.db_name,
                                            "'0'"];
                if guard_wr ud then
                begin
                    vd.cp_sig <- sprintf "RAM_%s_GD = '1'" db.db_name;
                    vd.cp_sen <- [sprintf "RAM_%s_GD" db.db_name];
                end;
            end
            else if not lhs && local then
            begin
                let vdc =   conv_rhs (sprintf "RAM_%s_RD" 
                                           db.db_name)
                                      uo.uo_type
                                      uo.uo_range
                                      uo.uo_index in
                let s = sprintf "RAM_%s_RD" db.db_name in
                if vdc.vdc_desc <> None then
                begin
                  let sc = sprintf "%s_CONV_%s" s (vn vdc.vdc_desc) in
                  vd.top_def <- [sprintf "signal %s: %s;" sc (obj_decl_type (vt vdc.vdc_desc))];
                  vd.top_expr <- [sprintf "%s <= %s;" sc vdc.vdc_sig];
                  vd.dp_sig <- sc;
                  vd.dp_sen <- vd.dp_sen @ [sc];
                end
                else
                begin
                 vd.dp_sig <- vdc.vdc_sig;
                 vd.dp_sen <- vd.dp_sen @ [s]; 
                end;
                vd.dp_aux <- vd.dp_aux @ 
                                (if guard_rd ud then
                                    [sprintf "RAM_%s_RE <= RAM_%s_GD;" db.db_name db.db_name;]
                                  else
                                    [sprintf "RAM_%s_RE <= '1';"db.db_name;])@ 
                
                                 [sprintf "RAM_%s_ADDR <= %s; -- %s"  
                                             db.db_name
                                             (addr ud)
                                             uo.uo_name];
                vd.dp_aux_def <- vd.dp_aux_def @ 
                                         [sprintf "RAM_%s_ADDR"  
                                            db.db_name,
                                            (addr_def ud);
                                          sprintf "RAM_%s_RE" db.db_name,
                                            "'0'"];
                vd.dp_aux_sen <- vd.dp_aux_sen;
                if guard_rd ud then
                begin
                  vd.cp_sig <- sprintf "RAM_%s_GD = '1'" db.db_name;
                  vd.cp_sen <- [sprintf "RAM_%s_GD" db.db_name];
                end;
            end
            else if lhs && not local then
            begin
                let vdc = conv_lhs (sprintf "RAM_%s_WR" db.db_name)
                                   uo.uo_type
                                   uo.uo_range
                                   uo.uo_index in
                vd.dp_sig <- vdc.vdc_sig; 
                vd.dp_conv <- vdc.vdc_mask;
              
                vd.dp_aux <- vd.dp_aux @ 
                                  (if guard_wr ud then
                                    [sprintf "RAM_%s_WE <= RAM_%s_GD;" db.db_name db.db_name;]
                                   else
                                    [sprintf "RAM_%s_WE <= '1';"db.db_name;])@ 
                                  [sprintf "RAM_%s_ADDR <= %s; -- %s"  
                                                  db.db_name
                                                  (addr ud)
                                                  uo.uo_name];
                vd.dp_def <- vd.dp_def @ 
                                    [sprintf "RAM_%s_WR" 
                                             db.db_name,   
                                     val_str (DT_logic db.db_width)
                                             (V_int Int64.zero);
                                    ];
                vd.dp_aux_def <- vd.dp_aux_def @ 
                                     [sprintf "RAM_%s_WE"    
                                             db.db_name,
                                     "'0'";
                                     sprintf "RAM_%s_ADDR"
                                             db.db_name,
                                     (addr_def ud)];

                if guard_wr ud then
                begin
                  vd.cp_sig <- sprintf "RAM_%s_GD = '1'" db.db_name;
                  vd.cp_sen <- [sprintf "RAM_%s_GD" db.db_name];
                end;
            end
            else if not lhs && not local then
            begin
                let vdc =   conv_rhs (sprintf "RAM_%s_RD" 
                                            db.db_name)
                                      uo.uo_type
                                      uo.uo_range
                                      uo.uo_index in
                let s = sprintf "RAM_%s_RD" db.db_name in
                if vdc.vdc_desc <> None then
                begin
                  let sc = sprintf "%s_CONV_%s" s (vn vdc.vdc_desc) in
                  vd.top_def <- [sprintf "signal %s: %s;" sc (obj_decl_type (vt vdc.vdc_desc))];
                  vd.top_expr <- [sprintf "%s <= %s;" sc vdc.vdc_sig];
                  vd.dp_sig <- sc;
                  vd.dp_sen <- vd.dp_sen @ [sc];
                end
                else
                begin
                 vd.dp_sig <- vdc.vdc_sig;
                 vd.dp_sen <- vd.dp_sen @ [s]; 
                end;
                vd.dp_aux <- vd.dp_aux @ 
                                  (if guard_rd ud then
                                    [sprintf "RAM_%s_RE <= RAM_%s_GD;" db.db_name db.db_name;]
                                   else
                                    [sprintf "RAM_%s_RE <= '1';"db.db_name;])@ 
                                  [sprintf "RAM_%s_ADDR <= %s; -- %s"
                                             db.db_name
                                             (addr ud)
                                             uo.uo_name];
                vd.dp_aux_def <- vd.dp_aux_def @ 
                                    [sprintf "RAM_%s_RE" 
                                             db.db_name,
                                     "'0'";
                                     sprintf "RAM_%s_ADDR"
                                             db.db_name,
                                     (addr_def ud)];
                vd.dp_sen <- vd.dp_sen ;
                if guard_rd ud then
                begin
                  vd.cp_sig <- sprintf "RAM_%s_GD = '1'" db.db_name;
                  vd.cp_sen <- [sprintf "RAM_%s_GD" db.db_name];
                end;
            end;

        | UC_val uv -> 
        begin
          match uv.uv_type.uo_conv with
          | Some ct ->
            (*
            ** Expression will be converted by data_trans_table entry (LHS?)!
            *)
            let vd' = vhdl_of_val uv.uv_type.uo_expr_type uv.uv_val in
            vd.dp_sig <- vd'.dp_sig;
            vd.top_expr <- vd'.top_expr;
            vd.top_def <- vd'.top_def;
          | None ->
            let vd' = vhdl_of_val uv.uv_type.uo_expr_type uv.uv_val in
            vd.dp_sig <- vd'.dp_sig;
            vd.top_expr <- vd'.top_expr;
            vd.top_def <- vd'.top_def;
        end;
        
        | UC_alu ua -> 
            let lhs = List.mem UO_lhs ua.ua_flags in
            if ua.ua_type <> DT_bool then
            begin
              let alu = 
                match alu with
                | Some alu -> alu;
                | None -> find_alu proc ua.ua_type in

              vd.dp_sig <- sprintf "alu_%s_reg" alu.alu_name;
              if not lhs then
                vd.dp_sen <- vd.dp_sen @ [vd.dp_sig]
              else    
                vd.dp_def <- vd.dp_def @ 
                                    [vd.dp_sig, 
                                     val_str alu.alu_type (V_int Int64.zero)];
              (*
              ** simple_assign: RHS is already fixed!
              *)
            end
            else
            begin
                match alu with
                | Some alu -> 
                    vd.dp_sig <- sprintf "alu_%s_bool_reg" alu.alu_name;
                    if not lhs then
                        vd.dp_sen <- vd.dp_sen @ [vd.dp_sig]
                    else
                        vd.dp_def <- vd.dp_def @ [vd.dp_sig,"'0'"];
                | None -> err "ALU[B] but no alu specified";
            end;
        | UC_list ul -> 
          let rec is_st ufl =
            match ufl with
            | UO_ot (OT_struct _) :: _ -> true;
            | _ :: tl -> is_st tl;
            | [] -> false in

          if not (is_st ul.ul_flags) then
          begin
            List.iter (fun ud ->
              let vd' = vhdl_of_ud proc alu ud lhs_guard in
              vd.dp_sig <- vd.dp_sig ^ (if vd.dp_sig <> "" then " & " else " ") ^ 
                           vd'.dp_sig;
              vd.dp_aux <- vd.dp_aux @ vd'.dp_aux;
              vd.dp_def <- vd.dp_def @ vd'.dp_def;
              vd.dp_aux_def <- vd.dp_aux_def @ vd'.dp_aux_def;
              vd.dp_aux_sen <- vd.dp_aux_sen @ vd'.dp_aux_sen;
              vd.dp_sen <- vd.dp_sen @ vd'.dp_sen;
              vd.top_expr <- vd.top_expr @ vd'.top_expr;
              vd.top_def <- vd.top_def @ vd'.top_def;
              (*
              ** No RHS guarded objects allowed here.
              *)
            ) ul.ul_list;
           let vdc =  conv_rhs vd.dp_sig 
                               ul.ul_type 
                               None
                               None in
           vd.dp_sig <- vdc.vdc_sig;
          end
          else
          begin
            List.iter (fun ud ->
              let vd' = vhdl_of_ud proc alu ud lhs_guard in
              vd.dp_sig <- sprintf "%s%s#"
                           vd.dp_sig vd'.dp_sig;
              vd.dp_aux <- vd.dp_aux @ vd'.dp_aux;
              vd.dp_def <- vd.dp_def @ vd'.dp_def;
              vd.dp_aux_def <- vd.dp_aux_def @ vd'.dp_aux_def;
              vd.dp_aux_sen <- vd.dp_aux_sen @ vd'.dp_aux_sen;
              vd.dp_sen <- vd.dp_sen @ vd'.dp_sen;
              vd.top_expr <- vd.top_expr @ vd'.top_expr;
              vd.top_def <- vd.top_def @ vd'.top_def;
              (*
              ** No RHS guarded objects allowed here.
              *)
            ) ul.ul_list;
          end;

        | UC_sel us ->
        begin
            let kind,uo,re =
                match us.us_obj with
                | UC_chan uo -> "CHAN",uo,true;
                | UC_queue uo -> "QUEUE",uo,true;
                | UC_reg uo -> "ARRAY",uo,false;
                | _ -> error 743967 "";
                in
            let local = is_local uo in
            let indval =  (* OPT *)
              match us.us_sel with
              | [UC_val uv] ->
              begin
                match uv.uv_val with
                | V_int w -> Some (Int64.to_int w);
                | _ -> None  
              end
              | _ ->  None in
            (* OPT? local, us.us_sel == [UC_val V_int] => no _SEL, direct sel. expr. *)
            let dt =
                match co_of_ud us.us_obj with
                | Some co -> co.co_type;
                | None -> error 225439 ""; in
            let at = us.us_array in
            let is_dyn = List.mem AT_dyn at.at_flags in

            let sel_size = proda at.at_dim in
            let sel_width = 
                    let w = const_width (V_int (Int64.of_int sel_size)) in
                    let s' = 1 lsl (w-1) in
                    if s' = sel_size then (w-1) else w in

            let flags = uo.uo_flags in
            let lhs = List.mem UO_lhs flags in
            let aux,aux_def,aux_sen =
                let aux = ref (sprintf "%s_%s_SEL <= " kind us.us_name) in
                let first = ref true in
                let def = sprintf "%s_%s_SEL" kind us.us_name,
                                  (zeros sel_width) in
                let sen = ref [] in
                let dim = ref 0 in
                if (indval == None) then
                begin
                  (* OPT *)
                  List.iter (fun sel ->
                    incr dim;
                    match sel with
                    | UC_val uv ->
                      let i =
                          match uv.uv_val with
                          | V_int w -> Int64.to_int w;
                          | _ -> error 505673 "" in
                      if !first then 
                      begin
                          aux := !aux ^ (at_dimi at i !dim);
                          first := false;
                      end
                      else aux := !aux ^ " or " ^ (at_dimi at i !dim);
                    | UC_reg uo ->
                      let vd' = vhdl_of_ud proc alu sel lhs_guard in
                      if !first then 
                      begin
                          aux := !aux ^ (at_dims at vd'.dp_sig !dim);
                          first := false;
                      end
                      else aux := !aux ^ " or " ^ (at_dims at vd'.dp_sig !dim);

                      sen := !sen @ vd'.dp_sen;
                      vd.top_expr <- vd.top_expr @ vd'.top_expr;
                      vd.top_def <- vd.top_def @ vd'.top_def;
                    | _ -> error 380941 "";
                   ) us.us_sel;
                   [!aux^";"],[def], !sen
                  end else [],[],[]
                in
            let name =
                if lhs && not local then sprintf "%s_%s_WR" kind us.us_name
                else if lhs && local && indval = None then (* OPT *) 
                    sprintf "%s_%s(%s(unsigned(%s_%s_SEL)))"
                            kind
                            us.us_name
                            (vhdl_map "to_integer")
                            kind
                            us.us_name
                else if lhs && local && indval <> None then (* OPT *)
                    sprintf "%s_%s(%d)"
                            kind
                            us.us_name
                            (get_some indval)
                else sprintf "%s_%s_RD" kind us.us_name in

            if not lhs then
            begin
                let vdc =  conv_rhs name
                                    uo.uo_type
                                    uo.uo_range
                                    uo.uo_index in
                vd.dp_sig <- vdc.vdc_sig;
                vd.dp_sen <- vd.dp_sen @ [
                            sprintf "%s_%s_RD" kind us.us_name;
                            ];
                if re && not local then
                begin
                    vd.dp_aux <- vd.dp_aux @ [
                        sprintf "%s_%s_RE <= '1';" kind us.us_name;
                        ];
                    vd.dp_aux_def <- vd.dp_aux_def @ [
                        sprintf "%s_%s_RE" kind us.us_name,"'0'";
                    ];
                end;
            end
            else
            begin
                vd.dp_sig <- name;
                if not local then
                begin
                    vd.dp_aux <- vd.dp_aux @ [
                        sprintf "%s_%s_WE <= '1';" kind us.us_name;
                        ];
                    vd.dp_def <- vd.dp_def @ [
                        sprintf "%s_%s_WR" kind us.us_name,
                                val_str dt (V_int Int64.zero);
                        ];
                    vd.dp_aux_def <- vd.dp_aux_def @ [
                        sprintf "%s_%s_WE" kind us.us_name,"'0'";
                    ];
                end;
            end;

            vd.dp_aux <- vd.dp_aux @ aux;
            vd.dp_aux_def <- vd.dp_aux_def @ aux_def;
            vd.dp_aux_sen <- vd.dp_aux_sen @ aux_sen;


            let gd_rd,gd_wr = array_guard us.us_array in
            if gd_rd && not lhs then
            begin
                vd.cp_sig <- sprintf "%s_%s_GD = '1'" kind us.us_name;
                vd.cp_sen <- [sprintf "%s_%s_GD" kind us.us_name];
            end;
            if gd_wr && lhs then
            begin
                vd.cp_sig <- sprintf "%s_%s_GD = '1'" kind us.us_name;
                vd.cp_sen <- [sprintf "%s_%s_GD" kind us.us_name];
            end;

            if local && is_dyn && lhs then
            begin
                (*
                ** Very special case: array cell write access is
                ** inside Data_trans with dynamic selector, an we need
                ** default cell assignments!
                *)
                for i = 0 to sel_size-1
                do
                    vd.dp_def <- vd.dp_def @ [
                        sprintf "%s_%s(%d)" kind us.us_name i,
                        (val_str dt (V_int Int64.zero));
                        ];
                done;
            end;
        end;
        | UC_immed _ -> vd.dp_sig <- "%s";
        | UC_bool _ -> vd.dp_sig <- "%s";
        | _ -> error 0 (sprintf "Unexpected object <%s> found, can't be synthesized to VHDL."
                                (name_of_ud ud)); 
        in
  	debug "vhdl_of_ud" with (sprintf "%s: #top_expr=%d #top_def=%d" vd.dp_sig
	  	  	  	  	  	  	  	  (List.length vd.top_expr) (List.length vd.top_def));
    vd



(*
** Evaluate expression (RHS). No alu may be inferred in expression
** evaluation. Some arithmetic operation require special treatment.
**
** Returns: 
**      vhdl_data (top_expr,top_def)
**
** vhdl_data: VHDL expression (RHS) and auxilliary signals
** top_expr: auxilliary toplevel expressions, if any
** top_def: auxilliary toplevel signal definitions, if any
*)

let vhdl_arithm_aux_n = ref 0 
let vhdl_of_expr pro op1 op2 op =
  let bp = compiler.t_block_constr.bc_params in
  let top_expr' = List.mem (BP_expr EXPR_top) bp in
  
  let top_expr = (get_env_str "expr_type") = "top" in
  
  let vd1 = vhdl_of_ud pro None op1 None in
  let vd2 = vhdl_of_ud pro None op2 None in
  let uot1 = type_of_ud op1 in
  let uot2 = type_of_ud op2 in
  
  let vd = {
      dp_sig = "";
      dp_def = vd1.dp_def @ vd2.dp_def;
      dp_sen = vd1.dp_sen @ vd2.dp_sen;
      dp_aux = vd1.dp_aux @ vd2.dp_aux;
      dp_aux_def = vd1.dp_aux_def @ vd2.dp_aux_def;
      dp_aux_sen = vd1.dp_aux_sen @ vd2.dp_aux_sen;
      cp_sig = "";
      cp_sen = [];
      dp_conv = "%s";
	  top_expr = vd1.top_expr @ vd2.top_expr;
	  top_def = vd1.top_def @ vd2.top_def;
      } in
  match op with
  | OP_lsl
  | OP_lsr
  | OP_asl
  | OP_asr  -> 
  begin
    stat "arithmetic unit" "shifter";
    let op1',expr_dt,data_dt,conv =
      match op1 with
      | UC_temp ut ->
          (UC_temp {ut with ut_type = {ut.ut_type with
                                        uo_expr_type=ut.ut_type.uo_data_type;
                                        uo_conv=None}}),
          ut.ut_type.uo_expr_type,
          ut.ut_type.uo_data_type,
          ut.ut_type.uo_conv;
      | UC_reg uo ->
          (UC_reg {uo with uo_type = {uo.uo_type with
                                          uo_expr_type=uo.uo_type.uo_data_type;
                                          uo_conv=None;}}),
          uo.uo_type.uo_expr_type,
          uo.uo_type.uo_data_type,
          uo.uo_type.uo_conv;
      | UC_val uv -> 
          op1,
          uv.uv_type.uo_expr_type,
          (* FIX map constant values on expression type, not uv.uv_type.uo_data_type,*) 
          uv.uv_type.uo_expr_type,
          None
      | UC_sel us ->
      begin
        match us.us_obj with
        | UC_reg uo ->
          (UC_sel {us with us_obj=UC_reg {uo with uo_type = { uo.uo_type with
                                      uo_expr_type=uo.uo_type.uo_data_type;
                                      uo_conv=None;}}}),
           uo.uo_type.uo_expr_type,
           uo.uo_type.uo_data_type,
           uo.uo_type.uo_conv;
        | _ -> error 0 "Unexpected shift operand selector object type found."
      end;
      | _ -> error 0 "Unexpected shift operand object type found." in

    let tid_e,size_e = id_of_dt expr_dt, size_of_dt expr_dt in
    let tid_d,size_d = id_of_dt data_dt, size_of_dt data_dt in

    let vd1' = vhdl_of_ud pro None op1' None in

    match op2 with    
    | UC_val uv -> 
    begin
      let s,s64 =
        match uv.uv_val with
        | V_int i64 -> Int64.to_int i64,i64;
        | _ -> error 0 "shift parameter of unexpected type" in

      match data_dt with
      | DT_logic n ->
          if op = OP_asl || op = OP_asr then
              error 0 (sprintf "Arithmetic shift operation not supported for type logic!");
          (*
          ** No VHDL operator available!
          *)
          (if op = OP_lsl && s < (max n size_e) then
              vd.dp_sig <- 
              sprintf "%s%s%s(%d downto %d)%s"
                      (if size_e-s > n then 
                          sprintf "%s & " (zeros (size_e-n-s)) else "")
                      ("")
                      vd1'.dp_sig
                      (min (n-1) (size_e-s-1))
                      (0)
                      (sprintf "& %s" (zeros s))
          else if s < n then  (* OP_lsr *)
              vd.dp_sig <- 
              sprintf "%s%s%s(%d downto %d)%s"
                      (if size_e > n then 
                         sprintf "%s & " (zeros (size_e-n)) else "")
                      (sprintf "%s & " (zeros s))
                      vd1'.dp_sig
                      ((min n size_e)-1)
                      (s)
                      ("")
          else error 0 (sprintf "%s: shift range %d larger than object or expression size %d!"
                                (name_of_ud op1) s (max n size_e)));
          vd
      | DT_int n -> 
          (*
          ** VHDL operator available!
          *)
          vd.dp_sig <- (
            let ops = vhdl_map (if (op = OP_lsl || op = OP_asl) then "shift_left" else "shift_right") in
            match ops with
            | "shift_left" | "shift_right" ->
              sprintf "%s(%s,%d)" ops vd1.dp_sig s
            | "shr" | "shl" ->
              sprintf "%s(%s,%s)" ops vd1.dp_sig
                     (val_str (DT_logic (const_width (V_int s64))) (V_int s64))
            | _ -> error 0 (sprintf "Got unexpected arithmetic VHDL operator <%s>." ops);
          );
          vd
      | _ -> error 0 "Unexpected shift operand data type found.";
    end;
    | UC_reg uo ->
    begin
      (*
      ** Dynamic shifting
      *)
      let dt_p = uo.uo_type.uo_data_type in
      let et_p = uo.uo_type.uo_expr_type in
      let width_p = size_of_dt dt_p in
      let n = 1 lsl width_p in
      (*
      ** fix dt_p
      *)    
      let vd2 = vhdl_of_ud pro None 
                  (UC_reg {uo with uo_type = {uo.uo_type with
                                              uo_expr_type=uo.uo_type.uo_data_type;
                                              uo_conv=None;}})
                  None in

      let temp_s1 = sprintf "aux_arithm_shift_OP1_%d"
                           !vhdl_arithm_aux_n in
      let temp_s2 = sprintf "aux_arithm_shift_OP2_%d"
                           !vhdl_arithm_aux_n in
      let temp_s3 = sprintf "aux_arithm_shift_R_%d"
                           !vhdl_arithm_aux_n in

      vd.top_def <- [sprintf "signal %s: %s;"
                              temp_s1 (obj_decl_type data_dt);
                      sprintf "signal %s: %s;"
                              temp_s2 (obj_decl_type dt_p);
                      sprintf "signal %s: %s;"
                              temp_s3 (obj_decl_type data_dt);];
      let aux = [sprintf "%s <= %s;" temp_s1 vd1'.dp_sig; 
                 sprintf "%s <= %s;" temp_s2 vd2.dp_sig] in
      let aux_def = 
            [temp_s1,(val_str data_dt (V_int Int64.zero)); 
             temp_s2,(val_str dt_p (V_int Int64.zero))] in
      vd.top_expr <- vd.top_expr @ [sprintf "%s <=" temp_s3];
      let shn = ref 0 in
      let cw = const_width (V_int (Int64.of_int n)) in
      for i = 1 to n-1
      do
        let s = i in
        let s64 = Int64.of_int i in
        match data_dt with
        | DT_logic n ->
          if op = OP_asl || op = OP_asr then
              error 0 (sprintf "Arithmetic shift operation not supported for type logic!");
         (*
         ** No VHDL operator available!
         *)
         if i < n then
         vd.top_expr <- vd.top_expr @ [(
          incr shn;
          sprintf "  (%s%s(%s)%s) when %s = %s else"
                  (if op = OP_lsr then sprintf "%s & " (zeros s) else "")
                  temp_s1
                  (
                    let a = if op = OP_lsr  then (n-1) else (n-s-1) in
                    let b = if op = OP_lsr  then s else 0 in
                    if a <> b then sprintf "%d downto %d" a b else
                                   sprintf "%d" a
                  )
                  (if op = OP_lsl then sprintf "& %s" (zeros s) else "")
                  temp_s2
                  (val_str dt_p (V_int (Int64.of_int i)))
          )];
        | DT_int n -> 
         (*
         ** VHDL operator available!
         *)
         if i < n then
         vd.top_expr <- vd.top_expr @ [(
          incr shn;
          if (vhdl_map "shift_left") = "shift_left" then
            sprintf "  (%s(%s,%d)) when %s = %s else"
               (if op = OP_lsl then "shift_left" else "shift_right")
               temp_s1
               s
               temp_s2
               (val_str dt_p (V_int (Int64.of_int i)))
          else if (vhdl_map "shift_left") = "shl" then
            sprintf "  (%s(%s,%s)) when %s = %s else"
               (if (op = OP_lsl || op = OP_asl) then "shl" else "shr")
               temp_s1
               (val_str (DT_logic (cw)) (V_int s64))
               temp_s2
               (val_str dt_p (V_int (Int64.of_int i)))
          else
              error 0 (sprintf "No rule to implement signed shift with operator %s..."
                               (vhdl_map "shift_left"));
          )];
        | _ -> error 0 "Unexpected shift operand data type found.";
      done;
      vd.top_expr <- vd.top_expr @ [
        sprintf "  %s; -- aux_arithm_shift %d"
                (* x lsl 0 or x lsr 0 is x! It's not zero, (val_str data_dt (V_int (Int64.zero))) *)
                temp_s1
                !vhdl_arithm_aux_n;
      ];
      vd.dp_sig <- (
          let cl,cr = 
            match conv with
            | Some conv -> vhdl_convert data_dt conv
            | None -> "","" in
          sprintf "%s%s%s" cl temp_s3 cr
          );
      vd.dp_aux <- vd.dp_aux @ aux; 
      vd.dp_aux_def <- vd.dp_aux_def @ aux_def;
      vd.dp_sen <- vd.dp_sen @ [temp_s3];
      info (sprintf "VHDL: aux_arithm_shift_%d: replacing dynamic shifter with %d static shifter(s)."
                    !vhdl_arithm_aux_n !shn); 
      incr vhdl_arithm_aux_n;
      vd
    end;
    | _ -> error 0 "unexpected dynamic shift parameter not supported";
  end;
  | OP_mul when (top_expr = true) ->
  begin
    stat "arithmetic unit" "multiplier";
    let expr_type = uot1.uo_expr_type in
    let temp_s1 = sprintf "aux_arithm_mul_RF_%d" !vhdl_arithm_aux_n in
    let temp_s2 = sprintf "aux_arithm_mul_RS_%d" !vhdl_arithm_aux_n in
    let temp_op1 = sprintf "aux_arithm_mul_OP1_%d" !vhdl_arithm_aux_n in
    let temp_op2 = sprintf "aux_arithm_mul_OP2_%d" !vhdl_arithm_aux_n in
    let data_dt,data_dt',rhs1,rhs2 = 
      match expr_type with
      | DT_logic n -> 
        DT_logic n,DT_logic (2*n),
        sprintf "%s * %s" temp_op1 temp_op2,
        sprintf "%s(%d downto 0)" temp_s1 (n-1)
      | DT_int n -> DT_int n,DT_int (2*n),
        sprintf "%s * %s" temp_op1 temp_op2,
        sprintf "%s(%s,%d)" (vhdl_map "resize") temp_s1 n
      | _ -> error 0 (sprintf "Unexpected data type <%s> in multiplication found."
                              (sprint_dt expr_type)) in
    vd.top_def <- vd.top_def @
        [sprintf "signal %s: %s;" temp_s1 (obj_decl_type data_dt');  
         sprintf "signal %s: %s;" temp_s2 (obj_decl_type data_dt);
         sprintf "signal %s: %s;" temp_op1 (obj_decl_type data_dt);  
         sprintf "signal %s: %s;" temp_op2 (obj_decl_type data_dt);];  
    vd.dp_sig <- temp_s2;  
    vd.top_expr <- vd.top_expr @
        [sprintf "%s <= %s;" temp_s1 rhs1;
         sprintf "%s <= %s;" temp_s2 rhs2];
    vd.dp_aux <- vd.dp_aux @ 
        [sprintf "%s <= %s;" temp_op1 vd1.dp_sig;
         sprintf "%s <= %s;" temp_op2 vd2.dp_sig];
    vd.dp_aux_def <- vd.dp_aux_def @
            [temp_op1,(val_str data_dt (V_int Int64.zero)); 
             temp_op2,(val_str data_dt (V_int Int64.zero))];    
    vd.dp_sen <- vd.dp_sen @ [temp_s2];
    info (sprintf "VHDL: aux_arithm_mul_%d: Moving local multiplication expression to entity top level." !vhdl_arithm_aux_n);                 
    incr vhdl_arithm_aux_n;
    vd
  end;
  | OP_mul when (vhdl_map "*") <> "*" ->
  begin
    stat "arithmetic unit" "multiplier";
    let op' = vhdl_map "*" in
    vd.dp_sig <- (
      sprintf "%s(%s,%s)"
        op'
        vd1.dp_sig
        vd2.dp_sig
    );
    vd
  end;
  | OP_max | OP_min ->
  begin
    stat "arithmetic unit" "muxcomperator";
    let op' = vhdl_map (op_vhdl op) in
    vd.dp_sig <- (
      sprintf "%s(%s,%s)"
        op'
        vd1.dp_sig
        vd2.dp_sig
    );
    vd
  end;
  | _ ->
      stat "arithmetic unit" 
        (match op with
          | OP_add -> "adder";
          | OP_sub -> "subtractor";
          | OP_mul -> "multiplier"
          | OP_eq | OP_neq | OP_lt | OP_gt | OP_le | OP_ge -> "comparator";
          | _ -> "misc.");

      vd.dp_sig <- (
        sprintf "%s %s %s"
          vd1.dp_sig
          (op_vhdl op)
          vd2.dp_sig
      );
	  debug "vhdl_expr" with (sprintf "%s: #top_def=%d #top_expr=%d def=%d def_aux=%d" vd.dp_sig 
		  	  	  	  	  	  	   (List.length vd.top_def) (List.length vd.top_expr)
                                   (List.length vd.dp_def) (List.length vd.dp_aux_def));
      vd

let vhdl_sig_of_ud pro ud =
      let vd = vhdl_of_ud pro None ud None in
      vd.dp_sig

let vhdl_sens_of_ud pro ud =
      let vd = vhdl_of_ud pro None ud None in
      vd.dp_sen

