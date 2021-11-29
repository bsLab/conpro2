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
**    $CREATED:     19.3.2006
**    $VERSION:     2.11
**
**    $INFO:
**
**  Core module.
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

include Cp_Core_head
include Cp_Core_obj_port
include Cp_Core_obj_map
include Cp_Core_obj_decl
include Cp_Core_obj_code
include Cp_Core_instr_ucode
include Cp_Core_top           

                    
                                                                                                                                                                                                                                                     
                                                                                                                                                                                                                                                                                
(*
** Not bounded MOVE(dst,src)
*)
let simple_assign pro lhs rhs =
  let is_terminated str =
    (*
    ** dp_conv: either expression or terminated statement;
    *)
    str.[(String.length str)-1] = ';' in
  fix_val_type lhs rhs;
  let rhs_guard =
    match rhs with
    | UC_chan uo ->
      Some ("", sprintf "CHAN_%s_GD" uo.uo_name);
    | UC_queue uo ->
      Some ("",sprintf "QUEUE_%s_GD" uo.uo_name);              
    | _ -> None in

  let vdr = vhdl_of_ud pro None rhs rhs_guard in
  let lhs_guard =
      let con () = 
        (* r_con: SIG = expr *)
        let n = String.index vdr.cp_sig ' ' in
        String.sub vdr.cp_sig 0 n in
      let gd_rd,gd_wr = ud_guard rhs in
      if (gd_rd or gd_wr) && vdr.cp_sig <> "" then
        Some ("not",con ())
      else
      begin
        match lhs with
        | UC_chan uo ->
          Some ("", sprintf "CHAN_%s_GD" uo.uo_name);
        | UC_queue uo ->
          Some ("",sprintf "QUEUE_%s_GD" uo.uo_name);              
        | _ -> None;
      end in
  let vdl = vhdl_of_ud pro None lhs lhs_guard in
  (*
  ** Data Path
  *)

  let is_list ud =
    (*
    ** List of structure elements? Happens for example in UC_queue!
    *)
    match ud with
    | UC_chan uo
    | UC_queue uo -> is_uo_struct uo;
    | UC_list ul -> 
      let rec is_st ufl =
        match ufl with
        | UO_ot (OT_struct _) :: _ -> true;
        | _ :: tl -> is_st tl;
        | [] -> false in
      is_st ul.ul_flags 
    | _ -> false in

  let lhs_list = is_list lhs in
  let rhs_list = is_list rhs in
  if (lhs_list && not rhs_list) ||
     (rhs_list && not lhs_list) then error 494171 "";
  let is_list = lhs_list && rhs_list in

  let is_local ud =
    match ud with
    | UC_var _ -> false;
    | _ -> is_local ud in
    
  let dp =
    if not (is_local lhs) then
    begin
        (if not is_list then
         [
          if not (is_terminated vdl.dp_conv) then
            Data_out (sprintf "%s <= %s;" 
                              vdl.dp_sig
                              (sprintf (Obj.magic vdl.dp_conv) 
                                      vdr.dp_sig))
          else
            Data_out (sprintf (Obj.magic vdl.dp_conv) 
                              vdr.dp_sig);
         ]
         else
         begin
            let lhs_sl = Str.split (Str.regexp "#") vdl.dp_sig in
            let rhs_sl = Str.split (Str.regexp "#") vdr.dp_sig in
            List.map2 (fun l r -> 
                Data_out (sprintf "%s <= %s;" l r);
              ) lhs_sl rhs_sl;
         end ) @
        (List.map (fun v -> Data_out v) vdl.dp_aux) @
        (List.map (fun v -> Data_def v) vdl.dp_aux_def) @
        (List.map (fun v -> Data_sens v) vdl.dp_aux_sen) @
        (List.map (fun v -> Data_sens v) vdl.dp_sen) @
        (List.map (fun sv -> Data_def sv) vdl.dp_def) @
        (List.map (fun v -> Data_top v) vdl.top_expr) @
        (List.map (fun v -> Data_top_def v) vdl.top_def) @

        (List.map (fun v -> Data_out v) vdr.dp_aux) @
        (List.map (fun v -> Data_def v) vdr.dp_aux_def) @
        (List.map (fun v -> Data_sens v) vdr.dp_aux_sen) @
        (List.map (fun sv -> Data_def sv) vdr.dp_def) @
        (List.map (fun v -> Data_sens v) vdr.dp_sen) @
        (List.map (fun v -> Data_top v) vdr.top_expr) @
        (List.map (fun v -> Data_top_def v) vdr.top_def) 

    end
    else
    begin
        (if not is_list then
         [
            if not (is_terminated vdl.dp_conv) then
                Data_trans (sprintf "%s <= %s;" 
                                vdl.dp_sig
                                (sprintf (Obj.magic vdl.dp_conv) 
                                      vdr.dp_sig))
            else
              Data_trans (sprintf (Obj.magic vdl.dp_conv) 
                                  vdr.dp_sig);
         ]
         else
         begin
            let lhs_sl = Str.split (Str.regexp "#") vdl.dp_sig in
            let rhs_sl = Str.split (Str.regexp "#") vdr.dp_sig in
            List.map2 (fun l r -> 
                Data_trans (sprintf "%s <= %s;" l r);
              ) lhs_sl rhs_sl;
         end ) @
        (List.map (fun v -> Data_out v) vdl.dp_aux) @
        (List.map (fun v -> Data_def v) vdl.dp_aux_def) @
        (List.map (fun v -> Data_sens v) vdl.dp_aux_sen) @
        (List.map (fun v -> Data_trans_sens v) vdl.dp_sen) @
        (List.map (fun sv -> Data_trans_def sv) vdl.dp_def) @
        (List.map (fun v -> Data_top v) vdl.top_expr) @
        (List.map (fun v -> Data_top_def v) vdl.top_def) @

        (List.map (fun v -> Data_out v) vdr.dp_aux) @
        (List.map (fun v -> Data_def v) vdr.dp_aux_def) @
        (List.map (fun v -> Data_sens v) vdr.dp_aux_sen) @
        (List.map (fun sv -> Data_trans_def sv) vdr.dp_def) @
        (List.map (fun v -> Data_trans_sens v) vdr.dp_sen) @
        (List.map (fun v -> Data_top v) vdr.top_expr) @
        (List.map (fun v -> Data_top_def v) vdr.top_def) 
    end
    in

  let cp_wr =
  (*
  ** Control Path WR
  *)
    if vdl.cp_sig <> "" then
        [
           Data_cond vdl.cp_sig;
        ] @
        (List.map (fun s -> Data_cond_sens s) vdl.cp_sen)
        else
        []
    in

  let cp_rd =
  (*
  ** Control Path RD
  *)
    if vdr.cp_sig <> "" then
        [
            Data_cond vdr.cp_sig;
        ] @
        (List.map (fun s -> Data_cond_sens s) vdr.cp_sen)
    else
        []
    in
  debug "simple_assign"  with (sprintf "vdl=%s\nvdr=%s" 
                                   (sprint_vhdl_data vdl)
                                   (sprint_vhdl_data vdr)); 
  dp,cp_wr,cp_rd



(*
** Functions. Returns state list. First string specifies
** actual, second next label if any.  
*)
let fun_scode ui label label_next modu pro =
    let pro_main pro =
      let len = String.length pro.pro_name in
      len > 4 && (String.sub pro.pro_name 0 4) = "MOD_" in
    let rec is_local ot =
      if (not (pro_main pro)) then
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
      end
      else false in
    let loc_flag ot = if is_local ot then [UO_loc] else [] in
    
    let instr,block = ui.ui_code,ui.ui_frame in

    let rec scode instr = match instr with
    | Fun ((opl,ot),sel,args) ->
    begin
     match ot with
     | OT_object ao when ao.ao_array <> [] ->
     begin
      (*
      ** Core methods are only applicable to arrays...!
      ** ao_array=[at] -> core object array (var/reg/sig)
      *)
      let at =
        match ao.ao_array with
        | at :: _ -> at;
        | [] -> error 0 "Core methods are only applicable to arrays!" in
      let ota = OT_array at in
      
      let var_type co =
        let db = 
          match co.co_block with
          | Some db -> db;
          | None -> error 726209 "";
          in
        DT_logic (db.db_width)
        in


      let at_dt,at_dt' = 
        match co_of_ot (OT_array at) with
        | Some co -> 
            if List.mem AT_block at.at_flags then
            begin
              let vt = var_type co in
              let n = size_of_dt vt in
              let dt = DT_logic n in
              co.co_type,dt
            end
            else 
             co.co_type,co.co_type
        | None -> error 652146 "" in
      let at_dt_size = size_of_dt at_dt in 

      let get_val arg =
        match arg with
        | UC_val uv -> uv.uv_val;
        | _ -> error 0 (sprintf "Core: unexpected object used in method <%s>"
                                sel) in
      let is_scalar v =
        match v with
        | V_int _
        | V_char  _
        | V_bool _
        | V_logic _ -> true;
        | V_list _ 
        | V_string _ -> false;
        | _ -> error 0 (sprintf "Core: unexpected value data used in method <%s>"
                                sel) in
      let is_local = is_ot_local (Some pro) (OT_array at) in
      (*
      ** State name
      *)
      let snum = ref 0 in
      let sname () =
        if !snum = 0 then
          sprintf "%s" label
        else
          sprintf "%s_%d" label !snum
          in
      let sname_next next = sprintf "%s" next in
      let last_label = sprintf "S_%s_end" pro.pro_name in
      let sname_target label =
        match label with
        | UC_label str -> 
            if str <> "%END" then sprintf "S_%s" str else last_label;
        | _ -> error 501386 "";
        in

      let is_dyn = List.mem AT_dyn at.at_flags in
      let is_block = List.mem AT_block at.at_flags in

      let branch_state this next loop dp cp =
          let rec s = {
                      s_name = this;
                      s_next = Branch (cp,
                                       Next next,
                                       Next loop);
                      s_data = dp;
                      s_block = block;
                    } in
          [s] in
        
      let make_state this next dp cp_wr cp_rd =
        if cp_wr = [] && cp_rd = [] then
        begin
          let s = {
                      s_name = this;
                      s_next = Next next;
                      s_data = dp;
                      s_block = block;
                    } in
          [s]
        end
        else if cp_wr <> [] then 
        begin
          let rec s = {
                      s_name = this;
                      s_next = Branch (cp_wr,
                                       Next this,
                                       Next next);
                      s_data = dp;
                      s_block = block;
                    } in
          [s]
        end 
        else if cp_rd <> [] then 
        begin
          let rec s = {
                      s_name = this;
                      s_next = Branch (cp_rd,
                                       Next this,
                                       Next next);
                      s_data = dp;
                      s_block = block;
                    } in
          [s]
        end 
        else error 126249 "";
        in
      
      match sel with
      | "set" 
      | "copy"
      | "init" ->
      begin
        let sel_size = proda at.at_dim in
        let src = 
          if sel = "set" then List.nth args 2 else
                              List.nth args 0 in
        let lim_a,lim_b =
          if sel = "set" then 
          begin
            (match List.nth args 0 with
             | UA_data (UC_val uv) ->
                OT_value uv.uv_val;
             | _ -> error 0 (sprintf 
                             "set: unexpected subrange argument found in array <%s> statement found."
                             at.at_name);
            ), 
            (match List.nth args 1 with
             | UA_data (UC_val uv) ->
                OT_value uv.uv_val;
             | _ -> error 0 (sprintf 
                             "set: unexpected subrange argument found in array <%s> statement found."
                             at.at_name);
            ) 
          end 
          else
            OT_value (V_int Int64.zero),
            OT_value (V_int (Int64.of_int (sel_size-1))) in
        let a,b =
          (int_of_val (get_value lim_a)),
          (int_of_val (get_value lim_b)) in 

        if a < 0 || b < 0 ||
           a > sel_size ||
           b > sel_size then
           error 0 (sprintf "%s: invalid subrange in array <%s> statement found."
                            sel at.at_name);
        let copy_vector v =
          let is_str,s,is_list,v=
        	  match v with
	          | V_string s -> true,s,false,[];
	          | V_list l -> false,"",true,l;
    	      | _ -> error 781929 "" in
          if not is_dyn && not is_block then
          begin
            (*
            ** All values must be assigned cell for cell, but only
            ** upto "string/vector length" cells...
            *)
            let sl = ref [] in
            let len = Array.length at.at_objs in
            let i =ref 0 in

            if is_str then
            begin
              let slen = String.length s in
              String.iter (fun c ->
                let ot = at.at_objs.(!i) in
                let expr_dt = get_some (expr_dt_of_ot [] ot) in
      	        let lhs = ud_of_ot ot [] ([UO_lhs]@(loc_flag ot)) expr_dt in
                let ot' = OT_value (V_char c) in
                let rhs = ud_of_ot ot' [] [UO_rhs] expr_dt in
                let dp,cp_wr,cp_rd = simple_assign pro lhs rhs in
                let this = sname () in
                incr i;
                incr snum;
                let next = 
                  if !i < slen then sname ()
                     else sname_next label_next in

                sl := !sl @ (make_state this next dp cp_wr cp_rd);
                ) s;
            end else
            begin
              let vlen = List.length v in
              List.iter (fun e ->
                let ot = at.at_objs.(!i) in
                let expr_dt = get_some (expr_dt_of_ot [] ot) in
      	        let lhs = ud_of_ot ot [] ([UO_lhs]@(loc_flag ot)) expr_dt in
                let ot' = OT_value e in
                let rhs = ud_of_ot ot' [] [UO_rhs] expr_dt in
                let dp,cp_wr,cp_rd = simple_assign pro lhs rhs in
                let this = sname () in
                incr i;
                incr snum;
                let next = 
                  if !i < vlen then sname ()
                     else sname_next label_next in

                sl := !sl @ (make_state this next dp cp_wr cp_rd);
                ) v;
            end;
            !sl
          end 
          else
          begin
            let ot = at.at_objs.(0) in
            let expr_dt = get_some (expr_dt_of_ot [] ot) in
            
            let sl = ref [] in
            let len = Array.length at.at_objs in

            let slen = if is_str then String.length s else 
				      List.length v in
            if slen > sel_size then 
              error 0 (sprintf "copy: %s size larger than array <%s>"
                               (if is_str then "string" else "vector")
				at.at_name);
            let lim_b =
              OT_value (V_int (Int64.of_int (slen-1))) in
            let sel_width = 
                    let w = const_width (V_int (Int64.of_int sel_size)) in
                    let s' = 1 lsl (w-1) in
                    if s' = sel_size then (w-1) else w in
            let sel_dt = DT_logic sel_width in
            
            let temp = get_tmp pro sel_dt in
            release_tmp pro temp;
            temp.co_rules <- !core_rules;
            let temp_dt = temp.co_type in
            let temp_ud = ud_of_ot (OT_reg temp) [] [UO_loc;UO_lhs] temp_dt in
            
            let init = 
              let lhs = temp_ud in
              let rhs = ud_of_ot lim_a [] [UO_rhs] temp_dt in
              let dp,cp_wr,cp_rd = simple_assign pro lhs rhs in
              let this = sname () in
              incr snum;
              let next = sname () in
              make_state this next dp cp_wr cp_rd in
              
            sl := init;
            let ar_sel,str_sel,str_sel_nat =
              let selobj = PI_obj ([],OT_reg temp) in
              let ar_sel = ud_of_ot ota [OD_sel_obj selobj]
                                                  ([UO_lhs]@(loc_flag ota)) 
                                                  expr_dt in
              let str_sel = ud_of_ot (OT_reg temp) [] [UO_rhs;UO_loc] expr_dt in
              let str_sel_nat = ud_of_ot (OT_reg temp) [] [UO_rhs;UO_loc] (DT_natural sel_width) in
              ar_sel,str_sel,str_sel_nat in

            let ar_assign,loop =
              let lhs = ar_sel in
              let rhs = 
                (*
                ** Just a place holder for post substitution!
                *)
                UC_val {
                  uv_type = uo_type (DT_logic 0) (DT_logic 0);
                  uv_val = V_string "STRING";
                }
                in
              let dp,cp_wr,cp_rd = simple_assign pro lhs rhs in
              let str_sel = 
                let vd = vhdl_of_ud pro None str_sel_nat None in
                vd in
 
              let dp_def = 
               let slc = ref [] in
               let i = ref 0 in 

               if is_str then             
                 String.iter (fun c ->
                   incr i;
                   slc := !slc @ [
                     Data_top_def (sprintf "      %s%s -- %s (%d)"
                                          (
                                              val_str at_dt'
                                              (V_int (Int64.of_int 
                                                  (int_of_char c)))
                                          )
                                          (if !i < slen then "," else
                                            ");")
                                          label (!i-1));
                   ];
                   ) s
               else
                 List.iter (fun e ->
                  incr i;
                  slc := !slc @ [
                    Data_top_def (sprintf "      %s%s -- %s (%d)"
                                         (val_str at_dt' e)
                                         (if !i < slen then "," else
                                           ");")
                                          label (!i-1));
                  ];
                  ) v;
               [
                Data_top_def (
                  sprintf "type const_%s_%s_array is array(0 to %d)\n    of %s;" 
                          label 
                          (if is_str then "string" else "vector")
                          (slen-1)
                          (obj_decl_type at_dt);
                  );
                Data_top_def (
                  sprintf "constant const_%s_%s: const_%s_%s_array := ("
                          label 
                          (if is_str then "string" else "vector")
                          label
                          (if is_str then "string" else "vector")
                  );
               ] @ !slc @
               (List.map (fun v -> Data_top v) str_sel.top_expr) @
               (List.map (fun v -> Data_top_def v) str_sel.top_def) in
               
              if is_str then info (sprintf "Created string element constant array: <%s> #%d"
                                            label (List.length dp_def));
              let dp = List.map (fun data ->
                match data with
                | Data_out str -> 
                  Data_out (Str.global_replace
                            (Str.regexp "STRING")
                            (sprintf "const_%s_%s(%s)" 
                                     label 
                                     (if is_str then "string" else "vector")
                                     str_sel.dp_sig)
                            str);
                | Data_trans str -> 
                  Data_trans (Str.global_replace
                               (Str.regexp "STRING")
                               (sprintf "const_%s_%s(%s)" 
                                        label 
                                        (if is_str then "string" else "vector")
                                        str_sel.dp_sig)
                               str);
                | _ -> data;
                ) dp in
              
              let this = sname () in
              incr snum;
              let next = sname () in
              make_state this next (dp@dp_def) cp_wr cp_rd,this in

            sl := !sl @ ar_assign;

            let count_and_cmp =
              let this = sname () in
              incr snum;
              let next = sname () in
              let lhs = temp_ud in
              let rhs1 = ud_fix_rhs lhs in
              let rhs2 = ud_of_ot (OT_value (V_int Int64.one)) [] [UO_rhs] temp_dt in
              let dp1,_,cp_rd1 = expr_assign pro [OP_add] lhs rhs1 rhs2 in
              let rhs1 = ud_fix_rhs lhs in
              let rhs2 = ud_of_ot lim_b [] [UO_rhs] temp_dt in
              let dp2,_,cp_rd2 = branch_expr pro [OP_eq] rhs1 rhs2 in
              branch_state this (sname_next label_next)
                                loop (dp1@dp2) 
                                (cp_rd1@cp_rd2) in
              
            sl := !sl @ count_and_cmp;
            !sl;
          
          end in


        match src with
        | UA_data (UC_val {uv_type=_;uv_val=V_int _})
        | UA_data (UC_val {uv_type=_;uv_val=V_logic _})
        | UA_data (UC_val {uv_type=_;uv_val=V_char _}) ->
          if sel = "copy" then
            error 0 (sprintf "copy: unexpected source argument in array <%s> statement found."
                             at.at_name);
          (*
          ** Simple case: it's just a loop
          *)
          let len = Array.length at.at_objs in
          if not is_dyn && not is_block then
          begin
            (*
            ** All initial values must be assigned cell for cell...
            *)
            let sl = ref [] in
            let i =ref 0 in
            Array.iter (fun ot ->
              let expr_dt = get_some (expr_dt_of_ot [] ot) in
              let lhs = ud_of_ot ot [] ([UO_lhs]@(loc_flag ot)) expr_dt in
              let rhs = 
                match src with
                | UA_data d -> d;
                | _ -> error 0 
                        (sprintf
                        "%s: unexpected expression in argument in array <%s> statement found." 
                        sel at.at_name);
                in
              let dp,cp_wr,cp_rd = simple_assign pro lhs rhs in
              let this = sname () in
              incr i;
              incr snum;
              let next = 
                if !i < len then sname ()
                   else sname_next label_next in
              sl := !sl @ (make_state this next dp cp_wr cp_rd);
              ) at.at_objs;
            !sl
          end 
          else
          begin
            let sl = ref [] in
            let len = Array.length at.at_objs in
            let sel_width = 
                    let w = const_width (V_int (Int64.of_int sel_size)) in
                    let s' = 1 lsl (w-1) in
                    if s' = sel_size then (w-1) else w in
            let temp = get_tmp pro (DT_logic sel_width) in
            let expr_dt = DT_logic sel_width in                
            release_tmp pro temp;
            temp.co_rules <- !core_rules;
            let temp_dt = temp.co_type in
            let temp_ud = ud_of_ot (OT_reg temp) [] [UO_loc;UO_lhs] temp_dt in
            let init = 
              let lhs = temp_ud in
              let rhs = ud_of_ot lim_a [] [UO_rhs] temp_dt in
              let dp,cp_wr,cp_rd = simple_assign pro lhs rhs in
              let this = sname () in
              incr snum;
              let next = sname () in
              make_state this next dp cp_wr cp_rd in
              
            sl := init;
            let ar_sel =
              let selobj = PI_obj ([],OT_reg temp) in
              let ot = OT_array at in
              let expr_dt = get_some (expr_dt_of_ot [] ot) in                
              let ar_sel = ud_of_ot ot [OD_sel_obj selobj]
                                       ([UO_lhs]@(loc_flag ot)) 
                                       expr_dt in
              ar_sel in                                    

            let ar_assign,loop =
              let lhs = ar_sel in
              let rhs = 
                match src with
                | UA_data d -> d;
                | _ -> error 0 
                        (sprintf
                        "%s: unexpected expression in argument in array <%s> statement found." 
                        sel at.at_name);
                in
              let dp,cp_wr,cp_rd = simple_assign pro lhs rhs in
              let this = sname () in
              incr snum;
              let next = sname () in
              make_state this next dp cp_wr cp_rd,this in

            sl := !sl @ ar_assign;

            let count_and_cmp =
              let this = sname () in
              incr snum;
              let next = sname () in
              let lhs = temp_ud in
              let rhs1 = ud_fix_rhs lhs in
              let rhs2 = ud_of_ot (OT_value (V_int Int64.one)) [] [UO_rhs] temp_dt in
              let dp1,_,cp_rd1 = expr_assign pro [OP_add] lhs rhs1 rhs2 in
              let rhs1 = ud_fix_rhs lhs in
              let rhs2 = ud_of_ot lim_b [] [UO_rhs] temp_dt in
              let dp2,_,cp_rd2 = branch_expr pro [OP_eq] rhs1 rhs2 in
              branch_state this (sname_next label_next)
                                loop (dp1@dp2) 
                                (cp_rd1@cp_rd2) in
              
            sl := !sl @ count_and_cmp;
            !sl;
          end;
        | UA_data (UC_val {uv_type=_;uv_val=V_string s}) ->
          if sel <> "copy" then
            error 0 (sprintf "%s: unexpected string argument in array <%s> statement found."
                     sel at.at_name);
          copy_vector (V_string s);
        | UA_data (UC_val {uv_type=_;uv_val=V_list v}) ->
          if sel <> "copy" then
            error 0 (sprintf "%s: unexpected vector argument in array <%s> statement found."
                     sel at.at_name);
          copy_vector (V_list v);
        | UA_data (UC_array uat') ->

          let gd_rd,gd_wr = array_guard uat'.uat_obj in

          let at' = uat'.uat_obj in
          if sel <> "copy" then
            error 0 (sprintf "%s: unexpected array argument in array <%s> statement found."
                     sel at.at_name);
                     
          if not is_dyn && not is_block then
            error 537838 "" 
          else
          begin
            let sl = ref [] in
            let temps = ref [] in
            let src_len = proda at'.at_dim in
            let src_dt = uat'.uat_type.uo_data_type in
            let dst_dt = 
                match dt_of_ot (OT_array at) with
                | Some dt -> dt;
                | None -> error 243828 "" in

            let dst_len = proda at.at_dim in
            if src_len > dst_len then 
              error 0 (sprintf "copy: source array size larger than destination array <%s>"
                               at.at_name);
            let lim_b =
              OT_value (V_int (Int64.of_int (src_len-1))) in
            let dst_sel_width = 
                    let w = const_width (V_int (Int64.of_int dst_len)) in
                    let s' = 1 lsl (w-1) in
                    if s' = dst_len then (w-1) else w in
            let src_sel_width = 
                    let w = const_width (V_int (Int64.of_int src_len)) in
                    let s' = 1 lsl (w-1) in
                    if s' = src_len then (w-1) else w in
            let dt = DT_logic (max src_sel_width dst_sel_width) in
            let temp = get_tmp pro dt in
            let expr_dt = dt in
            temps := !temps @ [temp];
            temp.co_rules <- !core_rules;
            let temp_dt = temp.co_type in
            let temp_ud = ud_of_ot (OT_reg temp) [] [UO_loc;UO_lhs] temp_dt in
            let init = 
              let lhs = temp_ud in
              let rhs = ud_of_ot lim_a [] [UO_rhs] temp_dt in
              let dp,cp_wr,cp_rd = simple_assign pro lhs rhs in
              let this = sname () in
              incr snum;
              let next = sname () in
              make_state this next dp cp_wr cp_rd in
              
            sl := init;

            let src_sel,dst_sel =
              let selobj = PI_obj ([],OT_reg temp) in
              let ot = OT_array at in
              let expr_dt = get_some (expr_dt_of_ot [] ot) in
              let dst_sel = ud_of_ot ot [OD_sel_obj selobj]
                                        ([UO_lhs]@(loc_flag ot))
                                        expr_dt in
              let ot' = OT_array at in
              let expr_dt' = get_some (expr_dt_of_ot [] ot') in
              let src_sel = ud_of_ot ot' 
                                     ([OD_sel_obj selobj]@
                                             (match uat'.uat_type.uo_conv with
                                              | Some ct -> [OD_conv ct];
                                              | None -> []  ) )
                                     ([UO_rhs]@(loc_flag ot)) 
                                     expr_dt' in
              src_sel,dst_sel in

            let ar_assign,loop =
              if not gd_rd then
              begin
                  let lhs = dst_sel in
                  let rhs = src_sel in
                  let dp,cp_wr,cp_rd = simple_assign pro lhs rhs in
                  let this = sname () in
                  incr snum;
                  let next = sname () in
                  make_state this next dp cp_wr cp_rd,this 
              end
              else
              begin
                  let tmp_src = get_tmp pro src_dt in 
                  temps := !temps @ [tmp_src];
                  let ot = OT_reg tmp_src in
                  let expr_dt = get_some (expr_dt_of_ot [] ot) in
                  let tmp_src_ud = ud_of_ot ot
                                            [] [UO_loc;UO_lhs] expr_dt in
                  let lhs = tmp_src_ud in
                  let rhs = 
                    match src_sel with
                    | UC_var uo ->
                            UC_var {
                                uo with
                                uo_type = {uo.uo_type with
                                    uo_expr_type=dst_dt;
                                    }};
                    | _ -> error  944439 "" in
                  let dp,cp_wr,cp_rd = simple_assign pro lhs rhs in
                  let this = sname () in
                  let loop = this in
                  incr snum;
                  let next = sname () in
                  let sl1 = make_state this next dp cp_wr cp_rd in
                
                  let lhs = dst_sel in
                  let rhs = 
                    match tmp_src_ud with
                    | UC_reg uo ->
                        ud_fix_rhs ( UC_reg uo  ) 
                    | _ -> error 601205 ""in
        
                  let dp,cp_wr,cp_rd = simple_assign pro lhs rhs in
                  let this = sname () in
                  incr snum;
                  let next = sname () in
                  let sl2 = make_state this next dp cp_wr cp_rd in
                  (sl1@sl2),loop
              end in

            sl := !sl @ ar_assign;

            let count_and_cmp =
              let this = sname () in
              incr snum;
              let next = sname () in
              let lhs = temp_ud in
              let rhs1 = ud_fix_rhs lhs in
              let rhs2 = ud_of_ot (OT_value (V_int Int64.one)) [] [UO_rhs] expr_dt in
              let dp1,_,cp_rd1 = expr_assign pro [OP_add] lhs rhs1 rhs2 in
              let rhs1 = ud_fix_rhs lhs in
              let rhs2 = ud_of_ot lim_b [] [UO_rhs] expr_dt in
              let dp2,_,cp_rd2 = branch_expr pro [OP_eq] rhs1 rhs2 in
              branch_state this (sname_next label_next)
                                loop (dp1@dp2) 
                                (cp_rd1@cp_rd2) in
              
            sl := !sl @ count_and_cmp;


            List.iter (fun temp ->
                release_tmp pro temp;
                ) !temps;
            !sl;
          
          end;
        | _ -> error 0 (sprintf "%s: unexpected value in array <%s> statement found."
                        sel at.at_name);
      end;
      | "copyn" ->
      begin
        let src = List.nth args 3 in
        let dst_a,src_a,len =
            (match List.nth args 0 with
             | UA_data (UC_val uv) ->
                OT_value uv.uv_val;
             | _ -> error 0 (sprintf 
                             "set: unexpected index argument in array <%s> statement found."
                             at.at_name);
            ), 
            (match List.nth args 1 with
             | UA_data (UC_val uv) ->
                OT_value uv.uv_val;
             | _ -> error 0 (sprintf 
                             "set: unexpected index argument in array <%s> statement found."
                             at.at_name);
            ),
            (match List.nth args 2 with
             | UA_data (UC_val uv) ->
                OT_value uv.uv_val;
             | _ -> error 0 (sprintf 
                             "set: unexpected length argument in array <%s> statement found."
                             at.at_name);
            ) 
            in

    	let copyn_vector v = 
	      let is_str,s,is_list,v=
    	  match v with
	      | V_string s -> true,s,false,[];
	      | V_list l -> false,"",true,l;
	      | _ -> error 781920 "" in

          let src_len = if is_str then String.length s else
				       List.length v in
          let dst_len = proda at.at_dim in

          let a,b,c =
            (int_of_val (get_value dst_a)),
            (int_of_val (get_value src_a)),
            (int_of_val (get_value len)) in 


          if a < 0 || b < 0 ||
             (a+c) > dst_len ||
             (b+c) > src_len then
           error 0 (sprintf "%s: index parameters out of range in array <%s> statement found."
                            sel at.at_name);
          let dst_b = 
            OT_value (V_int (Int64.of_int (a+c-1))) in
                     
          if not is_dyn && not is_block then
            error 537838 "" 
          else
          begin
            let sl = ref [] in
            let dst_sel_width = 
                    let w = const_width (V_int (Int64.of_int dst_len)) in
                    let s' = 1 lsl (w-1) in
                    if s' = dst_len then (w-1) else w in
            let src_sel_width = 
                    let w = const_width (V_int (Int64.of_int src_len)) in
                    let s' = 1 lsl (w-1) in
                    if s' = src_len then (w-1) else w in
            let dst_sel_dt = DT_logic dst_sel_width in
            let src_sel_dt = DT_logic src_sel_width in

            let temp_dst = get_tmp pro (DT_logic dst_sel_width) in
            temp_dst.co_rules <- !core_rules;
            let temp_src = get_tmp pro (DT_logic src_sel_width) in
            temp_src.co_rules <- !core_rules;
            let expr_dt = temp_dst.co_type in
            let expr_dt' = temp_src.co_type in
            let temp_ud_dst = ud_of_ot (OT_reg temp_dst) [] 
                                                         [UO_loc;UO_lhs] expr_dt in
            let temp_ud_src = ud_of_ot (OT_reg temp_src) [] 
                                                         [UO_loc;UO_lhs] expr_dt' in
            let init = 
              let lhs = temp_ud_dst in
              let rhs = ud_of_ot dst_a [] [UO_rhs] expr_dt in
              let dp1,cp_wr1,cp_rd1 = simple_assign pro lhs rhs in
              let lhs = temp_ud_src in
              let rhs = ud_of_ot src_a [] [UO_rhs] expr_dt' in
              let dp2,cp_wr2,cp_rd2 = simple_assign pro lhs rhs in
              let this = sname () in
              incr snum;
              let next = sname () in
              make_state this next (dp1@dp2) (cp_wr1@cp_wr2) 
                                   (cp_rd1@cp_rd2) in
              
            sl := init;
            let src_sel,dst_sel =
              let selobj = PI_obj ([],OT_reg temp_dst) in
              let ot = OT_array at in
              let expr_dt = get_some (expr_dt_of_ot [] ot) in
              let dst_sel = ud_of_ot ot [OD_sel_obj selobj]
                                        ([UO_lhs]@(loc_flag ot)) 
                                        expr_dt in
              let expr_dt' = DT_int 9999 in
              let src_sel = ud_of_ot (OT_reg temp_src) [] [UO_rhs;UO_loc] expr_dt' in
              src_sel,dst_sel in

            let ar_assign,loop =
              let lhs = dst_sel in
              let rhs = 
                (*
                ** Just a place holder for post substitution!
                *)
                UC_val {
                  uv_type = uo_type (DT_logic 0) (DT_logic 0);
                  uv_val = V_string "STRING";
                }
                in
              let dp,cp_wr,cp_rd = simple_assign pro lhs rhs in
              let str_sel = 
                let vd = vhdl_of_ud pro None src_sel None in
                vd.dp_sig in
              let dp_def = 
               let slc = ref [] in
               let i = ref 0 in               
               if is_str then
                String.iter (fun c ->
                 incr i;
                 slc := !slc @ [
                   Data_top_def (sprintf "      %s%s"
                                        (val_str at_dt
                                         (V_int (Int64.of_int 
                                                 (int_of_char c))))
                                        (if !i < src_len then "," else
                                          ");"));
                 ];
                 ) s
               else
                List.iter (fun e ->
                 incr i;
                 slc := !slc @ [
                   Data_top_def (sprintf "      %s%s"
                                        (val_str at_dt e)
                                        (if !i < src_len then "," else
                                          ");"));
                 ];
                 ) v;
               [
                Data_top_def (
                  sprintf "type const_%s_%s_array is array(0 to %d)\n    of %s;" 
                          label 
                          (if is_str then "string" else "vector")
                          (src_len-1)
                          (obj_decl_type at_dt);
                  );
                Data_top_def (
                  sprintf "constant const_%s_%s: const_%s_%s_array := ("
                          label 
                          (if is_str then "string" else "vector")
                          label
                          (if is_str then "string" else "vector")
                  );
               ] @ !slc in
              let dp = List.map (fun data ->
                match data with
                | Data_out str -> 
                  Data_out (Str.global_replace
                            (Str.regexp "STRING")
                            (sprintf "const_%s_%s(%s)" 
                                    label 
                                    (if is_str then "string" else "vector")
                                    str_sel)
                            str);
                | _ -> data;
                ) dp in
                                                                                                                                                                                                                                                                                                                                                            
              let this = sname () in
              incr snum;
              let next = sname () in
              make_state this next (dp@dp_def) cp_wr cp_rd,this in

            sl := !sl @ ar_assign;

            let count_and_cmp =
              let this = sname () in
              incr snum;
              let next = sname () in
              let lhs = temp_ud_dst in
              let rhs1 = ud_fix_rhs lhs in
              let rhs2 = ud_of_ot (OT_value (V_int Int64.one)) [] [UO_rhs] expr_dt in
              let dp1,_,cp_rd1 = expr_assign pro [OP_add] lhs rhs1 rhs2 in
              let lhs = temp_ud_src in
              let rhs1 = ud_fix_rhs lhs in
              let rhs2 = ud_of_ot (OT_value (V_int Int64.one)) [] [UO_rhs] expr_dt' in
              let dp2,_,cp_rd2 = expr_assign pro [OP_add] lhs rhs1 rhs2 in

              let lhs = temp_ud_dst in
              let rhs1 = ud_fix_rhs lhs in
              let rhs2 = ud_of_ot dst_b [] [UO_rhs] expr_dt in
              let dp3,_,cp_rd3 = branch_expr pro [OP_eq] rhs1 rhs2 in
              branch_state this (sname_next label_next)
                                loop (dp1@dp2@dp3) 
                                (cp_rd1@cp_rd2@cp_rd3) in
              
            sl := !sl @ count_and_cmp;

            release_tmp pro temp_dst;
            release_tmp pro temp_src;

            !sl;
          end in

        match src with
        | UA_data (UC_array uat') ->
          let at' = uat'.uat_obj in

          let gd_rd,gd_wr = ud_guard (UC_array uat') in

          let src_len = proda at'.at_dim in
          let dst_len = proda at.at_dim in

          let a,b,c =
            (int_of_val (get_value dst_a)),
            (int_of_val (get_value src_a)),
            (int_of_val (get_value len)) in 

          if a < 0 || b < 0 ||
             (a+c) > dst_len ||
             (b+c) > src_len then
           error 0 (sprintf "%s: index parameters out of range in array <%s> statement found."
                            sel at.at_name);
          let dst_b = 
            OT_value (V_int (Int64.of_int (a+c-1))) in
                     
          if not is_dyn && not is_block then
            error 537838 "" 
          else
          begin
            let sl = ref [] in
            let temps = ref [] in
            let dst_sel_width = 
                    let w = const_width (V_int (Int64.of_int dst_len)) in
                    let s' = 1 lsl (w-1) in
                    if s' = dst_len then (w-1) else w in
            let src_sel_width = 
                    let w = const_width (V_int (Int64.of_int src_len)) in
                    let s' = 1 lsl (w-1) in
                    if s' = src_len then (w-1) else w in
            let dst_sel_dt = DT_logic dst_sel_width in
            let src_sel_dt = DT_logic src_sel_width in
                
            let src_dt = uat'.uat_type.uo_data_type in
            let dst_dt = 
                match dt_of_ot (OT_array at) with
                | Some dt -> dt;
                | None -> error 243829 "" in

            let temp_dst = get_tmp pro (DT_logic dst_sel_width) in
            temp_dst.co_rules <- !core_rules;
            temps := !temps @ [temp_dst];
            let temp_src = get_tmp pro (DT_logic src_sel_width) in
            temp_src.co_rules <- !core_rules;
            temps := !temps @ [temp_src];
            let expr_dt = DT_logic dst_sel_width in
            let expr_dt' = DT_logic src_sel_width in
            let temp_ud_dst = ud_of_ot (OT_reg temp_dst) [] 
                                                         [UO_loc;UO_lhs] expr_dt in
            let temp_ud_src = ud_of_ot (OT_reg temp_src) [] 
                                                         [UO_loc;UO_lhs] expr_dt' in

            let init = 
              let lhs = temp_ud_dst in
              let rhs = ud_of_ot dst_a [] [UO_rhs] expr_dt in
              let dp1,cp_wr1,cp_rd1 = simple_assign pro lhs rhs in
              let lhs = temp_ud_src in
              let rhs = ud_of_ot src_a [] [UO_rhs] expr_dt' in
              let dp2,cp_wr2,cp_rd2 = simple_assign pro lhs rhs in
              let this = sname () in
              incr snum;
              let next = sname () in
              make_state this next (dp1@dp2) (cp_wr1@cp_wr2) 
                                   (cp_rd1@cp_rd2) in
              
            sl := init;
            let src_sel,dst_sel =
              let selobj = PI_obj ([],OT_reg temp_dst) in
              let ot = OT_array at in
              let expr_dt = get_some (expr_dt_of_ot [] ot) in
              let dst_sel = ud_of_ot ot [OD_sel_obj selobj]
                                        ([UO_lhs]@(loc_flag ot)) 
                                        expr_dt in
              let selobj = PI_obj ([],OT_reg temp_src) in
              let ot' = OT_array at' in
              let expr_dt' = get_some (expr_dt_of_ot [] ot') in
              let src_sel = ud_of_ot ot' ([OD_sel_obj selobj]@
                                             (match uat'.uat_type.uo_conv with
                                              | Some ct -> [OD_conv ct];
                                              | None -> []  ) )
                                         ([UO_rhs]@(loc_flag ot)) 
                                         expr_dt' in
              src_sel,dst_sel in

            let ar_assign,loop =
              if not gd_rd then
              begin
                  let lhs = dst_sel in
                  let rhs = src_sel in
                  let dp,cp_wr,cp_rd = simple_assign pro lhs rhs in
                  let this = sname () in
                  incr snum;
                  let next = sname () in
                  make_state this next dp cp_wr cp_rd,this 
              end
              else
              begin
                  let tmp_src = get_tmp pro src_dt in 
                  temps := !temps @ [tmp_src];
                  let tmp_src_ud = ud_of_ot (OT_reg tmp_src) 
                                            [] [UO_loc;UO_lhs] expr_dt' in
                  let lhs = tmp_src_ud in
                  let rhs = 
                    match src_sel with
                    | UC_var uo ->
                            UC_var {
                                uo with
                                uo_type = {uo.uo_type with
                                            uo_expr_type=dst_dt;
                                    }};
                    | _ -> error  944439 "" in
                  let dp,cp_wr,cp_rd = simple_assign pro lhs rhs in
                  let this = sname () in
                  let loop = this in
                  incr snum;
                  let next = sname () in
                  let sl1 = make_state this next dp cp_wr cp_rd in
                
                  let lhs = dst_sel in
                  let rhs = 
                    match tmp_src_ud with
                    | UC_reg uo ->
                        ud_fix_rhs ( UC_reg uo ) 
                    | _ -> error 601205 ""in
        
                  let dp,cp_wr,cp_rd = simple_assign pro lhs rhs in
                  let this = sname () in
                  incr snum;
                  let next = sname () in
                  let sl2 = make_state this next dp cp_wr cp_rd in
                  (sl1@sl2),loop
              end in

            sl := !sl @ ar_assign;

            let count_and_cmp =
              let this = sname () in
              incr snum;
              let next = sname () in
              let lhs = temp_ud_dst in
              let rhs1 = ud_fix_rhs lhs in
              let rhs2 = ud_of_ot (OT_value (V_int Int64.one)) [] [UO_rhs] expr_dt in
              let dp1,_,cp_rd1 = expr_assign pro [OP_add] lhs rhs1 rhs2 in
              let lhs = temp_ud_src in
              let rhs1 = ud_fix_rhs lhs in
              let rhs2 = ud_of_ot (OT_value (V_int Int64.one)) [] [UO_rhs] expr_dt' in
              let dp2,_,cp_rd2 = expr_assign pro [OP_add] lhs rhs1 rhs2 in

              let lhs = temp_ud_dst in
              let rhs1 = ud_fix_rhs lhs in
              let rhs2 = ud_of_ot dst_b [] [UO_rhs] expr_dt in
              let dp3,_,cp_rd3 = branch_expr pro [OP_eq] rhs1 rhs2 in
              branch_state this (sname_next label_next)
                                loop (dp1@dp2@dp3) 
                                (cp_rd1@cp_rd2@cp_rd3) in
              
            sl := !sl @ count_and_cmp;
            List.iter (fun temp ->
                release_tmp pro temp;
                ) !temps;
            !sl;
          end;
        | UA_data (UC_val {uv_type=_;uv_val=V_string s}) ->
            copyn_vector (V_string s);
        | UA_data (UC_val {uv_type=_;uv_val=V_list v}) ->
            copyn_vector (V_list v);
        | _ -> error 0 (sprintf "%s: unexpected value in array <%s> statement found."
                        sel at.at_name);
      end;
      | "cmp_eq"
      | "cmp_neq" 
      | "cmpn_eq"
      | "cmpn_neq" ->
      begin
        let neq = sel = "cmp_neq" || sel = "cmpn_neq" in

        let op1_size = proda at.at_dim in
        let op1_a,op2_a,len =
          if sel = "cmpn_eq" || sel = "cmpn_neq" then 
          begin
            (match List.nth args 1 with
             | UA_data (UC_val uv) ->
                OT_value uv.uv_val;
             | _ -> error 0 (sprintf 
                             "%s: unexpected subrange argument 1 found in array <%s> statement found."
                             sel at.at_name);
            ), 
            (match List.nth args 2 with
             | UA_data (UC_val uv) ->
                OT_value uv.uv_val;
             | _ -> error 0 (sprintf 
                             "%s: unexpected subrange argument 2 found in array <%s> statement found."
                             sel at.at_name);
            ),
            (match List.nth args 3 with
             | UA_data (UC_val uv) ->
                OT_value uv.uv_val;
             | _ -> error 0 (sprintf 
                             "%s: unexpected subrange argument 3 found in array <%s> statement found."
                             sel at.at_name);
            ) 
          end 
          else
            OT_value (V_int Int64.zero),
            OT_value (V_int Int64.zero),
            OT_value (V_int (Int64.of_int op1_size)) in


        let a1,a2,len =
          (int_of_val (get_value op1_a)),
          (int_of_val (get_value op2_a)),
          (int_of_val (get_value len)) in 

        let b1 = a1+len-1 in
        let op1_b = OT_value (V_int (Int64.of_int b1)) in
        
        if a1 < 0 || a2 < 0 ||
           (a1 + len) > op1_size ||
           (a2 + len) > op1_size then
           error 0 (sprintf "%s: invalid subrange in array <%s> statement found."
                            sel at.at_name);

        let res,op2 = 
          if sel = "cmpn_eq" || sel = "cmpn_neq" then 
          begin
            match args with
            | [UA_data res; _;_;_;op2] -> res,op2;
            | _ -> error 177529 "" 
          end
          else
          begin
            match args with
            | [UA_data res ; op2] -> res,op2;
            | _ -> error 177530 "" 
          end in

        let vdl_res = vhdl_of_ud pro None res None in


        let cmp_vector v =
          let op1_b =
            match v with
            | V_string s ->
                let len = String.length s in
                OT_value (V_int (Int64.of_int (a1+len-1)));
            | V_list l ->
                let len = List.length l in
                OT_value (V_int (Int64.of_int (a1+len-1)));
            | _ -> op1_b in
          let is_str,s,is_list,v=
                match v with
                | V_string s -> true,s,false,[];
                | V_list l -> false,"",true,l;
                | _ -> error 781921 "" in


          if not is_dyn && not is_block then
          begin
            (*
            ** All values must be compared cell for cell, but only
            ** upto string length cells...
            *)
            let sl = ref [] in
            let i =ref 0 in
            let slen = if is_str then String.length s else
                                      List.length v in

            if slen < len && len <> op1_size then 
              error 0 (sprintf "%s: %s size smaller than array subrange <%s>"
                               sel
                               (if is_str then "string" else "vector")
                               at.at_name);
            if (a1+slen) > op1_size then 
              error 0 (sprintf "%s: %s exceeds array limits <%s>"
                               sel
                               (if is_str then "string" else "vector")
                               at.at_name);

            let this = sname () in
            incr snum;
            let next = sname () in
            sl := [{
                    s_name = label;
                    s_next = Next next;
                    s_data = [
                        Data_trans (sprintf   
                                    "%s <= '1';"
                                    vdl_res.dp_sig 
                                   );
                        Data_trans_def (vdl_res.dp_sig,"'0'")
                        ];
                    s_block = block;
                }];


            if is_str then String.iter (fun c ->
              let ot = at.at_objs.(!i+a1) in
              let expr_dt = get_some (expr_dt_of_ot [] ot) in
              let op1 = ud_of_ot ot [] ([UO_rhs]@(loc_flag ot)) expr_dt in
              let ot' = OT_value (V_char c) in
              let expr_dt' = get_some (expr_dt_of_ot [] ot') in
              let op2 = ud_of_ot ot' [] [UO_rhs] expr_dt' in

              let vdl_op1 = vhdl_of_ud pro None op1 None in
              let vdl_op2 = vhdl_of_ud pro None op2 None in

              let gd_rd1,_ = ud_guard op1 in

              let this = sname () in
              incr i;
              incr snum;
              let next = 
                if !i < slen then sname ()
                   else sname_next label_next in
                
              let s = 
                if not gd_rd1 then 
                {
                    s_name = this;
                    s_next = Next next;
                    s_data = 
                    [
                        Data_trans (sprintf   
                                    "if %s %s %s then"
                                    vdl_op1.dp_sig
                                    (if neq then "/=" else "=")
                                    vdl_op2.dp_sig
                                   );
                        Data_trans (sprintf   
                                    "  %s <= %s and '1';"
                                    vdl_res.dp_sig 
                                    vdl_res.dp_sig 
                                   );
                        Data_trans (sprintf   
                                    "else"
                                   );
                        Data_trans (sprintf   
                                    "  %s <= '0';"
                                    vdl_res.dp_sig
                                   );
                        Data_trans (sprintf   
                                    "end if;"
                                   );
                        Data_trans_def (vdl_res.dp_sig,"'0'");
                    ]@
                    (if not (is_val op2) then
                      [
                        Data_trans_sens (sprintf "%s"
                                                 vdl_op2.dp_sig);
                      ] 
                      else []
                    ) @ (List.map (fun s -> Data_trans_sens s) vdl_op1.dp_sen)
                      @ (List.map (fun s -> Data_trans_sens s) vdl_op2.dp_sen)
                      @ (List.map (fun s -> Data_out s) vdl_op1.dp_aux)
                      @ (List.map (fun s -> Data_def s) vdl_op1.dp_aux_def)
                      ;
                    s_block = block;
                } 
                else
                begin
                  let gd_sig = sig_guard vdl_op1 in
                  let cp = [
                    Data_cond vdl_op1.cp_sig;
                    ]@
                    (List.map (fun s -> Data_cond_sens s) vdl_op1.cp_sen) in
                  let rec s = {
                    s_name = this;
                    s_next = Branch(cp,Next this,Next next);
                    s_data = 
                    [
                        Data_trans (sprintf "if %s = '0' then"
                                             gd_sig);
                        Data_trans_sens gd_sig;
                        Data_trans (sprintf   
                                    "  if %s %s %s then"
                                    vdl_op1.dp_sig
                                    (if neq then "/=" else "=")
                                    vdl_op2.dp_sig
                                   );
                        Data_trans (sprintf   
                                    "    %s <= %s and '1';"
                                    vdl_res.dp_sig 
                                    vdl_res.dp_sig 
                                   );
                        Data_trans (sprintf   
                                    "  else"
                                   );
                        Data_trans (sprintf   
                                    "    %s <= '0';"
                                    vdl_res.dp_sig
                                   );
                        Data_trans (sprintf   
                                    "  end if;"
                                   );
                        Data_trans_def (vdl_res.dp_sig,"'0'");
                        Data_trans "end if;"
                    ] @
                    (if not (is_val op2) then
                      [
                        Data_trans_sens (sprintf "%s"
                                                 vdl_op2.dp_sig);
                      ] 
                      else []
                    ) @ (List.map (fun s -> Data_trans_sens s) vdl_op1.dp_sen)
                      @ (List.map (fun s -> Data_trans_sens s) vdl_op2.dp_sen)
                      @ (List.map (fun s -> Data_out s) vdl_op1.dp_aux)
                      @ (List.map (fun s -> Data_def s) vdl_op1.dp_aux_def)
                      ;
                    s_block = block;
                  } in
                  s;
                end
                in
              sl := !sl @ [s];
              ) s
            else List.iter (fun e ->
              let ot = at.at_objs.(!i+a1) in
              let expr_dt = get_some (expr_dt_of_ot [] ot) in
              let op1 = ud_of_ot ot [] ([UO_rhs]@(loc_flag ot)) expr_dt in
              let ot' = OT_value e in
              let expr_dt' = get_some (expr_dt_of_ot [] ot') in
              let op2 = ud_of_ot ot' [] [UO_rhs] expr_dt' in

              let vdl_op1 = vhdl_of_ud pro None op1 None in
              let vdl_op2 = vhdl_of_ud pro None op2 None in

              let gd_rd1,_ = ud_guard op1 in

              let this = sname () in
              incr i;
              incr snum;
              let next = 
                if !i < slen then sname ()
                   else sname_next label_next in
                
              let s = 
                if not gd_rd1 then 
                {
                    s_name = this;
                    s_next = Next next;
                    s_data = 
                    [
                        Data_trans (sprintf   
                                    "if %s %s %s then"
                                    vdl_op1.dp_sig
                                    (if neq then "/=" else "=")
                                    vdl_op2.dp_sig
                                   );
                        Data_trans (sprintf   
                                    "  %s <= %s and '1';"
                                    vdl_res.dp_sig 
                                    vdl_res.dp_sig 
                                   );
                        Data_trans (sprintf   
                                    "else"
                                   );
                        Data_trans (sprintf   
                                    "  %s <= '0';"
                                    vdl_res.dp_sig
                                   );
                        Data_trans (sprintf   
                                    "end if;"
                                   );
                        Data_trans_def (vdl_res.dp_sig,"'0'");
                    ]@
                    (if not (is_val op2) then
                      [
                        Data_trans_sens (sprintf "%s"
                                                 vdl_op2.dp_sig);
                      ] 
                      else []
                    ) @ (List.map (fun s -> Data_trans_sens s) vdl_op1.dp_sen)
                      @ (List.map (fun s -> Data_trans_sens s) vdl_op2.dp_sen)
                      @ (List.map (fun s -> Data_out s) vdl_op1.dp_aux)
                      @ (List.map (fun s -> Data_def s) vdl_op1.dp_aux_def)
                      ;
                    s_block = block;
                    } 
                else
                begin
                  let gd_sig = sig_guard vdl_op1 in
                  let cp = [
                    Data_cond vdl_op1.cp_sig;
                    ]@
                    (List.map (fun s -> Data_cond_sens s) vdl_op1.cp_sen) in
                  let rec s = {
                    s_name = this;
                    s_next = Branch(cp,Next this,Next next);
                    s_data = 
                    [
                        Data_trans (sprintf "if %s = '0' then"
                                                gd_sig);
                        Data_trans_sens gd_sig;
                        Data_trans (sprintf   
                                    "  if %s %s %s then"
                                    vdl_op1.dp_sig
                                    (if neq then "/=" else "=")
                                    vdl_op2.dp_sig
                                   );
                        Data_trans (sprintf   
                                    "    %s <= %s and '1';"
                                    vdl_res.dp_sig 
                                    vdl_res.dp_sig 
                                   );
                        Data_trans (sprintf   
                                    "  else"
                                   );
                        Data_trans (sprintf   
                                    "    %s <= '0';"
                                    vdl_res.dp_sig
                                   );
                        Data_trans (sprintf   
                                    "  end if;"
                                   );
                        Data_trans_def (vdl_res.dp_sig,"'0'");
                        Data_trans "end if;"
                    ] @
                    (if not (is_val op2) then
                      [
                        Data_trans_sens (sprintf "%s"
                                                 vdl_op2.dp_sig);
                      ] 
                      else []
                    ) @ (List.map (fun s -> Data_trans_sens s) vdl_op1.dp_sen)
                      @ (List.map (fun s -> Data_trans_sens s) vdl_op2.dp_sen)
                      @ (List.map (fun s -> Data_out s) vdl_op1.dp_aux)
                      @ (List.map (fun s -> Data_def s) vdl_op1.dp_aux_def)
                      ;
                    s_block = block;
                    } in
                  s;
                 end;
                 in
              sl := !sl @ [s];
              ) v;
            !sl
          end 
          else 
          begin
            let sl = ref [] in
            let slen = if is_str then String.length s else
                                      List.length v in

            if slen < len && len <> op1_size then 
              error 0 (sprintf "%s: %s size smaller than array subrange <%s>"
                               sel
                               (if is_str then "string" else "vector")
                               at.at_name);
            if (a1+slen) > op1_size then 
              error 0 (sprintf "%s: %s exceeds array limits <%s>"
                               sel
                               (if is_str then "string" else "vector")
                               at.at_name);
            let sel_width = 
                    let w = const_width (V_int (Int64.of_int op1_size)) in
                    let s' = 1 lsl (w-1) in
                    if s' = op1_size then (w-1) else w in

            let expr_dt = DT_logic sel_width in
            let temp1 = get_tmp pro (DT_logic sel_width) in
            let temp2 =
                if a1 = 0 then temp1 else  
                get_tmp pro (DT_logic sel_width) in
            temp1.co_rules <- !core_rules;
            if a1 <> 0 then 
                temp2.co_rules <- !core_rules;

            let temp1_ud = ud_of_ot (OT_reg temp1) [] [UO_loc;UO_lhs] expr_dt in
            let temp2_ud =
                if a1 = 0 then temp1_ud else
                    ud_of_ot (OT_reg temp2) [] [UO_loc;UO_lhs] expr_dt in

            let init = 
              let lhs = temp1_ud in
              let rhs = ud_of_ot op1_a [] [UO_rhs] expr_dt in
              let dp1,cp_wr1,cp_rd1 = simple_assign pro lhs rhs in
              let dp2 = [
                        Data_trans (sprintf   
                                    "%s <= '1';"
                                    vdl_res.dp_sig 
                                   );
                        Data_trans_def (vdl_res.dp_sig,"'0'")
                        ] in
              let this = sname () in
              incr snum;
              let next = sname () in
              if a1 = 0 then
                make_state this next (dp1@dp2) cp_wr1 cp_rd1 
              else
              begin
                let lhs = temp2_ud in
                let rhs = ud_of_ot op2_a [] [UO_rhs] expr_dt in
                let dp3,cp_wr3,cp_rd3 = simple_assign pro lhs rhs in
                make_state this next (dp1@dp2@dp3) (cp_wr1@cp_wr3) 
                                                   (cp_rd1@cp_rd3) 

              end
              in


            sl := !sl @ init;

            let ar_sel,str_sel =
              let selobj = PI_obj ([],OT_reg temp1) in
              let ot = OT_array at in
              let expr_dt = get_some (expr_dt_of_ot [] ot) in
              let ar_sel = ud_of_ot ot [OD_sel_obj selobj]
                                       ([UO_rhs]@(loc_flag ot)) 
                                       expr_dt in
              let expr_dt' = DT_int 9999 in
              let str_sel = ud_of_ot (OT_reg temp2) [] [UO_rhs;UO_loc] expr_dt' in
              ar_sel,str_sel in

            let ar_cmp,loop =
              let op1 = ar_sel in

              let str_sel = 
                let vd = vhdl_of_ud pro None str_sel None in
                vd.dp_sig in

              let op2_sig = sprintf "const_%s_%s(%s)" 
                                    label 
                                    (if is_str then "string" else "vector")
                                    str_sel in
              let vdl_op1 = vhdl_of_ud pro None op1 None in
              let gd_rd1,_ = ud_guard op1 in

              let slc = ref [] in
              let i = ref 0 in               
              slc := [
                Data_top_def (
                  sprintf "type const_%s_%s_array is array(0 to %d)\n    of %s;" 
                          label 
                          (if is_str then "string" else "vector")
                          (slen-1)
                          (obj_decl_type at_dt);
                  );
                Data_top_def (
                  sprintf "constant const_%s_%s: const_%s_%s_array := ("
                          label 
                          (if is_str then "string" else "vector")
                          label
                          (if is_str then "string" else "vector"));
                ];
              if is_str then String.iter (fun c ->
                 incr i;
                 slc := !slc @ [
                   Data_top_def (sprintf "      %s%s"
                                        (
                                            val_str at_dt
                                            (V_int (Int64.of_int 
                                                (int_of_char c)))
                                        )
                                        (if !i < slen then "," else
                                          ");"));
                 ];
                 ) s
              else List.iter (fun e ->
                 incr i;
                 slc := !slc @ [
                   Data_top_def (sprintf "      %s%s"
                                        (val_str at_dt e)
                                        (if !i < slen then "," else
                                          ");"));
                 ];
                 ) v;

              let this = sname () in
              incr snum;
              let next = sname () in
                
              let s = 
                if not gd_rd1 then
                {
                    s_name = this;
                    s_next = Next next;
                    s_data = 
                    [
                        Data_trans (sprintf   
                                    "if %s %s %s then"
                                    vdl_op1.dp_sig
                                    (if neq then "/=" else "=")
                                    op2_sig
                                   );
                        Data_trans (sprintf   
                                    "  %s <= %s and '1';"
                                    vdl_res.dp_sig 
                                    vdl_res.dp_sig 
                                   );
                        Data_trans (sprintf   
                                    "else"
                                   );
                        Data_trans (sprintf   
                                    "  %s <= '0';"
                                    vdl_res.dp_sig
                                   );
                        Data_trans (sprintf   
                                    "end if;"
                                   );
                       Data_trans_def (vdl_res.dp_sig,"'0'");
                    ] 
                    @ (List.map (fun s -> Data_trans_sens s) vdl_op1.dp_sen)
                    @ (List.map (fun s -> Data_out s) vdl_op1.dp_aux)
                    @ (List.map (fun s -> Data_def s) vdl_op1.dp_aux_def)
                    @ !slc;
                    s_block = block;
                } 
                else
                begin
                  let gd_sig = sig_guard vdl_op1 in
                  let cp = [
                    Data_cond vdl_op1.cp_sig;
                    ]@
                    (List.map (fun s -> Data_cond_sens s) vdl_op1.cp_sen) in
                  let rec s = {
                    s_name = this;
                    s_next = Branch(cp,Next this,Next next);
                    s_data = 
                    [
                        Data_trans (sprintf "if %s = '0' then"
                                             gd_sig);
                        Data_trans_sens gd_sig;
                        Data_trans (sprintf   
                                    "  if %s %s %s then"
                                    vdl_op1.dp_sig
                                    (if neq then "/=" else "=")
                                    op2_sig
                                   );
                        Data_trans (sprintf   
                                    "    %s <= %s and '1';"
                                    vdl_res.dp_sig 
                                    vdl_res.dp_sig 
                                   );
                        Data_trans (sprintf   
                                    "  else"
                                   );
                        Data_trans (sprintf   
                                    "    %s <= '0';"
                                    vdl_res.dp_sig
                                   );
                        Data_trans (sprintf   
                                    "  end if;"
                                   );
                       Data_trans_def (vdl_res.dp_sig,"'0'");
                       Data_trans "end if;"
                    ]
                    @ (List.map (fun s -> Data_trans_sens s) vdl_op1.dp_sen)
                    @ (List.map (fun s -> Data_out s) vdl_op1.dp_aux)
                    @ (List.map (fun s -> Data_def s) vdl_op1.dp_aux_def)
                    @ !slc;
                    s_block = block;
                  } in
                  s;
                end;
                in


              [s],this in

            sl := !sl @ ar_cmp;

            let count_and_cmp =
              let this = sname () in
              incr snum;
              let next = sname () in
              let lhs = temp1_ud in
              let rhs1 = ud_fix_rhs lhs in
              let rhs2 = ud_of_ot (OT_value (V_int Int64.one)) [] [UO_rhs] expr_dt in
              let dp1,_,cp_rd1 = expr_assign pro [OP_add] lhs rhs1 rhs2 in
              let dp2,cp_rd2 =
                if a1 = 0 then [],[] else
                begin
                  let lhs = temp2_ud in
                  let rhs1 = ud_fix_rhs lhs in
                  let rhs2 = ud_of_ot (OT_value (V_int Int64.one)) [] [UO_rhs] expr_dt in
                  let dp,_,cp_rd = expr_assign pro [OP_add] lhs rhs1 rhs2 in
                  dp,cp_rd
                end in
              let rhs1 = ud_fix_rhs lhs in
              let rhs2 = ud_of_ot op1_b [] [UO_rhs] expr_dt in
              let dp3,_,cp_rd3 = branch_expr pro [OP_eq] rhs1 rhs2 in
              branch_state this (sname_next label_next)
                                loop (dp1@dp2@dp3) 
                                (cp_rd1@cp_rd2@cp_rd3) in
              
            sl := !sl @ count_and_cmp;

            release_tmp pro temp1;
            if a1 <> 0 then 
                release_tmp pro temp2;

            !sl;
          end in

        match op2 with
        | UA_data (UC_val {uv_type=_;uv_val=V_int _})
        | UA_data (UC_val {uv_type=_;uv_val=V_logic _})
        | UA_data (UC_val {uv_type=_;uv_val=V_char _}) ->
          (*
          ** Simple case: it's just a loop
          *)
          if not is_dyn && not is_block then
          begin
            (*
            ** All array elements must be compared with scalar value...
            *)
            let sl = ref [] in
            let this = sname () in
            incr snum;
            let next = sname () in
            sl := [{
                    s_name = label;
                    s_next = Next next;
                    s_data = [
                        Data_trans (sprintf   
                                    "%s <= '1';"
                                    vdl_res.dp_sig 
                                   );
                        Data_trans_def (vdl_res.dp_sig,"'0'")
                        ];
                    s_block = block;
                }];
            for ai = a1 to (a1+len) 
            do
              let ot = at.at_objs.(ai) in
              let expr_dt = get_some (expr_dt_of_ot [] ot) in
              let op1 = ud_of_ot ot [] ([UO_rhs]@(loc_flag ot)) expr_dt in
              let op2 = 
                match op2 with
                | UA_data d -> d;
                | _ -> error 0 
                        (sprintf
                        "%s: unexpected expression in argument in array <%s> statement found." 
                        sel at.at_name);
                in
              let vdl_op1 = vhdl_of_ud pro None op1 None in
              let vdl_op2 = vhdl_of_ud pro None op2 None in
              let gd_rd1,_ = ud_guard op1 in

              let this = sname () in
              incr snum;
              let next = 
                if ai < (a1+len) then sname ()
                   else sname_next label_next in
                
              let s = 
                if not gd_rd1 then 
                {
                    s_name = this;
                    s_next = Next next;
                    s_data = 
                    [
                        Data_trans (sprintf   
                                    "if %s %s %s then"
                                    vdl_op1.dp_sig
                                    (if neq then "/=" else "=")
                                    vdl_op2.dp_sig
                                   );
                        Data_trans (sprintf   
                                    "  %s <= %s and '1';"
                                    vdl_res.dp_sig 
                                    vdl_res.dp_sig 
                                   );
                        Data_trans (sprintf   
                                    "else"
                                   );
                        Data_trans (sprintf   
                                    "  %s <= '0';"
                                    vdl_res.dp_sig
                                   );
                        Data_trans (sprintf   
                                    "end if;"
                                   );
                        Data_trans_def (vdl_res.dp_sig,"'0'");
                    ]@
                    (if not (is_val op2) then
                      [
                        Data_trans_sens (sprintf "%s"
                                                 vdl_op2.dp_sig);
                      ] 
                      else []
                    ) @ (List.map (fun s -> Data_trans_sens s) vdl_op1.dp_sen)
                      @ (List.map (fun s -> Data_trans_sens s) vdl_op2.dp_sen)
                      @ (List.map (fun s -> Data_out s) vdl_op1.dp_aux)
                      @ (List.map (fun s -> Data_def s) vdl_op1.dp_aux_def)
                      ;
                    s_block = block;
                    } 
                else
                begin
                  let gd_sig = sig_guard vdl_op1 in
                  let cp = [
                    Data_cond vdl_op1.cp_sig;
                    ]@
                    (List.map (fun s -> Data_cond_sens s) vdl_op1.cp_sen) in
                  let rec s = {
                    s_name = this;
                    s_next = Branch (cp,Next this,Next next);
                    s_data = 
                    [
                        Data_trans (sprintf "if %s = '0' then"
                                                gd_sig);
                        Data_trans_sens gd_sig;
                        Data_trans (sprintf   
                                    "  if %s %s %s then"
                                    vdl_op1.dp_sig
                                    (if neq then "/=" else "=")
                                    vdl_op2.dp_sig
                                   );
                        Data_trans (sprintf   
                                    "    %s <= %s and '1';"
                                    vdl_res.dp_sig 
                                    vdl_res.dp_sig 
                                   );
                        Data_trans (sprintf   
                                    "  else"
                                   );
                        Data_trans (sprintf   
                                    "    %s <= '0';"
                                    vdl_res.dp_sig
                                   );
                        Data_trans (sprintf   
                                    "  end if;"
                                   );
                        Data_trans_def (vdl_res.dp_sig,"'0'");
                        Data_trans "end if;"
                    ] @
                    (if not (is_val op2) then
                      [
                        Data_trans_sens (sprintf "%s"
                                                 vdl_op2.dp_sig);
                      ] 
                      else []
                    ) @ (List.map (fun s -> Data_trans_sens s) vdl_op1.dp_sen)
                      @ (List.map (fun s -> Data_trans_sens s) vdl_op2.dp_sen)
                      @ (List.map (fun s -> Data_out s) vdl_op1.dp_aux)
                      @ (List.map (fun s -> Data_def s) vdl_op1.dp_aux_def)
                      ;
                    s_block = block;
                    } in
                  s;
                end;
                in
              sl := !sl @ [s];
            done;
            !sl
          end 
          else
          begin
            let sl = ref [] in
            let sel_width = 
                    let w = const_width (V_int (Int64.of_int op1_size)) in
                    let s' = 1 lsl (w-1) in
                    if s' = op1_size then (w-1) else w in
            let expr_dt = DT_logic sel_width in
            let temp = get_tmp pro (DT_logic sel_width) in
            release_tmp pro temp;
            temp.co_rules <- !core_rules;
            let temp_ud = ud_of_ot (OT_reg temp) [] [UO_loc;UO_lhs] expr_dt in
            let init = 
              let lhs = temp_ud in
              let rhs = ud_of_ot op1_a [] [UO_rhs] expr_dt in
              let dp1,cp_wr,cp_rd = simple_assign pro lhs rhs in
              let dp2 = [
                        Data_trans (sprintf   
                                    "%s <= '1';"
                                    vdl_res.dp_sig 
                                   );
                        Data_trans_def (vdl_res.dp_sig,"'0'")
                        ] in
              let this = sname () in
              incr snum;
              let next = sname () in
              make_state this next (dp1@dp2) cp_wr cp_rd in
              
            sl := !sl @ init;
            (* op1 *)
            let ar_sel =
              let selobj = PI_obj ([],OT_reg temp) in
              let ot = OT_array at in
              let expr_dt = get_some (expr_dt_of_ot [] ot) in
              let ar_sel = ud_of_ot ot [OD_sel_obj selobj]
                                       ([UO_rhs]@(loc_flag ot)) 
                                       expr_dt in
              ar_sel in                                    

            let ar_cmp,loop =
              let op1 = ar_sel in
              let op2 = 
                match op2 with
                | UA_data d -> d;
                | _ -> error 0 
                        (sprintf
                        "%s: unexpected expression in argument in array <%s> statement found." 
                        sel at.at_name);
                in

              let vdl_op1 = vhdl_of_ud pro None op1 None in
              let vdl_op2 = vhdl_of_ud pro None op2 None in
              let gd_rd1,_ = ud_guard op1 in

              let this = sname () in
              incr snum;
              let next = sname () in
                
              let s = 
                if not gd_rd1 then
                {
                    s_name = this;
                    s_next = Next next;
                    s_data = 
                    [
                        Data_trans (sprintf   
                                    "if %s %s %s then"
                                    vdl_op1.dp_sig
                                    (if neq then "/=" else "=")
                                    vdl_op2.dp_sig
                                   );
                        Data_trans (sprintf   
                                    "  %s <= %s and '1';"
                                    vdl_res.dp_sig 
                                    vdl_res.dp_sig 
                                   );
                        Data_trans (sprintf   
                                    "else"
                                   );
                        Data_trans (sprintf   
                                    "  %s <= '0';"
                                    vdl_res.dp_sig
                                   );
                        Data_trans (sprintf   
                                    "end if;"
                                   );
                        Data_trans_def (vdl_res.dp_sig,"'0'");
                    ]@
                    (if not (is_val op2) then
                      [
                        Data_trans_sens (sprintf "%s"
                                                 vdl_op2.dp_sig);
                      ] 
                      else []
                    ) @ (List.map (fun s -> Data_trans_sens s) vdl_op1.dp_sen)
                      @ (List.map (fun s -> Data_trans_sens s) vdl_op2.dp_sen)
                      @ (List.map (fun s -> Data_out s) vdl_op1.dp_aux)
                      @ (List.map (fun s -> Data_def s) vdl_op1.dp_aux_def)
                      ;
                    s_block = block;
                } 
                else
                begin
                  let gd_sig = sig_guard vdl_op1 in
                  let cp = [
                    Data_cond vdl_op1.cp_sig;
                    ]@
                    (List.map (fun s -> Data_cond_sens s) vdl_op1.cp_sen) in

                  let rec s = {
                    s_name = this;
                    s_next = Branch(cp,Next this,Next next);
                    s_data = 
                    [
                        Data_trans (sprintf "if %s = '0' then"
                                            gd_sig);
                        Data_trans_sens gd_sig;
                        Data_trans (sprintf   
                                    "  if %s %s %s then"
                                    vdl_op1.dp_sig
                                    (if neq then "/=" else "=")
                                    vdl_op2.dp_sig
                                   );
                        Data_trans (sprintf   
                                    "    %s <= %s and '1';"
                                    vdl_res.dp_sig 
                                    vdl_res.dp_sig 
                                   );
                        Data_trans (sprintf   
                                    "  else"
                                   );
                        Data_trans (sprintf   
                                    "    %s <= '0';"
                                    vdl_res.dp_sig
                                   );
                        Data_trans (sprintf   
                                    "  end if;"
                                   );
                        Data_trans_def (vdl_res.dp_sig,"'0'");
                        Data_trans "end if;"
                    ] @
                    (if not (is_val op2) then
                      [
                        Data_trans_sens (sprintf "%s"
                                                 vdl_op2.dp_sig);
                      ] 
                      else []
                    ) @ (List.map (fun s -> Data_trans_sens s) vdl_op1.dp_sen)
                      @ (List.map (fun s -> Data_trans_sens s) vdl_op2.dp_sen)
                      @ (List.map (fun s -> Data_out s) vdl_op1.dp_aux)
                      @ (List.map (fun s -> Data_def s) vdl_op1.dp_aux_def)
                      ;
                    s_block = block;
                    } in
                  s;
                end; 
                in
              [s],this in

            sl := !sl @ ar_cmp;

            let count_and_cmp =
              let this = sname () in
              incr snum;
              let next = sname () in
              let lhs = temp_ud in
              let rhs1 = ud_fix_rhs lhs in
              let rhs2 = ud_of_ot (OT_value (V_int Int64.one)) [] [UO_rhs] expr_dt in
              let dp1,_,cp_rd1 = expr_assign pro [OP_add] lhs rhs1 rhs2 in
              let rhs1 = ud_fix_rhs lhs in
              let rhs2 = ud_of_ot op1_b [] [UO_rhs] expr_dt in
              let dp2,_,cp_rd2 = branch_expr pro [OP_eq] rhs1 rhs2 in
              branch_state this (sname_next label_next)
                                loop (dp1@dp2) 
                                (cp_rd1@cp_rd2) in
              
            sl := !sl @ count_and_cmp;
            !sl;
          end;
        | UA_data (UC_val {uv_type=_;uv_val=V_string s}) ->
          cmp_vector (V_string s);
        | UA_data (UC_val {uv_type=_;uv_val=V_list v}) ->
          cmp_vector (V_list v);
        | UA_data (UC_array uat') ->
        begin
          (*
          ** Read guarded objects must be transferred into temporary
          ** registers first!
          *)
          error 0 "Relational operation with two arrays not implemented!";
        end;
        | _ -> error 0 (sprintf "%s: unexpected value in array <%s> statement found."
                        sel at.at_name);
      end;
      | _ -> error 729381 "";
     end;
     | OT_object ao ->
     begin
      match ao.ao_obj with
      | Some (OT_queue qu) ->
      begin
        match sel with
        | "unlock" ->
            let cp = [
                Data_cond (sprintf "QUEUE_%s_GD = '1'" ao.ao_name);
                Data_cond_sens (sprintf "QUEUE_%s_GD" ao.ao_name);
              ] in
            let s = {
                    s_name = label;
                    s_next = Branch (cp,Next label,Next label_next);
                    s_data = [
                        Data_out (sprintf   
                                    "QUEUE_%s_unlock <= '1';"
                                    ao.ao_name 
                                   );
                        Data_def (sprintf "QUEUE_%s_unlock"
                                      ao.ao_name,"'0'");
                    ];
                    s_block = block;
                    } in
            [s];
        | "full" | "empty" ->
        begin
          let is_terminated str =
            (*
            ** dp_conv: either expression or terminated statement;
            *)
            str.[(String.length str)-1] = ';' in
          let dst = List.nth args 0 in
          match dst with
          | UA_data (UC_reg uo) -> 
            let _,gd_wr = ud_guard (UC_reg uo) in 
            if not gd_wr then
            begin
              let vdl = vhdl_of_ud pro None (UC_reg uo) None in
              let cp = [
                  Data_cond (sprintf "QUEUE_%s_GD = '1'" ao.ao_name);
                  Data_cond_sens (sprintf "QUEUE_%s_GD" ao.ao_name);
                ] in
              let rec s = {
                s_name = label;
                s_next = Branch (cp,Next label,Next label_next);
                s_data = 
                    [
                        Data_out (sprintf   
                                    "QUEUE_%s_%s_RE <= QUEUE_%s_GD;"
                                    ao.ao_name (String.uppercase sel) 
                                    ao.ao_name
                                   );
                        Data_def (sprintf "QUEUE_%s_%s_RE"
                                      ao.ao_name
                                      (String.uppercase sel),"'0'");
                    ]@(
                        if vdl.cp_sig <> "" then
                         [
                            Data_cond vdl.cp_sig;
                         ] @
                         (List.map (fun s -> Data_cond_sens s) vdl.cp_sen)
                        else
                         []
                      )@
                    [
                       if not (is_terminated vdl.dp_conv) then
                          Data_trans (sprintf "%s <= %s;" 
                                           vdl.dp_sig
                                           (sprintf (Obj.magic vdl.dp_conv) 
                                                    (sprintf "QUEUE_%s_%s_RD" ao.ao_name (String.uppercase sel))))
                       else
                          Data_trans (sprintf (Obj.magic vdl.dp_conv) 
                                              (sprintf "QUEUE_%s_%s_RD" ao.ao_name (String.uppercase sel)));
                    ] @
                    (List.map (fun v -> Data_out v) vdl.dp_aux) @
                    (List.map (fun v -> Data_def v) vdl.dp_aux_def) @
                    (List.map (fun v -> Data_sens v) vdl.dp_aux_sen) @
                    (List.map (fun v -> Data_trans_sens v) vdl.dp_sen) @
                    (List.map (fun sv -> Data_trans_def sv) vdl.dp_def) @
                    (List.map (fun v -> Data_top v) vdl.top_expr) @
                    (List.map (fun v -> Data_top_def v) vdl.top_def);                 
                  s_block = block;
                 } in
                    
              [s]
            end
            else
             []
          | _ -> error 0 (sprintf "%s.%s: unexpected argument #1 object found." ao.ao_name sel);
        end;
        | _ -> error 0 "Unknown method used with queue object.";
      end;
      | Some (OT_channel ch) ->
      begin
        match sel with
        | "unlock" ->
            let cp = [
                Data_cond (sprintf "CHANNEL_%s_GD = '1'" ao.ao_name);
                Data_cond_sens (sprintf "CHANNEL_%s_GD" ao.ao_name);
              ] in
            let s = {
                    s_name = label;
                    s_next = Branch (cp,Next label,Next label_next);
                    s_data = [
                        Data_out (sprintf   
                                    "CHAN_%s_unlock <= '1';"
                                    ao.ao_name 
                                   );
                        Data_def (sprintf "CHAN_%s_unlock"
                                      ao.ao_name,"'0'");
                        Data_sens (sprintf "CHANNEL_%s_GD" ao.ao_name);
                    ];
                    s_block = block;
                    } in
            [s];
        | _ -> error 0 "Unknown method used with channel object.";
      end;
      | _ -> error 0 "Unknown abstract object type found.";
     end;
     | OT_array at -> 
     begin
      let sel_size = proda at.at_dim in
      let sel_width = 
          let w = const_width (V_int (Int64.of_int sel_size)) in
          let s' = 1 lsl (w-1) in
          if s' = sel_size then (w-1) else w in
      let is_static = (List.mem AT_static at.at_flags) or not
                      (List.mem AT_dyn at.at_flags) in
      let is_dyn = List.mem AT_dyn at.at_flags in
      let os = 
          match at.at_objs.(0) with
          | OT_queue qu -> "QUEUE"
          | OT_channel ch -> "CHANNEL"
          | _ -> error 0 (sprintf "Unexpected abstract object in array <%s> found." at.at_name) in
      let sel_dp =
        (*
        ** Dynamic accessed object array?
        ** Additional selector is required.
        *)
        if is_sel opl && is_dyn then
        begin
          let s = sprintf "%s_%s_SEL" os at.at_name in
          [
            Data_out (sprintf "%s <= %s;" s 
              (val_str (DT_logic sel_width) 
                       (V_int (Int64.of_int (proda (obj_sel opl))))));
            Data_def (s,(val_str (DT_logic sel_width) 
                          (V_int Int64.zero)))
          ]
        end
        else if is_sel_obj opl then
        begin
          let s = sprintf "%s_%s_SEL" os at.at_name in
          let sel_ot = 
            match obj_sel_obj opl with
            | PI_obj (opl',ot) -> ot;
            | _ -> error 118989 ""; in
          let sel_flags =   
            [
              UO_rhs;
            ]@ 
            (if is_ot_local (Some pro) sel_ot then
              [
                UO_loc; (* local or temporary register ! *)
              ]
            else []) in  
          let sel_params =
            [
              OD_conv (DT_natural 0);
            ] in

          let expr_dt = DT_logic sel_width in
          let ud_sel = ud_of_ot sel_ot sel_params sel_flags expr_dt in

          let vdr = vhdl_of_ud pro None ud_sel None in
          let sel_dp = [
                         Data_out (sprintf "%s <= %s;"
                                           s
                                           vdr.dp_sig);
                         Data_def (s,"0");

               ] @ 
               (List.map (fun str -> Data_sens str) vdr.dp_sen) @
               (List.map (fun s -> Data_top s) vdr.top_expr)@
               (List.map (fun s -> Data_top_def s) vdr.top_def) in
          sel_dp
        end
        else [] in
      (*
      ** Array selector.
      *)
      if is_static && is_sel opl then
      begin
        let n = at_index at (obj_sel opl) in
        let ot' =  at.at_objs.(n) in
        let ao' =
          match ot' with
          | OT_queue qu -> qu.qu_ao
          | OT_channel ch -> ch.ch_ao 
          | _ -> error 0 (sprintf "Unexpected abstract object in array <%s> found." at.at_name) in
        let opl' = List.filter (fun op -> 
                      match op with OD_sel _ -> false| _ -> true) opl in
        scode (Fun ((opl',OT_object ao'),sel,args));
      end
      else if not is_static then
      begin
        (*
        ** Static->Dynamic selection mode can change anytime,
        ** update read and write dependencies for the object
        *)
        let pro_unlock = ref [] in
        Array.iter (fun ot' ->
          let ao' =
            match ot' with
            | OT_queue qu -> qu.qu_ao
            | OT_channel ch -> ch.ch_ao
            | _ -> error 0 (sprintf "Unexpected abstract object in array <%s> found." at.at_name) in    
          List.iter (fun (sel',pro') -> 
            if not (List.mem (sel',pro') !pro_unlock) && pro = pro' 
              then pro_unlock := !pro_unlock @ [sel,pro]) ao'.ao_procs;
            ) at.at_objs;
        let ot' =  at.at_objs.(0) in
        let ao' =
          match ot' with
          | OT_queue qu -> {qu.qu_ao with ao_name = at.at_name; ao_procs = !pro_unlock; ao_array=[]}
          | OT_channel ch -> {ch.ch_ao with ao_name = at.at_name; ao_array=[]}
          | _ -> error 0 (sprintf "Unexpected abstract object in array <%s> found." at.at_name) in
        let opl' = List.filter (fun op -> 
                      match op with OD_sel _ -> false| _ -> true) opl in
        match scode (Fun ((opl',OT_object ao'),sel,args)) with
        | [s] -> [{s with s_data=s.s_data@sel_dp}]
        | _ -> progerr "scode:[s]";        
      end
      else
        error 0 (sprintf "Unexpected dynamic selector of abstract object array <%s> found." at.at_name);      
     end;  
     | _ -> error 496553 "";
    end;                
    | _ -> error 557114 "" in
    scode instr

(*
** Precompile and check function (method) call
*)
let fun_compile modu pro instr top =
 let temps = ref [] in
 let tmp_of_size pro size =
   let width = 
     let w = const_width (V_int (Int64.of_int size)) in
     let s' = 1 lsl (w-1) in
     if s' = size then (w-1) else w in
   let temp = new_tmp pro (DT_logic width) in
   temp.co_rules <- !core_rules;
   temps := !temps @ [temp];
   debug "fun_compile" with (sprintf "Core: created temporary register [%d]." width);
   in

 let tmp_of_width pro width =
   let temp = new_tmp pro (DT_logic width) in
   temp.co_rules <- !core_rules;
   temps := !temps @ [temp];
   debug "fun_compile"  with (sprintf "Core: created temporary register [%d]." width);
   in

 let release_temps pro =
   List.iter (fun temp ->  
        release_tmp pro temp;
     ) !temps in
   
 match pro with
 | Some pro ->
 begin
  (*
  ** Create temporary registers required for method implementation!
  ** fun_compile: new_tmp
  ** fun_scode: get_tmp
  *)
  match instr with
  | PI_fun(src,(opl,OT_object ao),sel,args) ->
  begin
    match sel with
    | "cmp_eq"
    | "cmp_neq"
    | "cmpn_eq"
    | "cmpn_neq"
    | "set"
    | "copy"
    | "copyn"
    | "init" ->
    begin
      match ao.ao_array with
      | at  :: _ ->
        (*
        ** Set dynamic selector flag
        *)
        array_change_dyn at;
        let sel_size = proda at.at_dim in
        tmp_of_size pro sel_size;

        if sel = "copy" then
        begin
          match args with
          | [PI_obj (opl,ot)] -> 
          begin
            match ot with
            | OT_array at' -> 
              let src_len = proda at'.at_dim in
              let dst_len = proda at.at_dim in
              tmp_of_size pro (max src_len dst_len);                
            | _ -> ();
          end;
          | _ -> error 0 (sprintf "%s: unexpected arguments." sel);
        end
        else if sel = "cmp_eq" || sel = "cmp_neq" then
        begin
          match args with
          | [_;PI_obj (opl,ot)] -> 
          begin
            match ot with
            | OT_array at' -> 
              let src_len = proda at'.at_dim in
              let dst_len = proda at.at_dim in
              tmp_of_size pro (max src_len dst_len);                
            | _ -> ();
          end;
          | _ -> error 0 (sprintf "%s: unexpected arguments." sel);
        end
        else if sel = "copyn" then
        begin
          match args with
          | [_;_;_;PI_obj (opl,ot)] -> 
          begin
            match ot with
            | OT_value (V_string s) -> ();
              let src_len = String.length s in
              let dst_len = proda at.at_dim in
              tmp_of_size pro (max src_len dst_len);  
            | OT_array at' -> 
              let src_len = proda at'.at_dim in
              let dst_len = proda at.at_dim in
              tmp_of_size pro (max src_len dst_len);  
              if List.mem AT_block at'.at_flags then
              begin
                let width = 
                    match dt_of_ot at'.at_objs.(0) with
                    | Some dt -> size_of_dt dt;
                    | None -> 572033 in
                tmp_of_width pro width;
              end;
            | _ -> ();
          end;
          | _ -> error 0 (sprintf "%s: unexpected arguments." sel);
        end
        else if sel = "cmpn_eq" || sel= "cmpn_neq" then
        begin
          match args with
          | [_;_;_;_;PI_obj (opl,ot)] -> 
          begin
            match ot with
            | OT_value (V_string s) -> ();
              let src_len = String.length s in
              let dst_len = proda at.at_dim in
              tmp_of_size pro (max src_len dst_len);  
            | OT_array at' -> 
              let src_len = proda at'.at_dim in
              let dst_len = proda at.at_dim in
              tmp_of_size pro (max src_len dst_len);  
              if List.mem AT_block at'.at_flags then
              begin
                let width = 
                    match dt_of_ot at'.at_objs.(0) with
                    | Some dt -> size_of_dt dt;
                    | None -> 572033 in
                tmp_of_width pro width;
              end;
            | _ -> ();
          end;
          | _ -> error 0 (sprintf "%s: unexpected arguments." sel);
        end;
        release_temps pro;
      | [] -> error 0 (sprintf "Core method <%s> used with non Core object <%s>!"
                                 sel ao.ao_name);
    end;
    | "unlock" ->
    begin
      match ao.ao_obj with
      | Some (OT_queue qu) -> ();
      | _ -> error 0 (sprintf "\nUnlock method (Core) applied to unsupported object <%s>."
                              ao.ao_name);
    end;
    | "empty"  | "full" ->
    begin
      let ot = fun_get_arg_ot "Core" sel args 1  in
      
      (match ao.ao_obj with
      | Some (OT_queue _) -> ();
      | Some (OT_channel _) -> ();
      | _ -> error 0 (sprintf "<%s> method (Core) applied to unsupported object <%s>."
                              sel ao.ao_name);
      );
      (match dt_of_ot ot with
      | Some DT_bool -> ();
      | _ -> error 0 (sprintf "<%s> method (Core) applied to unsupported argument type (1) <%s>."
                              sel ao.ao_name);
      );
    end;
    | "guard" ->
    begin
      match ao.ao_array with
      | at :: _ -> 
	    let is_block = List.mem AT_block at.at_flags in
		if not is_block then
		begin
          let ot = fun_get_arg_ot "Core" sel args 1  in
		  let ot_name = name_of_ot ot in
          ao.ao_flags <- ao.ao_flags @ ["guard",ot_name];
          info (sprintf "Added object guard <%s> to array <%s>." ot_name ao.ao_name);
		end
		else
		begin
          let ot = fun_get_arg_ot "Core" sel args 1  in
		  let ot_name = name_of_ot ot in
		  match co_of_ot at.at_objs.(0) with
		  | Some co -> 
		  begin
		  	let db = 
			  match co.co_block with
			  | Some db -> db;
			  | None -> error 212531 "" in
			  
        	ao.ao_flags <- ao.ao_flags @ ["guard",ot_name];
         	db.db_flags <- db.db_flags @ ["guard",ot_name];
       	    info (sprintf "Added object guard <%s> to RAM block <%s> for array <%s>." ot_name db.db_name ao.ao_name);
		  end;
		  | None -> error 340720 "";		
		end;
      | [] -> error 0 (sprintf "\nGuard method (Core) applied to unsupported object <%s>."
                                 ao.ao_name);
    end;
    | _ -> error 0 (sprintf "\nunknown Core method <%s>"
                            sel);
  end;
  | _ -> ();
 end;
 | _ -> error 0 "\nunexpected Core method outside process context"
 

let bf_time modu proo f =
    FT_min 1

let rec rules = {
    rl_name = "Core";
    rl_my = my;
    rl_obj_port = obj_port;
    rl_obj_map = obj_map;
    rl_obj_decl = obj_decl;
    rl_obj_code = obj_code;
    rl_instr_ucode = instr_ucode;
    rl_types = [];
    rl_methods = [
        (*
        ** Array methods
        *)
        (*
        ** init <value> 
        *)
        "init",[new_arg_desc Arg_rhs];
        (*
        ** set <index_a> <index_b> <value>
        *)
        "set",[new_arg_desc Arg_rhs;
               new_arg_desc Arg_rhs;
               new_arg_desc Arg_rhs];
        (*
        ** copy <vector>
        *)
        "copy",[new_arg_desc Arg_rhs];
        (*
        ** copyn <dst_index_a> <src_index_a> <len> <vector>
        *)
        "copyn",[new_arg_desc Arg_rhs;
                 new_arg_desc Arg_rhs;
                 new_arg_desc Arg_rhs;
                 new_arg_desc Arg_rhs];
        (*
        ** compare
        ** res_bool <- ADTO.OP <op2>
        *)
        "cmp_eq",[new_arg_desc Arg_lhs;
                  new_arg_desc Arg_rhs];
        "cmp_neq",[new_arg_desc Arg_lhs;
                   new_arg_desc Arg_rhs];
        (*
        ** compare only n elements
        ** res_bool <- ADTO.OP <op1_index_a> <op2_index_a> <len> <op2> 
        *)
        "cmpn_eq",[new_arg_desc Arg_lhs;
                   new_arg_desc Arg_rhs;
                   new_arg_desc Arg_rhs;
                   new_arg_desc Arg_rhs;
                   new_arg_desc Arg_rhs];
        "cmpn_neq",[new_arg_desc Arg_lhs;
                    new_arg_desc Arg_rhs; 
                    new_arg_desc Arg_rhs;
                    new_arg_desc Arg_rhs;
                    new_arg_desc Arg_rhs];
        (*
        ** Required for blocking objects to release locks.
        *)
        "unlock",[];
        (*
        ** Attach guard object to Core object (adds an indirect objetc lock)
        *)
        "guard",[new_arg_desc Arg_rhs];
        
        (*
        ** Return atatus of objects
        *)
        "empty",[new_arg_desc Arg_lhs];
        "full",[new_arg_desc Arg_lhs];
        ];
    rl_fun_compile = fun_compile;
    rl_fun_scode = fun_scode;
    rl_top_vcode = emit_top_code;
    rl_time = bf_time;
    rl_new_obj = (fun _ _ _ ->  rules);
    rl_interp = (fun _ -> "");
    rl_child=None;
}


let init () =
    out "Init: Core.";
    self := Some rules;
    core_rules := !self;
    sym_add top_syms (Sym_rule rules)
