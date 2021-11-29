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
**    $CREATED:     09.03.2008
**    $VERSION:     2.05
**
**    $INFO:
**
**  Graphical represenation of (hierarchical) structures using graphs.
**    Output format: dot
**    Final postscript transformation: dot -Tps xxx.dot -o xxx.ps
**
**    $ENDOFINFO
**
*)
open Cp_syntax
open Cp_types
open Cp_utils
open Cp_expr
open Cp_common
open Cp_block_frame
open Cp_printtypes
open Cp_ucode
open Cp_symbol 
open Cp_data 
open Cp_stat

open Printf

let out_stdout = ref true 
let info msg = Cp_common.info msg 
let out msg = if !out_stdout then Cp_common.out msg else Cp_common.vhdl msg
let ind_incr () = if !out_stdout then Cp_common.ind_incr () else Cp_common.vhdl_incr ()
let ind_decr () = if !out_stdout then Cp_common.ind_decr () else Cp_common.vhdl_decr ()

(*
** Process blockframe graph
*)

let blockframe_graph modu pro =
  let fname = sprintf "%s_%s_ft" (of_mod modu.mod_name) pro.pro_name in
  let link_ui = List.mem "ftu" compiler.t_graph in
  let uil = pro.pro_ucode in
  
  let root = 
    match pro.pro_instr with
      | (PI_block (_,bf))::tl -> bf;
      | _ -> error 0 "Can't find root frame" in
  let rec sprint_time ft =
    let get_time = sprint_time in
    match ft with
    | FT_0 -> 
      "0"
    | FT_list fl ->
      let str = ref "(" in
      let n = ref (List.length fl) in
      List.iter (fun ft' -> decr n;
                            str := !str ^ (sprintf "%s%s" 
                                          (get_time ft')
                                          (if !n > 0 then "+" else""))) fl;
      !str^")"
    | FT_n (ft_n,ft') ->
      sprintf "(%s*%s)" (get_time ft_n) (get_time ft');
    | FT_max n ->
      sprintf "<=%d" n;
    | FT_min n ->
      sprintf ">=%d" n;
    | FT_minmax (ft1,ft2) ->
      sprintf "(%s..%s)" (get_time ft1) (get_time ft2);
    | FT n ->
      sprintf "%d" n in

  let node_name bf =
    sprintf "%s_%d" bf.bf_name bf.bf_id in
  let make_struct bf =
    let str = 
      sprintf "%s [shape=record,fontname=\"Courier-Bold\",style=bold,label=\"{ NAME: %s | { ID: %d | TYPE: %s } | %s{ TIME: %s TU | BOUND: %b } %s}\"];" 
              (node_name bf)
              bf.bf_name
              bf.bf_id
              (
                 match bf.bf_type with
                 | BF_data -> "DATA";
                 | BF_branch -> "BRANCH";
                 | BF_compound -> "COMPOUND";
                 | BF_empty -> "EMPTY";
                 | BF_loop -> "LOOP";
                 | BF_conditional -> "CONDITIONAL";
              )
              (
                 if bf.bf_src_start.s_line <> bf.bf_src_end.s_line then
                   sprintf "{ \\ SOURCE: %s lines %d..%d \\ } |"
                              bf.bf_src_start.s_file
                              bf.bf_src_start.s_line
                              bf.bf_src_end.s_line
                 else if bf.bf_src_start.s_line > 0 then
                   sprintf "{ \\ SOURCE: %s line %d \\ } |"
                              bf.bf_src_start.s_file
                              bf.bf_src_start.s_line
                 else ""
              )
              (
                sprint_time (compact_time bf.bf_time)
              )
              (
                List.mem BP_bind bf.bf_params
              )
              (
                if link_ui then
                begin
                  let str = ref "" in
                  List.iter (fun ui ->
                    if bf = ui.ui_frame then
                      if !str <> "" then
                        str := sprintf "%s\\l\\ %s" 
                                       !str
                                       (ui_sprint_uc ui)
                      else
                        str := sprintf "\\ UCODE:\\l\\ %s" (ui_sprint_uc ui);

                    ) uil;
                  if !str <> "" then sprintf "|{%s\\l}" !str else !str
                end
                else
                  ""
              )
              in
    let str' = Str.global_replace (Str.regexp ">") "\>" str in
    let str'' = Str.global_replace (Str.regexp "<") "\<" str' in 
    str'' in
    
  out (sprintf "Emitting block frame graph for process <%s> in file <%s.dot>..." pro.pro_name fname);
  let ok = protects(vhdl_oc := open_out (sprintf "%s%s.dot" compiler.t_output fname)) in
  if not ok then error 0 (sprintf "Can't open graph file <%s.dot>!" fname);
  out_stdout := false;
  out "digraph structs {";
  ind_incr ();
  out "node [shape=records];";
  
  let rec emit bf =    
    let childs = List.map emit bf.bf_childs in
    List.iter (fun bf' ->
        out (sprintf "%s -> %s;" (node_name bf) (node_name bf'));
        out (make_struct bf');
      ) childs;
    bf in
  let bf = emit root in
  out (make_struct bf);
  ind_decr ();
  out "}";
  close_out !vhdl_oc;
  out_stdout := true;
  ind_incr ();
  let com = sprintf "dot -Tps -o %s%s.eps %s%s.dot;epsffit -c -m 20 20 575 822 %s%s.eps %s%s.ps"
                    compiler.t_output fname
                    compiler.t_output fname
                    compiler.t_output fname
                    compiler.t_output fname
                     in 
  out (sprintf "Creating postscript file <%s.ps> with command <%s>..."
                fname com);

  let exec cmd =
    let stat = ref (Unix.WEXITED 0) in
    let ok = protects(stat := Unix.system cmd) in
    ok && (!stat = (Unix.WEXITED 0)) in

       
  let ok = exec com in
  if not ok then
    error 0 "Failed to produce postscript from dot graph.";
  ind_decr ()

(*
** Process state graphs
*)

let state_graph modu pro =
  let fname = sprintf "%s_%s_st" (of_mod modu.mod_name) pro.pro_name in

  let fix str = Str.global_replace (Str.regexp "\"") "\\\"" str in

  let rec sprint_sd sdl =
    match sdl with
    | sd :: tl ->
    begin
      let str = 
        match sd with
        | Data_in str -> sprintf "IN: %s" (fix str)
        | Data_out str -> sprintf "OUT: %s" (fix str)
        | Data_trans str -> sprintf "TRANS: %s" (fix str)
        | Data_cond str -> sprintf "COND: %s" (fix str)
        | _ -> "" in
      if tl = [] then str^"\\l"
      else if str <> "" then
        "\\l "^str^(sprint_sd tl)
      else
        sprint_sd tl
    end;
    | [] -> "" in

  let node_name st =
    sprintf "%s" st.s_name in

  let make_struct flags st =
    let str = 
      sprintf "%s [shape=record,fontname=\"Courier-Bold\",label=\"{ NAME: %s | {\\ DATA:%s}} %s\"];" 
              (node_name st)
              st.s_name
              (sprint_sd st.s_data)
              (if flags <> "" then ","^flags else "")
              in
    let str' = Str.global_replace (Str.regexp ">") "\>" str in
    let str'' = Str.global_replace (Str.regexp "<") "\<" str' in 
    str'' in

  out (sprintf "Emitting state graph for process <%s> in file <%s.dot>..." pro.pro_name fname);
  let ok = protects(vhdl_oc := open_out (sprintf "%s%s.dot" compiler.t_output fname)) in
  if not ok then error 0 (sprintf "Can't open graph file <%s.dot>!" fname);
  out_stdout := false;
  out "digraph structs {";
  ind_incr ();
  out "node [shape=records];";
  
  let rec next s ns label =
    match ns with
    | Next node' -> 
      out (sprintf "%s -> %s%s;" 
                   s
                   node'
                   (Str.global_replace (Str.regexp "\\\\l") "" label));
    | Branch (sdl,n1,n2) ->
      next s n1 (sprintf "[label=\"%s\"]" (sprint_sd sdl));
      next s n2 (sprintf "[label=\"NOT(%s)\"]" (sprint_sd sdl));  
    | Select (sdl,csl) ->
      List.iter (fun cs -> next s cs.cs_next (sprintf "[label=\"%s\"]" (sprint_sd sdl))) csl;   
    | Next_instr -> () in
    
  let rec emit stbl = 
    match stbl with
    | (State st) :: tl ->    
      out (make_struct "" st);
      next (node_name st) st.s_next "";
      emit tl;
    | (State_block stbl) :: tl ->
      emit stbl;
      emit tl;
    | (State_top _) :: tl -> 
      emit tl;
    | [] -> () in
     
  emit pro.pro_states;
  ind_decr ();
  out "}";
  close_out !vhdl_oc;
  out_stdout := true;
  ind_incr ();
  let com = sprintf "dot -Tps -o %s%s.eps %s%s.dot;epsffit -c -m 20 20 575 822 %s%s.eps %s%s.ps"
                    compiler.t_output fname
                    compiler.t_output fname
                    compiler.t_output fname
                    compiler.t_output fname
                     in 
  out (sprintf "Creating postscript file <%s.ps> with command <%s>..."
                fname com);

  let exec cmd =
    let stat = ref (Unix.WEXITED 0) in
    let ok = protects(stat := Unix.system cmd) in
    ok && (!stat = (Unix.WEXITED 0)) in

       
  let ok = exec com in
  if not ok then
    error 0 "Failed to produce postscript from dot graph.";
  ind_decr ()


(*
**
*******************************************
**  Process Call Graph 
**  Interprocess call and exception link graph
*******************************************
**
*)


(*
** Link all processes
*)
let call_graph_build modu =
  (*
  ** All known exceptions
  *)
  let txl = ref [] in
  List.iter (fun sym ->
    match sym with
    | Sym_type tp ->
    begin
      match tp with
      | Type_exc tx -> txl := !txl @ [tx];
      | _ -> ()
    end;
    | _ -> ()
    ) (list_of_sym modu.mod_objs);
  let find_tx ex =
    match List.filter (fun tx -> tx.tx_id = ex) !txl with
    | [tx] -> tx;
    | _ -> error 0 (sprintf "Unknown exception #<%d> found." ex) in 
  let find_tn en =
    match List.filter (fun tx -> tx.tx_name = en) !txl with
    | [tx] -> tx;
    | _ -> error 0 (sprintf "Unknown exception <%s> found." en) in 
  (*
  ** Collect start/stop/end/raise/trywith instructions
  *)
  let rec iter pro_control pi =
    match pi with
    | PI_raise ex ->
      let tx = find_tx ex in
      if not (List.mem tx pro_control.pro_raise) then
        pro_control.pro_raise <- pro_control.pro_raise @ [tx]; 
    | PI_fun (src,(opl,ot),sel,args) ->
    begin
      match ot with
      | OT_object ao ->
      begin
        let is_pro = List.filter (fun pro -> pro.pro_name = ao.ao_name) modu.mod_procs in
        match is_pro,sel with
        | [pro'],"start" -> 
          if not (List.mem pro' pro_control.pro_start) then
            pro_control.pro_start <- pro_control.pro_start @ [pro'];
        | [pro'],"stop" -> 
          if not (List.mem pro' pro_control.pro_stop) then
            pro_control.pro_stop <- pro_control.pro_stop @ [pro'];
        | [pro'],"call" -> 
          if not (List.mem pro' pro_control.pro_call) then
            pro_control.pro_call <- pro_control.pro_call @ [pro'];
        | _ -> ()
      end;
      | _ -> ();
    end;
    | PI_try (block,csb) ->
    begin
      let bf =
        match block with
        | PI_block (_,bf) -> bf;
        | _ -> nilbf in
      let ct = {
        catch_env = bf;
        catch_exl = [];
        catch_ctl = { 
          pro_start=[];
          pro_stop=[];
          pro_call=[];
          pro_raise=[];
          pro_catch=[];
          }
        } in
      pro_control.pro_catch <- pro_control.pro_catch @ [ct];
      iter ct.catch_ctl block;
      (*
      ** Transfer start/stop/call from catch frame to actual process
      *)
      List.iter (fun pro' ->
          if not (List.mem pro' pro_control.pro_start) then
            pro_control.pro_start <- pro_control.pro_start @ [pro'];
          ) ct.catch_ctl.pro_start;
      List.iter (fun pro' ->
          if not (List.mem pro' pro_control.pro_stop) then
            pro_control.pro_stop <- pro_control.pro_stop @ [pro'];
          ) ct.catch_ctl.pro_stop;
      List.iter (fun pro' ->
          if not (List.mem pro' pro_control.pro_call) then
            pro_control.pro_call <- pro_control.pro_call @ [pro'];
          ) ct.catch_ctl.pro_call;
          
      List.iter (fun ex ->
        if not (List.mem ex pro_control.pro_raise) then
          pro_control.pro_raise <- pro_control.pro_raise @ [ex];
        ) ct.catch_ctl.pro_raise;
      match csb with
      | PI_block (l,_) 
      | PI_list l ->
        List.iter (fun pi ->
          match pi with
          | PI_block ([PI_case (src,exprl,_)],_) ->
            List.iter (fun pi ->
              match pi with
              | PI_obj (_,ot) ->
                let ex = 
                  match ot with
                  | OT_value (V_int ex64) -> Int64.to_int ex64;
                  | _ -> error 0 "Invalid exception found"  in
                let tx = find_tx ex in
                if not (List.mem tx ct.catch_exl) then
                  ct.catch_exl <- ct.catch_exl @ [tx]; 
              | _ -> error 0 "Invalid exception in try-with found." 
              ) exprl;
          | _ -> out_ (pi_sprint_instr pi); error 0 "Invalid PI_case in PI_try";
          ) l; 
      | _ -> error 0 "Invalid PI_try"
    end;
    | PI_block (pil,_)
    | PI_list pil -> List.iter (iter pro_control) pil;
    | PI_branch (src,_,b1,b2) ->
      iter pro_control b1;
      iter pro_control b2;
    | PI_forloop (src,_,_,_,_,b) ->
      iter pro_control b;
    | PI_loop (src,_,_,b) ->
      iter pro_control b;
    | PI_waitfor (src,_,_,_,b1,b2) ->
      iter pro_control b1;
      iter pro_control b2;      
    | PI_select (src,_,csb) ->
      iter pro_control csb;
    | PI_case (src,_,b) ->
      iter pro_control b;
    | _ -> () in
    
    
  List.iter (fun pro ->
    List.iter (fun pi ->
        iter pro.pro_control pi;
      ) pro.pro_instr;
    call_log pro;
    ) modu.mod_procs;

  
  let import_reg pro co =
      if not (sym_check_pro pro.pro_import co.co_name) then
      begin
          (*
          ** Must be imported...
          *)
          if not (sym_check_obj modu.mod_objs co.co_name) &&
             not (sym_check_obj_ext modu co.co_name) then
              error 0 (sprintf "Unknown object <%s> in process <%s> from module <%s>."
                           co.co_name pro.pro_name modu.mod_name); 
          if not (sym_check_obj pro.pro_import co.co_name) then
              sym_add pro.pro_import (Sym_obj (OT_reg co));
      end in
  (*
  ** Update PRO_%s_EXCEPTION register dependencies read/write/import
  *)
  List.iter (fun pro ->
    let exn = sprintf "PRO_%s_EXCEPTION" pro.pro_name in
    let r_ex = 
      match sym_get_obj modu.mod_objs exn with
      | OT_reg co -> co
      | _ -> error 0 (sprintf "register <%s> not found." exn) in
      
    let wr = ref (pro.pro_control.pro_raise <> []) in
    let rd = ref (pro.pro_control.pro_catch <> []) in
    
    let rec raised proc prol =
      match prol with
      | pro' :: tl ->
        let exn' = sprintf "PRO_%s_EXCEPTION" pro'.pro_name in
        let r_ex' = 
          match sym_get_obj modu.mod_objs exn' with
          | OT_reg co -> co
          | _ -> error 0 (sprintf "register <%s> not found." exn') in
        if pro'.pro_control.pro_raise <> []  &&  not (List.mem proc r_ex'.co_reader) then
          r_ex'.co_reader <- r_ex'.co_reader @ [proc];
        let stat = raised pro' pro'.pro_control.pro_call in
        (pro'.pro_control.pro_raise <> []) || stat ||
        (raised pro tl)
      | [] -> false in
      
    if raised pro pro.pro_control.pro_call then
      wr := true;
    
    r_ex.co_writer <- List.filter (fun pro' -> pro<>pro') r_ex.co_writer;    
    r_ex.co_reader <- List.filter (fun pro' -> pro<>pro') r_ex.co_reader;    

    if !wr then
      r_ex.co_writer <- r_ex.co_writer @ [pro];
    if !rd then
      r_ex.co_reader <- r_ex.co_reader @ [pro];
    if r_ex.co_reader <> [] || r_ex.co_writer <> [] then
    begin
      import_reg pro r_ex; 
      List.iter (fun pro' ->
        let exn' = sprintf "PRO_%s_EXCEPTION" pro'.pro_name in
        let r_ex' = 
          match sym_get_obj modu.mod_objs exn' with
          | OT_reg co -> co
          | _ -> error 0 (sprintf "register <%s> not found." exn') in
        import_reg pro r_ex';        
        ) pro.pro_control.pro_call;   
    end;   
    ) modu.mod_procs;
  List.iter (fun pro ->
    let exn = sprintf "PRO_%s_EXCEPTION" pro.pro_name in
    let r_ex = 
      match sym_get_obj modu.mod_objs exn with
      | OT_reg co -> co
      | _ -> error 0 (sprintf "register <%s> not found." exn) in
    call_exc_log r_ex;
    ) modu.mod_procs;

  call_summary()
  

    
let call_graph_print modu =
  List.iter (fun pro ->
    List.iter (fun pi ->
        ()
      ) pro.pro_instr;
    ) modu.mod_procs
  
