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
**    $CREATED:     14.5.2006
**    $VERSION:     2.10
**
**    $INFO:
**
**  Intermediate MicroCode representation derived from process instructions
**  PI_XX.
**  
**    $ENDOFINFO
**
*)
open Cp_print
open Cp_utils
open Cp_data
open Cp_types
open Cp_alu
open Printf
open Cp_symbol
open Cp_block_frame
open Cp_syntax
open Cp_common
open Cp_stat



let ui_sprint_range r =
    match r with
    | Some (a,b) -> if a <> b then sprintf "[%d to %d]" a b
                              else sprintf "[%d]" a;
    | None -> ""

let ui_sprint_index i =
    let datastr da =
          match da with
          | UC_reg uo -> uo.uo_name;
          | UC_var uo -> uo.uo_name;
          | UC_sig uo -> uo.uo_name;
          | UC_temp ut -> ut.ut_name; 
          | _ -> "?" in
    match i with
    | Some (UA_data (UC_val uv)) -> 
    begin
          match uv.uv_val with
          | V_int i64 -> sprintf ".[%d]" (Int64.to_int i64);
          | _ -> "";
    end;
    | Some (UA_data da) -> sprintf ".[%s]" (datastr da);
    | _ -> ""

let ui_sprint_dt dt =
    match dt with
    | DT_logic n -> sprintf "L%d" n;
    | DT_int n -> sprintf "I%d" n;
    | DT_char -> "C8";
    | DT_bool -> "B";
    | DT_string n -> sprintf "S%d" n;
    | DT_object on -> sprintf "O.%s" on;
    | DT_lneg -> "(-)";
    | DT_bneg -> "(-)";
    | DT_aneg -> "(-)";
    | DT_natural n -> sprintf "N%d" n

let ui_sprint_conv cl = 
  match cl with
  | Some ct -> sprintf ":%s" (ui_sprint_dt ct);
  | None -> ""
   

let ui_sprint_flags fl = 
  hlist (fun f->
          match  f with
          | UO_lhs -> "LHS";
          | UO_rhs -> "RHS";
          | UO_loc -> "LOC";
          | UO_ot ot -> sprintf "OT#%s" (name_of_ot ot);
          ) fl

let ui_sprint_type t =
  sprintf "ET(%s);DT(%s)%s" 
          (ui_sprint_dt t.uo_expr_type)        
          (ui_sprint_dt t.uo_data_type)        
          (
          if t.uo_conv <> None then
            sprintf "TC(%s)" (ui_sprint_conv t.uo_conv)
          else
            ""
          )
 
        

let ui_sprint_uo uo =
    sprintf "%s%s%s,[%s%s%s]"
                    uo.uo_name
                    (ui_sprint_range uo.uo_range)
                    (ui_sprint_index uo.uo_index)
                    (ui_sprint_flags uo.uo_flags) (if uo.uo_flags <> [] then ";" else "")
                    (ui_sprint_type uo.uo_type)

let ui_sprint_uat uat =
    sprintf "%s%s,[%s%s%s]"
                    uat.uat_name
                    (ui_sprint_range uat.uat_range)
                    (ui_sprint_flags uat.uat_flags) (if uat.uat_flags <> [] then ";" else "")
                    (ui_sprint_type uat.uat_type)

let ui_sprint_ut ut =
    sprintf "%s%s,[%s%s%s]" 
                ut.ut_name 
                (ui_sprint_range ut.ut_range)
                (ui_sprint_flags ut.ut_flags) (if ut.ut_flags <> [] then ";" else "")
                (ui_sprint_type ut.ut_type)

let rec ui_sprint_val v =
    let sprint_timeunit vunit =
        match vunit with
        | Nsec -> "Nsec";
        | Usec -> "Usec";
        | Msec -> "Msec";
        | Sec -> "Sec";
        | Cycles -> "Cycles" in

    let sprint_frequnit vunit =
        match vunit with
        | Ghz -> "Ghz";
        | Mhz -> "Mhz";
        | Khz -> "Khz";
        | Hz -> "Hz" in
    match v with
    | V_int n -> Int64.to_string n;
    | V_float f -> string_of_float f;
    | V_string s -> s;
    | V_char c -> sprintf "%c" c;
    | V_logic s -> s;
    | V_bool b -> if b then "$true" else "$false";
    | V_z -> "Z";
    | V_null -> "NULL";
    | V_freq (v,u) -> sprintf "%s %s" (Int64.to_string v) (sprint_frequnit u)  
    | V_time (v,u) -> sprintf "%s %s" (Int64.to_string v) (sprint_timeunit u)
    | V_list l -> sprintf "[%s]" (hlist ui_sprint_val l)

let ui_sprint_ul ul =
    sprintf "[%s%sET(%s);DT(%s)%s]" 
                (ui_sprint_flags ul.ul_flags) (if ul.ul_flags <> [] then ";" else "")
                (ui_sprint_dt ul.ul_type.uo_expr_type)
                (ui_sprint_dt ul.ul_type.uo_data_type)
                (
                if ul.ul_type.uo_conv <> None then
                  sprintf ";TC(%s)" (ui_sprint_conv ul.ul_type.uo_conv)
                else
                  ""
                )

let ui_sprint_ua ua =
    sprintf "ALU([%s],%s)"
                    (ui_sprint_flags ua.ua_flags)
                    (sprint_dt ua.ua_type) 

let rec ui_sprint_ud da =
  let datastr = ui_sprint_ud in
  let uo_conv uo =
    if uo.uo_type.uo_conv <> None then ui_sprint_conv uo.uo_type.uo_conv else
    if uo.uo_type.uo_expr_type <> uo.uo_type.uo_data_type then sprintf ":%s" (ui_sprint_dt uo.uo_type.uo_expr_type) else
      "" in
  let ut_conv ut =
    if ut.ut_type.uo_conv <> None then ui_sprint_conv ut.ut_type.uo_conv else
    if ut.ut_type.uo_expr_type <> ut.ut_type.uo_data_type then sprintf ":%s" (ui_sprint_dt ut.ut_type.uo_expr_type) else
      "" in
  let uo_sign uo =
    if uo.uo_type.uo_sign = (Some DT_aneg) then "-" else
    if uo.uo_type.uo_sign = (Some DT_bneg) then "not " else
    if uo.uo_type.uo_sign = (Some DT_lneg) then "not " else "" in
  let uo_sel uo =
    let str = ref "" in
    List.iter (fun sel ->
	  match sel with
	  | UC_val uv -> 
		if !str = "" then str := ui_sprint_val uv.uv_val 
                     else str := sprintf "%s,%s" !str (ui_sprint_val uv.uv_val);  
	  | _ ->
        if !str = "" then str := name_of_ud sel 
                     else str := sprintf "%s,%s" !str (name_of_ud sel);  
      ) uo.uo_sel;
    if !str <> "" then sprintf ".[%s]" !str else "" in

  match da with
  | UC_reg uo -> sprintf "%s%s%s%s" (uo_sign uo) uo.uo_name (ui_sprint_range uo.uo_range) (uo_conv uo);
  | UC_var uo -> sprintf "%s%s%s%s%s" (uo_sign uo) uo.uo_name (ui_sprint_range uo.uo_range) (uo_sel uo) (uo_conv uo);
  | UC_sig uo -> sprintf "%s%s%s%s" (uo_sign uo) uo.uo_name (ui_sprint_range uo.uo_range) (uo_conv uo);
  | UC_chan uo -> sprintf "%s%s%s%s" (uo_sign uo) uo.uo_name (ui_sprint_range uo.uo_range) (uo_conv uo);
  | UC_queue uo -> sprintf "%s%s%s%s" (uo_sign uo) uo.uo_name (ui_sprint_range uo.uo_range) (uo_conv uo);
  | UC_val uv -> ui_sprint_val uv.uv_val;
  | UC_alu ua -> sprintf "$alu.[%s]" (sprint_dt ua.ua_type);
  | UC_immed id -> sprintf "$immed.[%d]" id;
  | UC_bool id -> sprintf "$bool.[%d]" id;
  | UC_temp ut -> sprintf "$tmp.[%s]%s%s" ut.ut_name (ui_sprint_range ut.ut_range) (ut_conv ut);
  | UC_array uat -> uat.uat_name;
  | UC_list ul -> 
          let str = ref "" in
          List.iter (fun da -> str := !str ^ 
                            (if !str <> "" then "@" else "")
                            ^ (datastr da)) ul.ul_list;
          !str;
  | UC_sel us -> 
     let data,sel = 
         let ds,ss=ref "",ref "" in
         List.iter (fun sel ->
             match sel with
             | UC_val uv ->
             let i =
                 match uv.uv_val with
                 | V_int w -> Int64.to_int w;
                 | _ -> error 505672 "" in
             ds := if !ds = "" then (datastr us.us_obj) else sprintf "%s,%s" !ds (datastr us.us_obj);
             ss := if !ss = "" then (string_of_int i) else sprintf "%s,%d" !ss i;
             | _ -> 
             ds := if !ds = "" then (datastr us.us_obj) else sprintf "%s,%s" !ds (datastr us.us_obj);
             ss := if !ss = "" then (datastr sel) else sprintf "%s,%s" !ss (datastr sel);
           ) us.us_sel; 
         !ds, !ss 
         in
    sprintf "%s.[%s]" us.us_name sel

let pr_frame_id fr = 
  if fr.bf_id <> (-1) then
  sprintf "%s#%d" 
          fr.bf_name 
          fr.bf_id
  else "" 
let pr_frame fr = 
  if fr.bf_src_start = fr.bf_src_end then
  begin
    let pr_src s = sprintf "%s:%d" s.s_file s.s_line in
    if fr.bf_id <> 0 then
    sprintf "%s#%d,%s,%s,[%s]" 
            fr.bf_name 
            fr.bf_id
            (pr_src fr.bf_src_start)
            (
              match fr.bf_parent with
              | Some bf' -> sprintf "%s#%d" bf'.bf_name bf'.bf_id;
              | None -> "*";
            )
            (hlist (fun bf' ->
                      sprintf "%s%d" bf'.bf_name bf'.bf_id;
                    ) fr.bf_childs) else "" 
  end
  else
  begin
    let pr_src1 s = sprintf "%s:%d" s.s_file s.s_line in
    let pr_src2 s = sprintf "..%d" s.s_line in
    if fr.bf_id <> 0 then
    sprintf "%s#%d,%s%s,%s,[%s]" 
            fr.bf_name 
            fr.bf_id
            (pr_src1 fr.bf_src_start) 
            (pr_src2 fr.bf_src_end)                    
            (
              match fr.bf_parent with
              | Some bf' -> sprintf "%s#%d" bf'.bf_name bf'.bf_id;
              | None -> "*";
            )
            (hlist (fun bf' ->
                      sprintf "%s%d" bf'.bf_name bf'.bf_id;
                    ) fr.bf_childs) else "" 
  end

let rec ui_sprint_uc uc =
  let datastr = ui_sprint_ud in
  let arg_str args =
    let argstr arg =
      match arg with
      | UA_data d -> datastr d;
      | UA_expr ul -> 
          let str = ref "" in
          List.iter (fun uc' ->
              str := sprintf "%s/%s" !str (ui_sprint_uc uc')) ul;
          sprintf "%s^^EXPR^^" !str
          in
    let str = ref "" in
    let n = ref (List.length args) in
    List.iter (fun arg ->
      decr n;
      str := !str ^ (argstr arg) ^ (if !n = 0 then "" else ","); 
      ) args;
    if !str = "" then "()" else !str
    in
  let opstr ops =
      let str = ref "" in
      List.iter (fun op ->
              str := !str ^ (op_name op);
          ) ops;
      !str 
      in
  let locstr ul = 
      match ul with
      | UC_next -> "NEXT";
      | UC_label str -> str;
      in
  match uc.ui_code with
  | Bind n -> sprintf "%20s bind (%d)" 
                      (if debug_it "ui_frame" then pr_frame_id uc.ui_frame else "")
                       n;
  | Move (dst,src) -> 
          sprintf "%20s move (%s,%s)" 
                  (if debug_it "ui_frame" then pr_frame_id uc.ui_frame else "")
                  (datastr dst) 
                  (datastr src);
  | Expr (ops,dst,op1,op2) ->
          sprintf "%20s expr (%s,%s,%s,%s)"
                  (if debug_it "ui_frame" then pr_frame_id uc.ui_frame else "")
                  (datastr dst) 
                  (datastr op1)
                  (opstr ops)
                  (datastr op2);
  | Falsejump (res,ul) ->
          sprintf "%20s falsejump (%s,%s)"
                  (if debug_it "ui_frame" then pr_frame_id uc.ui_frame else "")
                  (datastr res)
                  (locstr ul);
  | Jump ul ->
          sprintf "%20s jump (%s)"
                  (if debug_it "ui_frame" then pr_frame_id uc.ui_frame else "")
                  (locstr ul);
  | Label str ->
          sprintf "%20s %s"
                  (sprintf "%s:" str)
                  (if debug_it "ui_frame" then sprintf "BF(%s)" (pr_frame uc.ui_frame) else "");
  | Special _ -> sprintf "%12s Special;" 
                         "";
  | Fun ((opl,ot),sel,args) -> 
      let rec get_ao_name ot =
        match ot with
        | OT_object ao -> ao.ao_name;
        | OT_array at ->
          if is_sel_obj opl then
            sprintf "sel[|%s;...|]" (get_ao_name at.at_objs.(0))
          else 
            sprintf "sel[|%s|]"  (get_ao_name at.at_objs.(
                                  at_index at (obj_sel opl)));
        | OT_queue qu -> get_ao_name (OT_object qu.qu_ao);
        | OT_channel ch -> get_ao_name (OT_object ch.ch_ao);
        | _ -> error 305686 "";
        in
      let ao_name = get_ao_name ot in
      sprintf "%20s fun (%s.%s,%s)" 
              (if debug_it "ui_frame" then pr_frame_id uc.ui_frame else "")
              ao_name sel
              (arg_str args);
  | Nop -> sprintf "%20s nop"                                 
                   (if debug_it "ui_frame" then pr_frame_id uc.ui_frame else "")

(*
** Emit MicroCode into file...
*)
let ui_emit_pro_ucode modu pro =
    out (sprintf "Emitting MicroCode for process %s..." pro.pro_name);
    ind_incr ();

    uc_out Cp_version.vhdl_version;

    let datastr = ui_sprint_ud in
    
    let opstr ops =
        let str = ref "" in
        List.iter (fun op ->
                str := !str ^ (op_name op);
            ) ops;
        !str 
        in
    let locstr ul = 
        match ul with
        | UC_next -> "NEXT";
        | UC_label str -> str;
        in


    let emit_val  = ui_sprint_val in
    let emit_type dt =
            match dt with
            | DT_logic n -> sprintf "L%d" n;
            | DT_int n -> sprintf "I%d" n;
            | DT_string n -> sprintf "S%d" n;
            | DT_char -> "C";
            | DT_bool -> "B";
            | DT_object s -> sprintf "%s" s;
            | DT_lneg -> "(-)"
            | DT_bneg -> "(-)"
            | DT_aneg -> "(-)"
            | DT_natural n -> sprintf "NAT%d" n
            in
    let emit_block dbs =
            match dbs with
            | Some db -> sprintf " in %s" db.db_name;
            | None -> ""
            in
    let is_guarded co =
        match co.co_guard with
        | Some gd ->
            let sep = if List.length gd.gd_req > 1 then " AND " else "" in
            sprintf "IS %s%s%s GUARDED"
                    (if List.mem GD_rd gd.gd_req then "READ" else "")
                    sep
                    (if List.mem GD_wr gd.gd_req then "WRITE" else "")
        | None -> "";
        in


    let rec emit_obj ob =
        match ob with
        | OT_const co ->
            sprintf "const %s: %s" 
                    co.co_name 
                    (emit_val co.co_init);
        | OT_signal co ->
            sprintf "signal %s: %s" 
                    co.co_name 
                    (emit_type co.co_type);
        | OT_reg co ->
            sprintf "register %s: %s" 
                    co.co_name 
                    (emit_type co.co_type);
        | OT_var co ->
            sprintf "variable %s: %s in %s[%d]" 
                    co.co_name 
                    (emit_type co.co_type)
                    (emit_block co.co_block)
                    co.co_index;
        | OT_channel ch ->
            sprintf "channel %s: %s" 
                    ch.ch_obj.co_name 
                    (emit_type ch.ch_obj.co_type);
        | OT_queue qu ->
            sprintf "queue %s: %s" 
                    qu.qu_obj.co_name 
                    (emit_type qu.qu_obj.co_type);
        | OT_array at ->
            let is_block = List.mem AT_block at.at_flags in
            let is_dyn = List.mem AT_dyn at.at_flags in
            let rec get_desc ot = match ot with
              | OT_signal co -> "signal",(emit_type co.co_type),(emit_block co.co_block);
              | OT_reg co -> "register",(emit_type co.co_type),(emit_block co.co_block);
              | OT_var co -> "variable",(emit_type co.co_type),(emit_block co.co_block);
              | OT_queue qu -> "queue",(emit_type qu.qu_obj.co_type),(emit_block qu.qu_obj.co_block);
              | OT_channel ch -> "channel",(emit_type ch.ch_obj.co_type),(emit_block ch.ch_obj.co_block);
              | OT_object ao -> "object",sprintf "%s.%s" ao.ao_type.ta_rules.rl_name ao.ao_type.ta_name,""
              | OT_struct st -> if st.st_objs = [] then
                                  "","",""
                                else (match List.hd st.st_objs with
                                  | OT_signal _ -> "signal";
                                  | OT_reg _ -> "register";
                                  | OT_var _ -> "variable";
                                  | OT_queue _ -> "queue";
                                  | OT_channel _ -> "channel";
                                  | OT_array at' ->  
                                    let sot',_,_ = get_desc at'.at_objs.(0) in
                                    sot'
                                  | _ -> "?"),st.st_type.ts_name,""
              | OT_component st -> "component",st.st_type.ts_name,""
              | _ -> "?","?","" in
            let sot,sdt,sdb = get_desc at.at_objs.(0) in
            let dim = ref "" in
            let size = ref 1 in
            Array.iter (fun i -> size := !size * i) at.at_dim;
            Array.iter (fun i -> if !dim = "" then dim := sprintf "%d" i else
                                 dim := sprintf "%s,%d" !dim i) at.at_dim;
            box 0 "array %s" (sprintf "%s: %s[%s] of %s%s%s" 
                            at.at_name
                            sot !dim sdt sdb
                            (if not is_block && not is_dyn then
                             begin
                              let str = ref "" in
                              for i = 0 to !size-1 
                              do 
                                let nl = if i > 0 && (i mod 8)=0 then "\n" else "" in
                                str := if !str = "" then sprintf "%s_%d" at.at_name i
                                                    else sprintf "%s,%s%s_%d" !str nl at.at_name i;
                              done;
                              box 0 " :: {%s}" (sprintf "\n\n%s" !str)
                             end else ""                            
                            ));
        | OT_array_sel (at,sel) ->
            let is_dyn = List.mem AT_dyn at.at_flags in
            if not is_dyn then
            begin
                sprintf "arraysel %s: %s" 
                  at.at_name
                  (emit_obj at.at_objs.(sel)); 
            end
            else
              sprintf "arraysel %s:" 
                  at.at_name;  
        | OT_object ao ->
            sprintf "object %s: %s.%s" 
                    ao.ao_name
                    ao.ao_type.ta_rules.rl_name
                    ao.ao_type.ta_name;
        | OT_named_value (name,v) ->
            sprintf "val %s: %s" 
                    name (emit_val v);          
        | OT_value v ->
            sprintf "val: %s" 
                    (emit_val v);
        | OT_component st ->
            sprintf "component %s: %s" 
                    st.st_name
                    st.st_type.ts_name;
        | OT_struct st ->
            sprintf "struct %s: %s" 
                    st.st_name
                    st.st_type.ts_name;
        | _ -> "OT=?";
        in

    let rec emit_sym sym =
        match sym with
            | Sym_obj ob ->
                uc_out (sprintf "%s" (emit_obj ob)); 
            | Sym_mod md ->
                uc_out (sprintf "module %s" modu.mod_name); 
            | Sym_rule md ->
                uc_out (sprintf "rules %s" modu.mod_name); 
            | Sym_pro pr ->
                uc_out (sprintf "process %s" pr.pro_name); 
            | Sym_fun f ->
                let sarg = ref "" in
                let n = ref (List.length f.fun_args) in
                List.iter (fun arg ->
                  decr n;
                  sarg := !sarg ^ (sprintf "%s%s"
                                           arg
                                           (if !n > 0 then "," else ""));
                  ) f.fun_args;
                uc_out (sprintf "function %s(%s)" 
                                f.fun_name !sarg
                                ); 
            | Sym_block bl ->
                uc_out (sprintf "block %s: cells[%d] of %s" 
                        bl.db_name 
                        bl.db_size
                        (emit_type (DT_logic bl.db_width))); 
            | Sym_type tp ->
                let tp_name =
                    match tp with 
                    | Type_const tc -> tc.tc_name;
                    | Type_struct ts -> ts.ts_name;
                    | Type_abstract ta -> ta.ta_name;
                    | _ -> "?";
                    in
                uc_out (sprintf "type %s" tp_name);
            | Sym_mon (dbg,mon) ->
                uc_out "monitor:"; 
                emit_sym mon;
        in

    let syms = list_of_sym pro.pro_import in
    if syms <> [] then
    begin
        uc_out "import:";
        uc_out "begin";
        uc_incr ();
        List.iter emit_sym syms;
        uc_decr ();
        uc_out "end";
        uc_out "";
    end;

    let syms = list_of_sym pro.pro_export in
    if syms <> [] then
    begin
        uc_out "export:";
        uc_out "begin";
        uc_incr ();
        List.iter emit_sym syms;        
        uc_decr ();
        uc_out "end";
        uc_out "";
    end;


    if pro.pro_temps <> [] then
    begin
        uc_out "temp:";
        uc_out "begin";
        uc_incr ();
        List.iter (fun co -> uc_out (emit_obj (OT_reg co))) pro.pro_temps;
        uc_decr ();
        uc_out "end";
        uc_out "";
    end;

    let syms = list_of_sym pro.pro_objs in
    if syms <> [] then
    begin
        uc_out "data:";
        uc_out "begin";
        uc_incr ();
        List.iter emit_sym syms;        
        uc_decr ();
        uc_out "end";
        uc_out "";
    end;
    
    List.iter (fun alu ->
            uc_out (sprintf "alu %s: " alu.alu_name);
            uc_out "begin";
            uc_incr ();
            uc_out (sprintf "type: %s" (emit_type alu.alu_type));
            List.iter (fun op ->
                if op <> OP_alu then
                    uc_out (sprintf "op: %s" (op_name op));
                ) alu.alu_ops;
            uc_decr ();
            uc_out "end;";
            uc_out "";
        ) pro.pro_alu;

    uc_out "code:";
    uc_out "begin";




    let rec print_uc uc =
        let arg_str args =
          let argstr arg =
            match arg with
            | UA_data d -> datastr d;
            | UA_expr ul -> 
                uc_incr ();
                List.iter (fun uc' ->
                    print_uc uc') ul;
                uc_decr ();
                "^^EXPR^^"
                in
          let str = ref "" in
          let n = ref (List.length args) in
          List.iter (fun arg ->
            decr n;
            str := !str ^ (argstr arg) ^ (if !n = 0 then "" else ","); 
            ) args;
          if !str = "" then "()" else !str
          in
        match uc.ui_code with
        | Bind n -> uc_out (sprintf "%20s bind (%d)" 
                                    (if debug_it "ui_frame" then pr_frame_id uc.ui_frame else "")
                                    n);
        | Move (dst,src) -> 
                uc_out (sprintf "%20s move (%s,%s)" 
                             (if debug_it "ui_frame" then pr_frame_id uc.ui_frame else "")
                             (datastr dst) 
                             (datastr src));
        | Expr (ops,dst,op1,op2) ->
                uc_out (sprintf "%20s expr (%s,%s,%s,%s)"
                             (if debug_it "ui_frame" then pr_frame_id uc.ui_frame else "")
                             (datastr dst) 
                             (datastr op1)
                             (opstr ops)
                             (datastr op2));
        | Falsejump (res,ul) ->
                uc_out (sprintf "%20s falsejump (%s,%s)"
                                 (if debug_it "ui_frame" then pr_frame_id uc.ui_frame else "")
                                 (datastr res)
                                 (locstr ul));
        | Jump ul ->
                uc_out (sprintf "%20s jump (%s)"
                                (if debug_it "ui_frame" then pr_frame_id uc.ui_frame else "")
                                (locstr ul));
        | Label str ->
                uc_out (sprintf "%20s %s"
                                 (sprintf "%s:" str)
                                 (if debug_it "ui_frame" then sprintf "BF(%s)" (pr_frame uc.ui_frame) else ""));
        | Special _ -> uc_out (sprintf "%12s Special;" 
                                       "");
        | Fun ((opl,ot),sel,args) -> 
            let rec get_ao_name ot =
              match ot with
              | OT_object ao -> ao.ao_name;
              | OT_array at ->
                if is_sel_obj opl then
                  sprintf "sel[|%s;...|]" (get_ao_name at.at_objs.(0))
                else 
                  sprintf "sel[|%s|]"  (get_ao_name at.at_objs.(
                                        at_index at (obj_sel opl)));
              | OT_queue qu -> get_ao_name (OT_object qu.qu_ao);
              | OT_channel ch -> get_ao_name (OT_object ch.ch_ao);
              | _ -> error 305686 "";
              in
            let ao_name = get_ao_name ot in
            uc_out (sprintf "%20s fun (%s.%s,%s)" 
                            (if debug_it "ui_frame" then pr_frame_id uc.ui_frame else "")
                            ao_name sel
                            (arg_str args));
        | Nop -> uc_out (sprintf "%20s nop"                                 
                                 (if debug_it "ui_frame" then pr_frame_id uc.ui_frame else ""));
        in
    List.iter (fun uc ->
        print_uc uc;
        ) pro.pro_ucode;
    uc_out "end";
    ind_decr ()



let ui_emit_module modu  =
    out (sprintf "Emitting object file for module %s..." modu.mod_name);
    ind_incr ();

    uc_out Cp_version.vhdl_version;

    let datastr = ui_sprint_uc in
    
    let sprint_timeunit vunit =
        match vunit with
        | Nsec -> "Nsec";
        | Usec -> "Usec";
        | Msec -> "Msec";
        | Sec -> "Sec";
        | Cycles -> "Cycles" in

    let sprint_frequnit vunit =
        match vunit with
        | Ghz -> "Ghz";
        | Mhz -> "Mhz";
        | Khz -> "Khz";
        | Hz -> "Hz" in


    let rec emit_val v =
            match v with
            | V_int n -> Int64.to_string n;
            | V_float f -> string_of_float f;
            | V_string s -> s;
            | V_char c -> sprintf "%c" c;
            | V_logic s -> s;
            | V_bool b -> if b then "$true" else "$false";
            | V_z -> "Z";
            | V_null -> "";
            | V_freq (v,u) -> sprintf "%s %s" (Int64.to_string v) (sprint_frequnit u)  
            | V_time (v,u) -> sprintf "%s %s" (Int64.to_string v) (sprint_timeunit u)
            | V_list l -> sprintf "[%s]" (hlist emit_val l)
            in
    let emit_type dt =
            match dt with
            | DT_logic n -> sprintf "L%d" n;
            | DT_int n -> sprintf "I%d" n;
            | DT_string n -> sprintf "S%d" n;
            | DT_char -> "C";
            | DT_bool -> "B";
            | DT_object s -> sprintf "%s" s;
            | DT_lneg -> "(-)"
            | DT_bneg -> "(-)"
            | DT_aneg -> "(-)"
            | DT_natural n -> sprintf "NAT%d" n
            in
    let emit_block dbs =
            match dbs with
            | Some db -> sprintf " in %s" db.db_name;
            | None -> ""
            in
    let is_guarded co =
        match co.co_guard with
        | Some gd ->
            let sep = if List.length gd.gd_req > 1 then " AND " else "" in
            sprintf "IS %s%s%s GUARDED"
                    (if List.mem GD_rd gd.gd_req then "READ" else "")
                    sep
                    (if List.mem GD_wr gd.gd_req then "WRITE" else "")
        | None -> "";
        in


    let rec emit_obj ob =
        match ob with
        | OT_const co ->
            sprintf "const %s: %s" 
                    co.co_name 
                    (emit_val co.co_init);
        | OT_signal co ->
            sprintf "signal %s: %s" 
                    co.co_name 
                    (emit_type co.co_type);
        | OT_reg co ->
            sprintf "register %s: %s" 
                    co.co_name 
                    (emit_type co.co_type);
        | OT_var co ->
            sprintf "variable %s: %s in %s[%d]" 
                    co.co_name 
                    (emit_type co.co_type)
                    (emit_block co.co_block)
                    co.co_index;
        | OT_channel ch ->
            sprintf "channel %s: %s" 
                    ch.ch_obj.co_name 
                    (emit_type ch.ch_obj.co_type);
        | OT_queue qu ->
            sprintf "queue %s: %s" 
                    qu.qu_obj.co_name 
                    (emit_type qu.qu_obj.co_type);
        | OT_array at ->
            let is_block = List.mem AT_block at.at_flags in
            let is_dyn = List.mem AT_dyn at.at_flags in
            let rec get_desc ot = match ot with
              | OT_signal co -> "signal",(emit_type co.co_type),(emit_block co.co_block);
              | OT_reg co -> "register",(emit_type co.co_type),(emit_block co.co_block);
              | OT_var co -> "variable",(emit_type co.co_type),(emit_block co.co_block);
              | OT_queue qu -> "queue",(emit_type qu.qu_obj.co_type),(emit_block qu.qu_obj.co_block);
              | OT_channel ch -> "channel",(emit_type ch.ch_obj.co_type),(emit_block ch.ch_obj.co_block);
              | OT_object ao -> "object",sprintf "%s.%s" ao.ao_type.ta_rules.rl_name ao.ao_type.ta_name,""
              | OT_struct st -> if st.st_objs = [] then
                                  "","",""
                                else (match List.hd st.st_objs with
                                  | OT_signal _ -> "signal";
                                  | OT_reg _ -> "register";
                                  | OT_var _ -> "variable";
                                  | OT_queue _ -> "queue";
                                  | OT_channel _ -> "channel";
                                  | OT_array at' ->  
                                    let sot',_,_ = get_desc at'.at_objs.(0) in
                                    sot'
                                  | _ -> "?"),st.st_type.ts_name,""
              | OT_component st -> "component",st.st_type.ts_name,""
              | _ -> "?","?","" in
            let sot,sdt,sdb = get_desc at.at_objs.(0) in
            let dim = ref "" in
            let size = ref 1 in
            Array.iter (fun i -> size := !size * i) at.at_dim;
            Array.iter (fun i -> if !dim = "" then dim := sprintf "%d" i else
                                 dim := sprintf "%s,%d" !dim i) at.at_dim;
            if (sot,sdt,sdb) <> ("","","") then
            box 0 "array %s" (sprintf "%s: %s[%s] of %s%s%s" 
                            at.at_name
                            sot !dim sdt sdb
                            (if not is_block && not is_dyn then
                             begin
                              let str = ref "" in
                              for i = 0 to !size-1 
                              do 
                                let nl = if i > 0 && (i mod 8)=0 then "\n" else "" in
                                str := if !str = "" then sprintf "%s_%d" at.at_name i
                                                    else sprintf "%s,%s%s_%d" !str nl at.at_name i;
                              done;
                              box 0 " :: {%s}" (sprintf "\n\n%s" !str)
                             end else ""                            
                            ))
            else "";
        | OT_array_sel (at,sel) ->
            let is_dyn = List.mem AT_dyn at.at_flags in
            if not is_dyn then
            begin
                sprintf "arraysel %s: %s" 
                  at.at_name
                  (emit_obj at.at_objs.(sel)); 
            end
            else
              sprintf "arraysel %s:" 
                  at.at_name;  
        | OT_object ao ->
            sprintf "object %s: %s.%s" 
                    ao.ao_name
                    ao.ao_type.ta_rules.rl_name
                    ao.ao_type.ta_name;
        | OT_named_value (name,v) ->
            sprintf "value %s: %s" 
                    name (emit_val v);          
        | OT_value v ->
            sprintf "value: %s" 
                    (emit_val v);
        | OT_component st ->
            sprintf "component %s: %s" 
                    st.st_name
                    st.st_type.ts_name;
        | OT_struct st ->
            sprintf "struct %s: %s" 
                    st.st_name
                    st.st_type.ts_name;
        | _ -> "OT=?";
        in

    let rec emit_sym sym =
        match sym with
            | Sym_obj ob ->
                uc_out (sprintf "%s" (emit_obj ob)); 
            | Sym_mod md ->
                uc_out (sprintf "module %s" modu.mod_name); 
            | Sym_rule md ->
                uc_out (sprintf "rules %s" modu.mod_name); 
            | Sym_pro pr ->
                uc_out (sprintf "process %s" pr.pro_name); 
            | Sym_fun f ->
                let sarg = ref "" in
                let n = ref (List.length f.fun_args) in
                List.iter (fun arg ->
                  decr n;
                  sarg := !sarg ^ (sprintf "%s%s"
                                           arg
                                           (if !n > 0 then "," else ""));
                  ) f.fun_args;
                uc_out (sprintf "function %s(%s)" 
                                f.fun_name !sarg
                                ); 
            | Sym_block bl ->
                uc_out (sprintf "block %s: cells[%d] of %s" 
                        bl.db_name 
                        bl.db_size
                        (emit_type (DT_logic bl.db_width))); 
            | Sym_type tp ->
                let tp_name =
                    match tp with 
                    | Type_const tc -> tc.tc_name;
                    | Type_struct ts -> ts.ts_name;
                    | Type_abstract ta -> ta.ta_name;
                    | _ -> "?";
                    in
                uc_out (sprintf "type %s" tp_name);
            | Sym_mon (dbg,mon) ->
                uc_out "monitor:"; 
                emit_sym mon;
        in

    let syms = list_of_sym modu.mod_import in
    if syms <> [] then
    begin
        uc_out "import:";
        uc_out "begin";
        uc_incr ();
        List.iter emit_sym syms;
        uc_decr ();
        uc_out "end";
        uc_out "";
    end;

    let syms = list_of_sym modu.mod_export in
    if syms <> [] then
    begin
        uc_out "export:";
        uc_out "begin";
        uc_incr ();
        List.iter emit_sym syms;        
        uc_decr ();
        uc_out "end";
        uc_out "";
    end;

    let syms = list_of_sym modu.mod_objs in
    if syms <> [] then
    begin
        uc_out "data:";
        uc_out "begin";
        uc_incr ();
        List.iter emit_sym syms;        
        uc_decr ();
        uc_out "end";
        uc_out "";
    end;

    ind_decr ()






let ui_emit_ucode modu = 
  let mn = modu.mod_name in
  out (sprintf "Emitting MicroCode for module %s..." mn);
  let mod_main = List.mem Mod_main modu.mod_flags in
  try
  begin
    (
      try uc_oc := open_out (sprintf "%s%s.obj" compiler.t_output (of_mod modu.mod_name)) with
      _ -> error 0 (sprintf "Can't open file <%s%s>." compiler.t_output (of_mod modu.mod_name))
    );
    ui_emit_module modu;
    close_out !uc_oc;
    (* 
    ** Create for each process a separate entity and file
    *)
    List.iter (fun pro ->
        (
          try uc_oc := open_out (sprintf "%s%s_%s.uc" 
                                   compiler.t_output
                                   (of_mod modu.mod_name)
                                   pro.pro_name) with
          _ -> error 0 (sprintf "Can't open file <%s%s_%s.uc"
                                 compiler.t_output
                                 (of_mod modu.mod_name)
                                 pro.pro_name)
        );
        ui_emit_pro_ucode modu pro;
        close_out !uc_oc;
      ) modu.mod_procs;
  end
  with
    | Synthesis str -> out (sprintf "Synthesizing of module <%s> failed:\n %s"
                                    mn str); exit 1


(*
** Use search pattern to compact MicroCode instruction groups.
**
**  1. Expr{[],UC_immed,...}; Move {DST,UC_immed}  =>
**     Expr{[],DST}
**  2. Expr{[],UC_alu,...}; Move {DST,UC_alu}  =>
**     Expr{[],DST}
**  3. Expr{[],UC_alu_bool,...}; Move {DST,UC_alu_bool}  =>
**     Expr{[],DST}
**  
**  4. Jump {L}; L => {}
**  5. Falsejump/Jump {L1_end:Jump L2}; => L1_end=L2
**
**
*)
let ui_compact_ucode ucl =
    out "Compacting MicroCode instructions...";
    
    let jump_merge = ref [] in
    let last_label = ref "" in
    let nop fr = {ui_code=Nop;ui_frame=fr} in

    let relocate_jump ui =
        let fix_label ul =
            match ul with
            | UC_label str ->
                let reloc = ref str in
                for i = 1 to 2 
                do
                  (*
                  ** Two passes are required for 
                  ** relocate paths before the actual
                  ** relocation...
                  *)
                  List.iter (fun (label,label') ->
                      if !reloc = label then 
                        reloc := label';
                      ) !jump_merge;
                done;
                UC_label !reloc
            | _ -> ul;
            in
        match ui.ui_code with
        | Jump ul -> ui.ui_code <- Jump (fix_label ul);
        | Falsejump (ud,ul) -> ui.ui_code <- Falsejump (ud,(fix_label ul));
        | _ -> ();
        in


    let is_immed x =
        match x with
        | UC_immed _ -> true;
        | _ -> false in
    let is_bool x =
        match x with
        | UC_bool _ -> true;
        | _ -> false in
    let is_alu x =
        match x with
        | UC_alu _ -> true;
        | _ -> false in
    let is_alu_bool x =
        match x with
        | UC_alu ua  -> ua.ua_type = DT_bool;
        | _ -> false in
    let not_end l =
        let str_end = "_end" in
        let n = String.length l in
        (n < 4) ||
        ((String.sub l (n-4) 4) <> str_end) in
    
    let forge x y =
        ((is_immed x) && (is_immed y)) ||
        ((is_alu x) && (is_alu y)) ||
        ((is_alu_bool x) && (is_alu_bool y)) ||
        (x = y) in

    let is_sel x =
      match x with
      | UC_sel _ -> true;
      | _ -> false; in
      
    let compact i1 i2 =
        match i1.ui_code with
        | Expr (ops,xdst,xsrc1,xsrc2) ->
        begin
            (*
            ** Find
            **      Expr(..,dst_e,..)
            **      Move(dst,dst_e)
            ** patterns.
            *)
            match i2.ui_code with
            | Move (mdst,msrc) -> 
                if (forge msrc xdst) (* && not (is_sel mdst) ?TODO? *) then
                    [{i1 with ui_code=Expr (ops,mdst,xsrc1,xsrc2)};nop i2.ui_frame]
                else
                    [];
            | _ -> [];
        end;
        | Jump ul1 ->
        begin
            (*
            ** Find
            **      Jump L
            **   L: ...
            ** patterns.
            *)
            match i2.ui_code with
            | Label l2 -> 
            begin
                match ul1 with
                | UC_label l1 ->  
                    if l1 = l2 then 
                    begin
                        jump_merge := !jump_merge @ [!last_label,l2;];
                        last_label := l1;
                        [nop i2.ui_frame;i2] 
                    end
                    else [];
                | _ -> [];
            end;
            | _ -> [];
        end;
        | Label l1 -> 
            last_label := l1; [];
        | _ -> [];
        in 

    let rec iter il =
        match il with
        | last::[] -> 
        begin
            (*
            ** Can't be compacted anymore.
            *)
            match last.ui_code with
            | Jump ul ->  
            begin
                match ul with
                | UC_label label ->
                    jump_merge := !jump_merge @ [!last_label,label];
                    [last];
                | _ -> [last];
            end;
            | _ -> [last];
        end;
        | this::tl ->
        begin
            let next,remains =
                match tl with
                | hd'::tl' -> hd',tl'
                | [] -> error 597144 "compact_ucode.next";
                in
            let compacted = compact this next in
            if compacted <> [] then
                compacted @ (iter remains)
            else
                [this] @ (iter tl);
        end;    
        | [] -> [];
        in


    let ucl' = iter ucl in
    (*
    ** Find 
    **  L1_end: 
    **      Jump L2 
    ** and
    **  L1:
    **      Nop
    **  L2:
    **      ***
    **
    ** patterns and update jump_merge list. Must be done HERE!
    *)
    let rec find_ljumps li =
        match li with
        | (i1::(i2::tl)) ->
        begin
            match i1.ui_code with
            | Label l1 -> 
            begin
              (*
                if not_end l1 then
                    find_ljumps (i2::tl)
                else
              *)
                match i2.ui_code with
                | Jump (UC_label l2) when not (not_end l1) (* when (l2 <> "%END") *) ->
                    if not (List.mem (l1,l2) !jump_merge) then
                        jump_merge := !jump_merge @ [l1,l2];    
                    find_ljumps (i2::tl);
                | Nop ->
                begin
                  match tl with
                  | i3::tl' -> 
                  begin
                    match i3.ui_code with
                    | Label l3 ->
                      if not (List.mem (l1,l3) !jump_merge) then
                         jump_merge := !jump_merge @ [l1,l3];  
                      find_ljumps (i3::tl');
                    | _ -> find_ljumps (i2::tl);
                  end;
                  | _ -> find_ljumps (i2::tl);
                end;
                | _ -> find_ljumps (i2::tl);
            end;
            | _ -> find_ljumps (i2::tl);
        end;
        | _ -> () in

    find_ljumps ucl';


    (*
    ** Find last unused label if any:
    ** ...
    ** Label1: XX
    ** End:    Jump END
    *)

    let rec check_end_label li =
      match li with
      | i1 :: [i2] ->
      begin
            match i1.ui_code with
            | Label l1 -> 
            begin
              match i2.ui_code with
              | Jump (UC_label l2) when (l2 = "%END") ->
                jump_merge := !jump_merge @ [l1,"%END"];
              | _ -> ();
            end;
            | _ -> ();        
      end;
      | hd::tl -> 
        check_end_label tl;
      | [] -> () in

    check_end_label ucl'; 
    

    

    if !jump_merge <> [] then
    begin
        (*
        ** Relocate jumps with new target labels...
        ** After a relocation step, Label-Nop pairs can occur,
        ** therefore update merge list and relocate again...
        *)
        let n = ref 0 in
        while !n < (List.length !jump_merge)
        do
          debug "ui_compact_ucode" with (
            let str = ref "Jump merge:\n" in   
            List.iter (fun (l1,l2) -> str := sprintf "%s%s => %s\n" !str l1 l2) !jump_merge;
            !str);
          n := List.length !jump_merge;
          List.iter (fun ui -> relocate_jump ui) ucl';
          find_ljumps ucl';
        done;
        ucl'
    end
    else ucl'

(*
** MicroCode binding (post parallelisation)
*)

let ui_bind_ucode pro ucl =
    out "Binding MicroCode instructions...";
    let immed x = 
        match x with
        | UC_immed _ -> true;
        | _ -> false in
    let bool x = 
        match x with
        | UC_bool _ -> true;
        | _ -> false in
            
    let bind bl last =
        let n = (List.length bl) + 1 in
        [
            {
                ui_code = Bind n;
                ui_frame = last.ui_frame;
            }
        ] @ bl @ [last] in

    let bind_count = ref 0 in
    let decr_bind () =
      decr bind_count; bind_count := max 0 !bind_count;
      in
    let rec iter bl il =
        match il with
        | this::tl ->
        begin
            let bind_next = 
                match tl with
                | next::_ -> 
                begin
                    match next.ui_code with
                    | Falsejump _ -> true;
                    | _ -> false; 
                end;
                | [] -> false in

            match this.ui_code with
            | Bind n -> 
              (*
              ** Consider explicit binding...
              *)
              bind_count := n+1;
              [this] @ (iter [] tl);
            | Expr (ops,dst,src1,src2) ->
              decr_bind ();
              let opm = op_mode (List.hd ops) in
              if !bind_count = 0 then
              begin
                if (immed dst) || 
                   ((bool dst) && opm = OP_bool) ||
                   bind_next then
                    iter (bl @ [this]) tl
                else if bl <> [] then
                    (bind bl this) @ (iter [] tl)
                else
                    [this] @ (iter [] tl);
              end
              else
                [this] @ (iter [] tl);
            | Falsejump (cond,targ) ->
                decr_bind ();
                if !bind_count = 0 then
                  (bind bl this) @ (iter [] tl)
                else
                  [this] @ (iter [] tl);
            | _ -> 
              decr_bind (); 
              [this] @ (iter [] tl);                
        end;    
        | [] -> if bl <> [] then error 729482 "bind_ucode: missing last instruction"
                            else [];
        in
    iter [] ucl

(*
** Extract ALU (type,size,operators)
*)
let ui_infer_alu pro =
    out (sprintf "Extracting ALU for process <%s>..." pro.pro_name);
    ind_incr ();
    let use_alu ops = List.mem OP_alu ops in

    let pr_frame fr = 
            let pr_src s = sprintf "%s:%d" s.s_file s.s_line in
            sprintf "<B%d %s>" fr.bf_id
                    (pr_src fr.bf_src_start) in


    List.iter (fun ui ->
        debug "ui_infer_alu" with (sprintf "checking Instr <%s>..." (pr_frame ui.ui_frame));
        match ui.ui_code with
        | Expr (ops,dst,src1,src2) ->
            if use_alu ops then
                add_alu pro ops (dt_of_udl [dst;src1;src2]);
        | _ -> ();
        ) pro.pro_ucode;

    List.iter (fun alu ->
        let size,tname =
            match alu.alu_type with
            | DT_logic n -> n,"logic";
            | DT_int n -> n,"int";
            | _-> error 419466 "infer_alu";
            in
        out (sprintf "Found ALU: Size=%d bits, Type=%s, Ops=[%s]"
                     size tname
                     (
                       let str = ref  "" in
                       List.iter (fun op -> 
                           str := !str ^ (sprintf "<%s>" (op_name op));
                           ) alu.alu_ops;
                       !str
                     ));
        ) pro.pro_alu;
    ind_decr ()
    

let ui_resolve_temps pro =
    out (sprintf "Resolving temporary registers for process <%s>..." pro.pro_name);
    ind_incr ();

    let pr_frame fr = 
            let pr_src s = sprintf "%s:%d" s.s_file s.s_line in
            sprintf "<B%d %s>" fr.bf_id
                    (pr_src fr.bf_src_start) in

    let is_temp ud =
        match ud with
        | UC_temp _ -> true;
        | _ -> false 
        in

    let rec is_temp' ud =
        match ud with
        | UC_temp _ -> true;
        (*
        ** Required for preallocated temporary registers
        *)
        | UC_reg uo ->
        begin
            match uo.uo_obj with
            | Some co ->
                List.mem co pro.pro_temps;
            | None -> false;
        end;        
        | UC_sel us ->
        begin
            let tmp = ref false in
            List.iter (fun ud -> tmp := !tmp or (is_temp' ud)) us.us_sel;
            !tmp 
        end;
        | _ -> false 
        in

    let regs = ref [] in
    let tmp_exist reg = 
        let ut = 
            match reg with
            | UC_temp ut -> ut;
            | _ -> error 781128 "resolve_temps";
            in
        let exists = ref false in
        List.iter (fun reg' ->
            let ut' = 
                match reg' with
                | UC_temp ut -> ut;
                | _ -> error 604342 "resolve_temps";
                in
            if ut.ut_name = ut'.ut_name &&
               ut.ut_frame = ut'.ut_frame 
               then exists := true;
            debug "ui_resolve_temps" with (
                let print_label lo =
                  match lo with
                  | Some l ->
                  begin
                    match l with
                    | UC_label str -> str;
                    | _ -> "" 
                  end;
                  | None -> "" in
                sprintf "ui_resolve_temps.tmp_exist: ut=%s.%s ut'=%s.%s"
                             ut.ut_name (print_label ut.ut_frame)
                             ut'.ut_name (print_label ut'.ut_frame));
            ) !regs;
        !exists in

    let tmp_find reg = 
        let ut = 
            match reg with
            | UC_temp ut -> ut;
            | _ -> error 937660 "resolve_temps";
            in
        let exists = ref None in
        List.iter (fun reg' ->
            let ut' = 
                match reg' with
                | UC_temp ut' -> ut';
                | _ -> error 654986 "resolve_temps";
                in
            if ut.ut_name = ut'.ut_name &&
               ut.ut_frame = ut'.ut_frame 
               then exists := Some ut';
            ) !regs;
        match !exists with
        | Some ut -> ut;
        | None -> error 167673 "resolve_temps: tmp_find failed" 
        in
          
    let add_tmp reg =
        if not (tmp_exist reg) then
        begin
            let ut = 
                match reg with
                | UC_temp ut -> ut;
                | _ -> error 632987 "resolve_temps: unexpected UD";
                in
            let size,tname,phy_type = 
                match ut.ut_type.uo_expr_type with
                | DT_logic n -> n,"logic",DT_logic n;
                | DT_int n -> n,"int", DT_int n;
                | DT_bool -> 1,"bool", DT_logic 1;
                | DT_char -> 8,"char", DT_logic 8;
                | DT_object str -> 0,str, DT_object str;
                | _ -> 0,"",ut.ut_type.uo_expr_type 
                in
            debug "ui_resolve_temps" with (sprintf "ui_resolve_temps.add_tmp: new_tmp for %s: %s[%d]"  ut.ut_name tname size);

            let co = new_tmp pro ut.ut_type.uo_expr_type in
            co.co_rules <- !core_rules;
            ut.ut_obj <- Some co;
            ut.ut_type.uo_phys_type <- Some phy_type; 
            regs := !regs @ [reg];
        end
        else
        begin
            (*
            ** Temporary register was already assigned (LHS). Assign the
            ** temporary register to succeeding calls (RHS,further LHS).
            *) 
            let ut = 
                match reg with
                | UC_temp ut -> ut;
                | _ -> error 207962 "resolve_temps: unexpected UD";
                in
            debug "ui_resolve_temps" with (sprintf "ui_resolve_temps.add_tmp for %s"  ut.ut_name);

            let ut' = tmp_find reg in
            if ut'.ut_obj = None then error 952300 (sprintf "UT %s without OBJ"
                                             ut'.ut_name);
            ut.ut_obj <- ut'.ut_obj;
        end
        in

    let rec update_tmp reg =
      match reg with
      | UC_temp ut ->
      begin
        let obj = 
          match ut.ut_obj with
          | Some co -> co;
          | None -> error 47123 (sprintf "resolve_temps: no ut_obj in %s"
                                   ut.ut_name);
          in
        let lhs = List.mem UO_lhs ut.ut_flags in
        
        ut.ut_type.uo_data_type <- (
          match ut.ut_range with
          | Some (a,b) ->
            (*
            ** Special case: temporary register with subrange.
            ** Used only in intermediate expressions.
            *)
            ut.ut_type.uo_data_type;
          | None -> obj.co_type
          );
        let data_dt = ut.ut_type.uo_data_type in
        let expr_dt = ut.ut_type.uo_expr_type in
        (*
        ** Update physical data type if required (expanded register)
        *)
        ut.ut_type.uo_phys_type <- (
          match ut.ut_type.uo_phys_type with
          | Some (DT_logic n) -> 
          begin
            match data_dt with
            | DT_logic n' -> if n' > n then Some (DT_logic n') else ut.ut_type.uo_phys_type;
            | _ -> ut.ut_type.uo_phys_type;
          end; 
          | Some (DT_int n) -> 
          begin
            match data_dt with
            | DT_int n' -> if n' > n then Some (DT_int n') else ut.ut_type.uo_phys_type;
            | _ -> ut.ut_type.uo_phys_type;
          end; 
          | None -> Some data_dt;
          | _ -> ut.ut_type.uo_phys_type; 
          );
      end;
      | UC_reg uo ->
        (*
        ** Contains temporary register!
        *)
        let obj = 
          match uo.uo_obj with
          | Some co -> co;
          | None -> error 47123 (sprintf "resolve_temps: no uo_obj in %s"
                                   uo.uo_name);
          in
        let lhs = List.mem UO_lhs uo.uo_flags in
        
        uo.uo_type.uo_data_type <- (
          match uo.uo_range with
          | Some (a,b) ->
            (*
            ** Special case: temporary register with subrange.
            ** Used only in intermediate expressions.
            *)
            uo.uo_type.uo_data_type;
          | None -> obj.co_type
          );
        let data_dt = uo.uo_type.uo_data_type in
        let expr_dt = uo.uo_type.uo_expr_type in
        (*
        ** Update physical data type if required (expanded register)
        *)
        uo.uo_type.uo_phys_type <- (
          match uo.uo_type.uo_phys_type with
          | Some (DT_logic n) -> 
          begin
            match data_dt with
            | DT_logic n' -> if n' > n then Some (DT_logic n') else uo.uo_type.uo_phys_type;
            | _ -> uo.uo_type.uo_phys_type;
          end; 
          | Some (DT_int n) -> 
          begin
            match data_dt with
            | DT_int n' -> if n' > n then Some (DT_int n') else uo.uo_type.uo_phys_type;
            | _ -> uo.uo_type.uo_phys_type;
          end; 
          | None -> Some data_dt;
          | _ -> uo.uo_type.uo_phys_type; 
          );
      | UC_sel us ->
        List.iter update_tmp us.us_sel;
      | _ -> error 123101 "" 
        in
 
    let release_reg label =
        debug "ui_resolve_temps" with (sprintf "ui_resolve_temps.release_reg: %s" label);
        List.iter (fun reg ->
            let ut = 
                match reg with
                | UC_temp ut -> ut;
                | _ -> error 250519 "resolve_temps";
                in
            match ut.ut_frame with
            | Some frame -> 
            begin    
                let label' =
                    match frame with
                    | UC_label str -> str^"_end";
                    | _ -> "" in
                
                if label' = label then
                begin
                    match ut.ut_obj with
                    | Some obj ->
                        debug "ui_resolve_temps" with (sprintf "ui_resolve_temps.release_reg: releasing <%s>..." obj.co_name);
                        release_tmp pro obj;
                    | None -> error 680929 "resolve_temps: UC_temp without obj";
                end;
            end;
            | None -> error 448510 (sprintf
                            "resolve_temps: UC_temp <%s> without frame"
                            ut.ut_name);
            ) !regs in

    List.iter (fun ui ->
        debug "ui_resolve_temps" with (sprintf "ui_resolve_temps: checking Instr %s..." (pr_frame ui.ui_frame));
        match ui.ui_code with
        | Expr (ops,dst,src1,src2) ->
            if is_temp dst then
                add_tmp dst;
            if is_temp src1 then
                add_tmp src1;
            if is_temp src2 then
                add_tmp src2;
        | Move (dst,src) ->
            if is_temp dst then
                add_tmp dst;
            if is_temp src then
                add_tmp src;
        | Label str -> release_reg str;
        | _ -> ();
        ) pro.pro_ucode;
    out (sprintf "Created %d register(s):" (List.length pro.pro_temps));
    List.iter (fun ui ->
        debug "ui_resolve_temps" with (sprintf "ui_resolve_temps: updating Instr %s..." (pr_frame ui.ui_frame));
        match ui.ui_code with
        | Expr (ops,dst,src1,src2) ->
            if is_temp' dst then
                update_tmp dst;
            if is_temp' src1 then
                update_tmp src1;
            if is_temp' src2 then
                update_tmp src2;
        | Move (dst,src) ->
            if is_temp' dst then
                update_tmp dst;
            if is_temp' src then
                update_tmp src;
        | Label str -> release_reg str;
        | _ -> ();
        ) pro.pro_ucode;


    ind_incr ();
    List.iter (fun t ->
            let size,tname = 
                match t.co_type with
                | DT_logic n -> n,"logic";
                | DT_int n -> n,"int";
                | DT_bool -> 1,"bool";
                | DT_char -> 8,"char";
                | _ -> error 400571 "resolve_temps: TEMP of unexpected type";
                in
            out (sprintf "%s: %s[%d]%s" t.co_name tname size
                 (if List.mem Obj_inuse t.co_flags then "*" else ""));
        ) pro.pro_temps;
    ind_decr ();
    ind_decr ()

(*
** Resolve boolean expressions.
*)
let ui_resolve_bool pro =
    let modu = pro.pro_module in
    let max_bool = ref 0 in
    let ucl = pro.pro_ucode in
    out (sprintf "Resolving boolean expressions for process <%s>..." pro.pro_name);
    ind_incr ();

    let is_immed x =
        match x with
        | UC_immed _ -> true;
        | _ -> false in
    let is_bool x =
        match x with
        | UC_bool i -> true;
        | _ -> false in
    let bool_width x =
        match x with
        | UC_bool i -> i;
        | _ -> 0 in
    let is_alu x =
        match x with
        | UC_alu _ -> true;
        | _ -> false in
    let is_alu_bool x =
        match x with
        | UC_alu ua  -> ua.ua_type = DT_bool;
        | _ -> false in
    let not_end l =
        let str_end = "_end" in
        let n = String.length l in
        (n < 4) ||
        ((String.sub l (n-4) 4) <> str_end) in

    let collect = ref [] in
    let ucl' = ref [] in
    let add_coll this =
        collect := !collect @ [this] in
    let add this =
        ucl' := !ucl' @ [this] in
    let release () =
        ucl' := !ucl' @ !collect;
        collect := [] in
    let convert () =
        (*
        ** BOOLX -> IMMEDX
        *)
        let bools = ref [] in
        List.iter (fun this ->
            match this.ui_code with
            | Expr (ops,dst,src1,src2) ->
            begin
                match dst with
                | UC_bool i -> bools := !bools @ [i];
                | _ -> ();
            end;
            | _ -> ();
            ) !collect;

        ucl' := !ucl' @ (List.map (fun this ->
            match this.ui_code with
            | Expr (ops,dst,src1,src2) ->
            begin
                match dst with
                | UC_bool i -> 
                    let dst' = UC_immed i in
                    let src1' =
                        match src1 with
                        | UC_bool i when (List.mem i !bools) -> UC_immed i;
                        | _ -> src1 in
                    let src2' =
                        match src2 with
                        | UC_bool i when (List.mem i !bools) -> UC_immed i;
                        | _ -> src2 in
                    {this with ui_code=Expr (ops,dst',src1',src2')};
                | _ -> this;
            end;
            | Falsejump (cond,ul) ->
            begin
                match cond with
                | UC_bool i -> 
                    let cond' = UC_immed i in
                    {this with ui_code=Falsejump (cond',ul)};
                | _ -> this;
            end;
            | _ -> this; 
            ) !collect);
        collect := [] in

    (*
    ** Collect groups of boolean expressions and convert them
    ** to immediate expressions.
    *)
    let rec iter il =
        match il with
        | last::[] -> 
        begin
            if !collect <> [] then convert ();
        end;
        | this::tl ->
        begin
            match this.ui_code with
            | Expr (ops,dst,src1,src2) -> 
                let opm = op_mode (List.hd ops) in
                let alu = List.mem OP_alu ops in
                let try_immed = 
                    match tl with
                    | next :: _ -> 
                    begin
                        match next.ui_code with
                        | Falsejump _ -> true;
                        | Move _ -> true;
                        | _ -> false; 
                    end;
                    | _ -> false in 
                if (not alu && (is_bool dst)) || try_immed
                   then add_coll this
                else 
                begin
                    release ();
                    add this;
                end;
                iter tl; 
            | Falsejump (cond,ul) ->
                if is_bool cond then 
                begin
                    add_coll this;
                    convert ();
                end
                else 
                begin
                    release ();
                    add this;
                end;
                iter tl;
            | _ -> convert ();
                   add this;
                   iter tl;
        end;    
        | [] -> ();
        in
    iter ucl;
    pro.pro_ucode <- !ucl';

    (*
    ** Find maximal boolean expression width...
    *)
    List.iter (fun this ->
        match this.ui_code with
        | Expr (ops,dst,src1,src2) ->
            max_bool := max !max_bool (bool_width dst);
        | _ -> ();
        ) !ucl';

    (*
    ** Replace bools with bool_reg.
    *)
    if !max_bool > 0 then
    begin
        out (sprintf "Creating boolean result registers [%d]."
                     !max_bool);

        let ud = Array.init !max_bool (fun i ->
            let co = new_reg modu (Some pro) 
                             (sprintf "bool_reg_%d" i) (DT_bool) in
            co.co_rules <- !core_rules;
            let ot = OT_reg co in
            let expr_dt = DT_bool in
            let ud lhs opl = ud_of_ot ot opl 
                                   (if lhs then [UO_lhs;UO_loc] else 
                                                [UO_rhs;UO_loc]) 
                                   expr_dt in
            ud) in

        pro.pro_ucode <-
          List.map (fun this ->
            match this.ui_code with
            | Expr (ops,dst,src1,src2) ->
            begin
                let dst' = 
                        match dst with
                        | UC_bool i -> ud.(i-1) true []
                        | _ -> dst in
                let src1' =
                        match src1 with
                        | UC_bool i -> ud.(i-1) false [];
                        | _ -> src1 in
                let src2' =
                        match src2 with
                        | UC_bool i -> ud.(i-1) false [];
                        | _ -> src2 in
                {this with ui_code=Expr (ops,dst',src1',src2')};
            end;
            | Falsejump (cond,ul) ->
            begin
                match cond with
                | UC_bool i -> 
                    let cond' = ud.(i-1) false [] in
                    {this with ui_code=Falsejump (cond',ul)};
                | _ -> this;
            end;
            | _ -> this; 
            ) pro.pro_ucode;
    end;
    ind_decr ()

(*
** Post adjust types (TC) of RHS objects for LHS objects with ET <> PT <> DT, e.g. variable objects
** with PT=DT_logic and ET <> DT_logic on LHS requires TC with RHS objects (concerns 
** <Move> rather than <Expr> instructions).
*)
let ui_expr_type_align pro =
  out (sprintf "Post type alignment of expressions for process <%s>..." pro.pro_name);
  let n = ref 0 in
  let temps = ref 0 in
  let label = ref "" in
  ind_incr ();
  let rec iter ul =
    match ul with
    | this :: tl ->
    begin
      match this.ui_code with
      | Label str -> label := str; [this]@(iter tl);
      | Expr (ops,dst,src1,src2) ->
      begin
        let is_assign,dst =
          match dst with
          | UC_immed _ ->
          begin
            let rec find_dst ul' =
              match ul' with
              | that :: tl' -> 
              begin
                match that.ui_code with
                | Move (dst',_) -> true,dst';
                | Falsejump _ -> false,dst;
                | _ -> find_dst tl';
              end;
              | [] -> false,dst in
            find_dst tl;  
          end;
          | _ -> true,dst in
        if is_assign then
        begin
          let dst_type = type_of_ud dst in
          match dst_type.uo_phys_type with
          | Some pt when pt <> dst_type.uo_data_type ->
            (*
            ** Align DST type conversion (indeed applied to RHS of expression!)
            *)
            dst_type.uo_conv <- Some pt;
            incr n;
            if dst_type.uo_data_type <> DT_bool then
            begin
              (*
              ** Replace expression by temporary register - we can't apply
              ** type conversion to expressions!!!!
              *)
              let tmp_name = sprintf "#%d" !temps in
              incr temps;
              let temp = {
                      ut_type = uo_type dst_type.uo_data_type dst_type.uo_expr_type;
                      ut_range = None;
                      ut_name = name_of_ud dst;
                      ut_frame = Some (UC_label !label);
                      ut_obj = None;
                      ut_flags = [UO_lhs;UO_loc];
                    } in
              let ut = UC_temp temp in
              [
                {this with ui_code=Expr (ops,ut,src1,src2)};
                {this with ui_code=Move (dst,ud_fix_rhs ut)}
              ] @ (iter tl);
            end
            else
            begin
              [
                {this with ui_code=Expr (ops,dst,src1,src2)}
              ] @ (iter tl);
            end;
          | _ -> [this]@(iter tl);
        end else [this]@(iter tl);
      end;
      | Move (dst,src) ->
      begin
        let dst_type = type_of_ud dst in
        match dst_type.uo_phys_type with
        | Some pt when pt <> dst_type.uo_data_type ->
          (*
          ** Align SRC type conversion.
          *)
          let src_type = type_of_ud src in
          src_type.uo_conv <- Some pt;
          incr n;
          [{this with ui_code=Move (dst,src)}] @ (iter tl);
        | _ -> 
        begin
          let src_type = type_of_ud src in
          match src_type.uo_phys_type with
          | Some pt when pt <> src_type.uo_data_type  && src_type.uo_sign <> None ->
            (*
            ** First assign and convert SRC to DST, and then change sign of DST!
            *)
            let src',uo_sign = ud_change_sign src None in
            let dst',_       = ud_change_sign dst uo_sign in
            [
                {this with ui_code=Move (dst,src')};
                {this with ui_code=Move (dst,ud_fix_rhs dst')}
            ] @ (iter tl);            
          | _ ->
            [this]@(iter tl);
        end;
      end;
      | _ -> [this]@(iter tl); 
    end;
    | [] -> [] in    
  pro.pro_ucode <- iter pro.pro_ucode;
  out (sprintf "%d object references aligned." !n);
  ind_decr ()

  
