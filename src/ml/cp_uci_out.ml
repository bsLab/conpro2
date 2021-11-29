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
**    $CREATED:     4.11.2008
**    $VERSION:     2.21
**
**    $INFO:
**
**  Micro Code Interface -  Output Class
**
**    $ENDOFINFO
**
*)
open Cp_common
open Cp_types
open Cp_syntax
open Cp_symbol
open Cp_utils
open Cp_data
open Cp_printtypes
open Cp_data_core
open Printf
open Cp_fun
open Cp_uci_types
open Cp_print
open Cp_block_frame

class uci_out pro =
  object (self)    
  (*
  ** Module, object and type name
  *)
  val tname = "uci_out"
  val oname = pro.pro_name
  val modu = pro.pro_module 
 

  val oc = ref (Pervasives.stdout)
  val mutable out_indent = 0
  method private out str = 
    output_string !oc ((ibox out_indent str)^"\n")
  method private ind_incr = out_indent <- out_indent + 2
  method private ind_decr = out_indent <- out_indent - 2
  method private file_open fname =
    try
      oc := open_out (sprintf "%s%s" compiler.t_output fname);
    with
      _ -> self#err "file_open" (sprintf "Can't open file <%s%s>." compiler.t_output fname) 
  method private file_close =
    close_out !oc;
    oc := Pervasives.stdout
    

  method private info = sprintf "ucode.%s.%s" tname oname

  val empty_env : ((string,uci_env) Hashtbl.t) = Hashtbl.create 1
  val mutable env: ((string,uci_env) Hashtbl.t) = Hashtbl.create 100
  val mutable types: ((string, uci_type) Hashtbl.t) = Hashtbl.create 100
  
  (*
  *********************************************************************
  ** Utils
  *********************************************************************
  *)
  method private cur_pos p =
    if p <> {f_name="";f_cpos=0} then 
    begin
      an.a_curpos <- source_of_pos p;
    end
  method private get_pos =
    pos_of_source an.a_curpos


  method private str_is_value str =
    let n = String.length str in
    let m = n - 2 in
    if m > 0 then
    begin
      match str.[0],str.[1] with
      | '0','b' -> true
      | '0','x' -> true
      | '0','o' -> true
      | '0'..'9',_ -> true
      | 'X','"' -> true
      | '\'',_ -> true
      | '"',_ -> true
      | _ -> false
    end
    else if n > 0 then
    begin
      match str.[0] with
      | '0'..'9' -> true
      | _ -> false        
    end
    else false

  method private and_l pl =
    match pl with
    | (p1 :: (p2 :: tl)) -> sprintf "%s and %s" p1 (self#and_l (p2::tl));
    | [p] -> p;
    | [] -> "" 
  

  method private com_l pl =
    match pl with
    | (p1 :: (p2 :: tl)) -> sprintf "%s,%s" p1 (self#com_l (p2::tl));
    | [p] -> p;
    | [] -> "" 
   
  method private err where str =
    error 0 (sprintf "UCI <%s>#%s:%s" self#info where str) 
 
  (*
  *********************************************************************
  ** Parameter environment, for example $datawidth
  *********************************************************************
  *)
  method private set_env new_env = 
    let err str = error 0 (sprintf "UCI <%s>#%s:%s" self#info "set_env" str) in
    List.iter (fun uce ->
      try
      begin
        debug "uci_out"  with (sprintf "set_env(%s)" uce.uce_name);
        let uce' = Hashtbl.find env uce.uce_name in
        if uce.uce_type <> uce'.uce_type then
          err (sprintf "Expected expression type <%c>, but got <%c>." uce.uce_type uce'.uce_type);
        out (sprintf "UCI <%s>#set_env: setting environment parameter <%s> to new value <%s>." 
                             self#info uce.uce_name uce.uce_val);
        uce'.uce_val <- uce.uce_val;
        uce'.uce_var <- true;
      end
      with
        Not_found -> Hashtbl.add env uce.uce_name uce;      
      ) new_env

  method private get_env = 
    let l = ref [] in
    Hashtbl.iter (fun name env' ->
                l := !l @ [env'];
            ) env;
    !l

  (*
  *********************************************************************
  ** EMIT MicroCode
  *********************************************************************
  *)

  (*
  ** Types: structures, enumerations and abstract object types
  *)
  method private add_type tp =
    if not (Hashtbl.mem types tp.uct_name) then
      Hashtbl.add types tp.uct_name tp
      
  (*
  ** type xyz : {
  **    e1
  **    e2
  **    ...
  **    }
  **  1. e => name:data_type
  **  2. e => name
  **  3. e => funname(data_type,...):data_type
  **
  *)
  method private p_tp =
    let l = ref [] in
    Hashtbl.iter (fun name tp ->
                l := !l @ [tp];
            ) types;
    let tpl = ref [] in
    List.iter (fun tp ->
      tpl := !tpl @ [
        sprintf "type %s: {" tp.uct_name
      ] @ (List.map (fun line -> ibox 4 line) tp.uct_def) @
      [
        "  }"
      ];
      ) !l;
    !tpl
    
    
  (*
  ** Datatype formatting
  *)
  method private p_dt dt =
    match dt with
    | DT_logic n -> sprintf "L[%d]" n;
    | DT_int n -> sprintf "I[%d]" n;
    | DT_char -> "C8";
    | DT_bool -> "B";
    | DT_string n -> sprintf "S[%d]" n;
    | DT_object on -> sprintf "O#%s" on;
    | DT_lneg -> "(-)";
    | DT_bneg -> "(-)";
    | DT_aneg -> "(-)";
    | DT_natural n -> sprintf "N[%d]" n

  method private p_v v =
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
    | V_string s -> sprintf "\"%s\"" s;
    | V_char c -> sprintf "'%c'" c;
    | V_logic s -> s;
    | V_bool b -> if b then "true" else "false";
    | V_z -> "Z";
    | V_null -> "NULL";
    | V_freq (v,u) -> sprintf "%s %s" (Int64.to_string v) (sprint_frequnit u)  
    | V_time (v,u) -> sprintf "%s %s" (Int64.to_string v) (sprint_timeunit u)
    | V_list l -> sprintf "[%s]" (hlist (fun v -> self#p_v v) l)

  method private p_uv uv =
    let v = uv.uv_val in
    self#p_v v
  
  method private p_ot ot =
    let block dbs =
      match dbs with
      | Some db -> db.db_name;
      | None -> "noblock"
      in
    match ot with
    | OT_const co ->
        sprintf "const %s: %s" 
                co.co_name 
                (self#p_v co.co_init);
    | OT_named_value (name,v) ->
        sprintf "value %s: %s" 
                name (self#p_v v);          
    | OT_value v ->
        sprintf "value: %s" 
                (self#p_v v);
    | OT_signal co ->
        sprintf "signal %s: %s" 
                co.co_name 
                (self#p_dt co.co_type);
    | OT_reg co ->
        sprintf "register %s: %s" 
                co.co_name 
                (self#p_dt co.co_type);
    | OT_var co ->
        sprintf "variable %s: %s in %s.[%d]" 
                co.co_name 
                (self#p_dt co.co_type)
                (block co.co_block)
                co.co_index;
    | OT_channel ch ->
        sprintf "channel %s: %s" 
                ch.ch_obj.co_name 
                (self#p_dt ch.ch_obj.co_type);
    | OT_queue qu ->
        sprintf "queue %s: %s" 
                qu.qu_obj.co_name 
                (self#p_dt qu.qu_obj.co_type);
    | OT_array at ->
        let is_block = List.mem AT_block at.at_flags in
        let is_dyn = List.mem AT_dyn at.at_flags in
        let rec get_desc ot = 
          match ot  with
          | OT_signal co -> "signal",(self#p_dt co.co_type),"";
          | OT_reg co -> "register",(self#p_dt co.co_type),"";
          | OT_var co -> "variable",(self#p_dt co.co_type),sprintf " in %s" (block co.co_block);
          | OT_queue qu -> "queue",(self#p_dt qu.qu_obj.co_type),"";
          | OT_channel ch -> "channel",(self#p_dt ch.ch_obj.co_type),"";
          | OT_object ao -> 
            __(self#p_ot (OT_object ao));
            "object",sprintf "%s.%s" ao.ao_type.ta_rules.rl_name ao.ao_type.ta_name,""
          | OT_struct st -> (match List.hd st.st_objs with
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
              (self#p_ot at.at_objs.(sel)); 
        end
        else
          sprintf "arraysel %s:" 
              at.at_name;  
    | OT_object ao ->
        let aot = ao.ao_type in
        self#add_type {
          uct_name = sprintf "%s.%s" aot.ta_rules.rl_name aot.ta_name;
          uct_kind = 'A';
          uct_def = List.map (fun (meth,adl) ->
            sprintf "method %s (%s)" meth
              (self#com_l (List.map (fun ad ->
                  sprintf "%s:%s" 
                    (self#p_dt ad.arg_data_type)
                    (match ad.arg_type with 
                     | Arg_lhs -> "LHS" 
                     | Arg_rhs -> "RHS"
                     | Arg_lrhs -> "LRHS")
                     
                 ) adl));
            ) aot.ta_rules.rl_methods;
        };
        sprintf "object %s: %s.%s" 
                ao.ao_name
                ao.ao_type.ta_rules.rl_name
                ao.ao_type.ta_name;
    | OT_component st ->
        sprintf "component %s: %s" 
                st.st_name
                st.st_type.ts_name;
    | OT_struct st ->
        sprintf "structure %s: %s" 
                st.st_name
                st.st_type.ts_name;
    | _ -> "OT=?";
    
  (*
  ** 1. Operand formatter: <sign> <name> <range> [:CT=DT] 
  *)
  method private p_ud ud =
    let ct uo = 
      let uot = uo.uo_type in
      match uot.uo_conv with
      | Some dt -> sprintf ":CT=%s" (self#p_dt dt)
      | None -> "" in
    let ctt ut = 
      let uot = ut.ut_type in
      match uot.uo_conv with
      | Some dt -> sprintf ":CT=%s" (self#p_dt dt)
      | None -> "" in
    let lt uo = 
      let uot = uo.uo_type in
      match uot.uo_log_type with
      | Some dt -> sprintf ":LT=%s" (self#p_dt dt)
      | None -> "" in
    let ra uo = 
      match uo.uo_range with
      | Some (a,b) -> if a <> b then sprintf "[%d to %d]" a b else sprintf "[%d]" a
      | None -> "" in
    let rat ut = 
      match ut.ut_range with
      | Some (a,b) -> if a <> b then sprintf "[%d to %d]" a b else sprintf "[%d]" a
      | None -> "" in
    let si uo =
      if uo.uo_type.uo_sign = (Some DT_aneg) then "-" else
      if uo.uo_type.uo_sign = (Some DT_bneg) then "not " else
      if uo.uo_type.uo_sign = (Some DT_lneg) then "not " else "" in
    let sit ut =
      if ut.ut_type.uo_sign = (Some DT_aneg) then "-" else
      if ut.ut_type.uo_sign = (Some DT_bneg) then "not " else
      if ut.ut_type.uo_sign = (Some DT_lneg) then "not " else "" in
    let sel uo =
      let str = ref "" in
      List.iter (fun sel ->
	    match sel with
	    | UC_val uv -> 
		  if !str = "" then str := self#p_uv uv 
                       else str := sprintf "%s,%s" !str (self#p_uv uv);  
	    | _ ->
          if !str = "" then str := name_of_ud sel 
                       else str := sprintf "%s,%s" !str (name_of_ud sel);  
        ) uo.uo_sel;
      if !str <> "" then sprintf ".[%s]" !str else "" in
    let str =
      match ud with
      | UC_reg uo -> sprintf "%s%s%s%s" (si uo) uo.uo_name (ra uo) (ct uo);
      | UC_var uo -> sprintf "%s%s%s%s%s" (si uo) uo.uo_name (ra uo) (sel uo) (ct uo);
      | UC_sig uo -> sprintf "%s%s%s%s" (si uo) uo.uo_name (ra uo) (ct uo);
      | UC_chan uo -> sprintf "%s%s%s%s" (si uo) uo.uo_name (ra uo) (ct uo);
      | UC_queue uo -> sprintf "%s%s%s%s" (si uo) uo.uo_name (ra uo) (ct uo);
      | UC_val uv -> self#p_uv uv;
      | UC_alu ua -> sprintf "$alu.[%s]" (sprint_dt ua.ua_type);
      | UC_immed id -> sprintf "$immed.[%d]" id;
      | UC_bool id -> sprintf "$bool.[%d]" id;
      | UC_temp ut -> sprintf "%s$tmp.[%s]%s%s" (sit ut) ut.ut_name (rat ut) (ctt ut);
      | UC_array uat -> uat.uat_name;
      | UC_list ul -> 
        let str = ref "" in
        List.iter (fun da -> str := !str ^ 
                          (if !str <> "" then "@" else "")
                          ^ (self#p_ud da)) ul.ul_list;
        !str
      | UC_sel us -> 
        let p ud = self#p_ud ud in
        let data,sel = 
            let ds,ss=ref "",ref "" in
            List.iter (fun sel ->
                match sel with
                | UC_val uv ->
                let i =
                    match uv.uv_val with
                    | V_int w -> Int64.to_int w;
                    | _ -> error 1505672 "" in
                ds := if !ds = "" then (p us.us_obj) else sprintf "%s,%s" !ds (p us.us_obj);
                ss := if !ss = "" then (string_of_int i) else sprintf "%s,%d" !ss i;
                | _ -> 
                ds := if !ds = "" then (p us.us_obj) else sprintf "%s,%s" !ds (p us.us_obj);
                ss := if !ss = "" then (p sel) else sprintf "%s,%s" !ss (p sel);
              ) us.us_sel; 
            !ds, !ss 
            in
        sprintf "%s.[%s]" us.us_name sel in
    str
  (*
  ** 2. expression parameter (like ET) list.
  *)
  method private p_ep udl =
    let et uo = 
      let uot = uo.uo_type in
      sprintf "ET=%s" (self#p_dt uot.uo_expr_type)  in
    let ett ut = 
      let uot = ut.ut_type in
      sprintf "ET=%s" (self#p_dt uot.uo_expr_type) in
    let evt uv = 
      let uot = uv.uv_type in
      sprintf "ET=%s" (self#p_dt uot.uo_expr_type)  in
    (*
    ** Expression parameter list management
    *)
    let expr = ref [] in
    let ae e = if not (List.mem e !expr) && e <> "" then expr := !expr @ [e] in
    List.iter (fun ud ->
      match ud with
      | UC_reg uo -> ae (et uo);
      | UC_var uo -> ae (et uo);
      | UC_sig uo -> ae (et uo);
      | UC_chan uo -> ae (et uo);
      | UC_queue uo -> ae (et uo);
      | UC_val uv -> ae (evt uv);
      | UC_alu ua -> ();
      | UC_immed id -> ();
      | UC_bool id -> ();
      | UC_temp ut -> ae (ett ut);
      | UC_array uat -> ();
      | UC_list ul -> 
        List.iter ae (self#p_ep ul.ul_list);
      | UC_sel us -> 
        List.iter ae (self#p_ep [us.us_obj]); 
      ) udl;
    !expr

  (*
  ** MicroCode instruction formatter
  **
  **  move (dst,src)
  **  move (dst,src) with <params>
  **  expr (dst,op1,op,op2)
  **  expr (dst,op1,op,op2) with <params>
  **
  **  bind(n)
  **    
  *)
  method private p_uc uc =
    let err str = error 0 (sprintf "UCI <%s>#%s:%s" self#info "p_uc" str) in
    let pre = ref [] in
    let argstr args =
      let argstr arg =
        match arg with
        | UA_data ud -> self#p_ud ud;
        | UA_expr ul -> 
        begin
          (*
          ** Must be separated with $immed expansion
          *)
          let last = ref None in
          List.iter (fun uc' ->
                pre :=  !pre @ (self#p_uc uc');
                last := Some uc')  ul;
          match !last with
          | Some uc' ->
          begin
            match uc'.ui_code with
            | Expr (_,dst,_,_) -> self#p_ud dst;
            | _ -> err "invalid expression argument found" 
          end;
          | None -> err "empty expression found"
        end in
      let str = ref "" in
      List.iter (fun arg ->
        if !str = "" then str := argstr arg 
        else str := sprintf "%s,%s" !str (argstr arg); 
        ) args;
      !str in

    let opstr ops =
        let str = ref "" in
        List.iter (fun op ->
                if !str = "" then str := (op_name op)
                else str := sprintf "%s,%s" !str (op_name op);
            ) ops;
        if (List.length ops) > 1 then sprintf "[%s]" !str else !str in

    let locstr ul = 
        match ul with
        | UC_next -> "NEXT";
        | UC_label str -> str in


    match uc.ui_code with
    | Bind n -> 
          !pre @ [
            sprintf "%20s bind (%d)" 
                    ""
                    n;
          ];
    | Move (dst,src) -> 
          let dst' = self#p_ud dst in
          let src' = self#p_ud src in
          let pl = self#p_ep [dst;src] in
          !pre @ [
            sprintf "%20s move (%s,%s)%s" 
                    "" dst' src'
                    (if pl <> [] then sprintf " with %s" (self#and_l pl) else "")
          ];
    | Expr (ops,dst,op1,op2) ->
          let dst' = self#p_ud dst in
          let op1' = self#p_ud op1 in
          let op2' = self#p_ud op2 in
          let pl = self#p_ep [dst;op1;op2] in
          !pre @ [
            sprintf "%20s expr (%s,%s,%s,%s)%s"
                    "" dst' op1' (opstr ops) op2'
                    (if pl <> [] then sprintf " with %s" (self#and_l pl) else "")
          ];
    | Falsejump (res,ul) ->
          !pre @ [
            sprintf "%20s falsejump (%s,%s)"
                    ""
                    (self#p_ud res)
                    (locstr ul);
          ];
    | Jump ul ->
          !pre @ [
            sprintf "%20s jump (%s)"
                    ""
                    (locstr ul);
          ];
    | Label str ->
          !pre @ [
            sprintf "%20s %s"
                    (sprintf "%s:" str)
                    ""
          ];
    | Special _ -> 
          !pre @ [
            sprintf "%12s Special;" 
                    "";
          ];
    | Fun ((opl,ot),sel,args) -> 
        let rec get_ao_name ot =
          match ot with
          | OT_object ao -> ao.ao_name;
          | OT_array at ->
            if is_sel_obj opl then
              sprintf "%s.[%s]" at.at_name (cp_sprint_instr (obj_sel_obj opl))
            else 
              sprintf "%s.[%d]" at.at_name (at_index at (obj_sel opl));
          | OT_queue qu -> get_ao_name (OT_object qu.qu_ao);
          | OT_channel ch -> get_ao_name (OT_object ch.ch_ao);
          | _ -> err "unexpected function object found.";
          in
        let ao_name = get_ao_name ot in
        let args' = argstr args in
        !pre @ [
          sprintf "%20s fun %s.%s(%s)" 
                  ""
                  ao_name sel args';
        ];
    | Nop -> 
      !pre @ [
        sprintf "%20s nop"                                 
                "";
      ]
  
  (*
  **
  **  Block frame formatter
  **
  **  begin
  **    ...
  **  end with  p = v and  ... p = v
  **    
  **
  *)
  method private p_blk bf =
    []
    
  (*
  **
  *)
  val mutable compiled = false
  val mutable initialized = false

  method private compile_all =
    if not initialized then
    begin
      out (sprintf "UCI <%s>: Initializing ..." self#info);
      initialized <- true;
    end;
    if not compiled then
    begin
      out (sprintf "UCI <%s>: Compiling ..." self#info);
      compiled <- true;
    end
     


  (*
  ********************************************************
  ** Analysis and Synthesis entry point functions - API
  ********************************************************
  *)
  
  method private emit =
    let pro_name = pro.pro_name in
    let uc_name = sprintf "%s_%s.UC" (of_mod modu.mod_name) pro_name in
    out (sprintf "UCI <%s>: emitting MicroCode for process <%s>." self#info pro_name);
    self#file_open uc_name;
    let rec emit_sym sym =
        match sym with
            | Sym_obj ot ->
                self#out (sprintf "%s" (self#p_ot ot)); 
            | Sym_mod md ->
                self#out (sprintf "module %s" modu.mod_name); 
            | Sym_rule md ->
                self#out (sprintf "rules %s" modu.mod_name); 
            | Sym_pro pr ->
                self#out (sprintf "process %s" pr.pro_name); 
            | Sym_fun f ->
                let f_ret = "" in
                let f_args =
                  if f.fun_args_obj = [] then
                    self#com_l f.fun_args 
                  else
                    self#com_l (List.map2 (fun arg co ->
                      sprintf "%s:%s" arg (self#p_dt co.co_type)
                      ) f.fun_args f.fun_args_obj) in
                self#out (sprintf "function %s(%s)%s" 
                                f.fun_name f_args
                                f_ret); 
            | Sym_block bl ->
                self#out (sprintf "block %s: cells[%d] of %s" 
                        bl.db_name 
                        bl.db_size
                        (self#p_dt (DT_logic bl.db_width))); 
            | Sym_type tp ->
                let tp_name =
                    match tp with 
                    | Type_const tc -> tc.tc_name;
                    | Type_struct ts -> ts.ts_name;
                    | Type_abstract ta -> ta.ta_name;
                    | _ -> "?";
                    in
                self#out (sprintf "type %s" tp_name);
            | Sym_mon (dbg,mon) ->
                self#out "monitor:"; 
                emit_sym mon;
        in
    self#out "modules:";
    self#out "begin";
    self#ind_incr;
    self#out (sprintf "module %s: PARENT" modu.mod_name);
    List.iter (fun rl ->
      self#out (sprintf "module %s: IMPORT" rl.rl_name);
      ) modu.mod_rules;
    self#ind_decr;
    self#out "end";
    self#out "";
    

    let syms = list_of_sym pro.pro_import in
    if syms <> [] then
    begin
        self#out "import:";
        self#out "begin";
        self#ind_incr;
        List.iter emit_sym syms;
        self#ind_decr;
        self#out "end";
        self#out "";
    end;

    let syms = list_of_sym pro.pro_export in
    if syms <> [] then
    begin
        self#out "export:";
        self#out "begin";
        self#ind_incr;
        List.iter emit_sym syms;        
        self#ind_decr;
        self#out "end";
        self#out "";
    end;


    if pro.pro_temps <> [] then
    begin
        self#out "temp:";
        self#out "begin";
        self#ind_incr;
        List.iter (fun co -> self#out (self#p_ot (OT_reg co))) pro.pro_temps;
        self#ind_decr;
        self#out "end";
        self#out "";
    end;

    let syms = list_of_sym pro.pro_objs in
    if syms <> [] then
    begin
        self#out "data:";
        self#out "begin";
        self#ind_incr;
        List.iter emit_sym syms;        
        self#ind_decr;
        self#out "end";
        self#out "";
    end;

    let tpl = self#p_tp in
    if tpl <> [] then
    begin
        self#out "types:";
        self#out "begin";
        self#ind_incr;
        List.iter self#out tpl;        
        self#ind_decr;
        self#out "end";
        self#out "";
    end;
     
    
    List.iter (fun alu ->
            self#out (sprintf "alu %s: " alu.alu_name);
            self#out "begin";
            self#ind_incr;
            self#out (sprintf "type: %s" (self#p_dt alu.alu_type));
            List.iter (fun op ->
                if op <> OP_alu then
                    self#out (sprintf "op: %s" (op_name op));
                ) alu.alu_ops;
            self#ind_decr;
            self#out "end;";
            self#out "";
        ) pro.pro_alu;

    self#out "code:";
    self#out "begin";
    self#ind_incr;
    List.iter (fun uc ->
        List.iter self#out (self#p_uc uc);
        ) pro.pro_ucode;
    self#ind_decr;    
    self#out "end";
    self#file_close
          
     
  (*
  ** External access to this class object
  *)
  method uci_out_info = self#info
  method uci_out_emit = self#emit
  method uci_out_set_env = self#set_env
  method uci_out_get_env = self#get_env
  method uci_out_interp cmd =
    match cmd with
    | "compile" -> self#compile_all; "ok";
    | "init" -> "ok";
    | "emit" -> self#emit; "ok";
    | _ -> "err";
    
      

end

