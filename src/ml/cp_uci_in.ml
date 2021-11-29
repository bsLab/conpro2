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
**    $INITIAL:     (C) 2006-2009 BSSLAB
**    $CREATED:     4.11.2008
**    $VERSION:     2.20
**
**    $INFO:
**
**  Micro Code Interface - Input Class
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


type selector = 
  | Sel_val of string
  | Sel_obj of string
  | Sel_index of selector list

(*
***************************************************************
** UCI Input
***************************************************************
*)
class uci_in name tops lmap =
  object (self)
  (*_mod
  ** Compile on object instantiation
  *)
  initializer
    self#read_parameters;
    self#read_modules;
    self#read_types;
    self#read_import;
    self#read_data;
    self#read_code
    
    
    
  (*
  ** Module, object and type name
  *)
  val tname = "uci_in"
  val oname = name
  val mutable omodu = None

  method private info = sprintf "ucode.%s.%s" tname oname

  val empty_env : ((string,uci_env) Hashtbl.t) = Hashtbl.create 1
  val mutable env: ((string,uci_env) Hashtbl.t) = Hashtbl.create 100
  val mutable types: ((string, uci_type) Hashtbl.t) = Hashtbl.create 100

  val mutable opro : process option = None
 
  (*
  ************************************************************
  ** UTILS
  ************************************************************
  *)
  method private cur_pos p =
    if p <> {f_name="";f_cpos=0} then 
    begin
      an.a_curpos <- source_of_pos p;
    end
  method private get_pos =
    pos_of_source an.a_curpos

  method private modu =
    match omodu with
    | Some modu -> modu;
    | None -> error 0 (sprintf "UCI <%s>#modu: No module specified." self#info)
    
  method private pro =
    match opro with
    | Some pro -> pro
    | None -> error 0 (sprintf "UCI <%s>#modu: No process object available." self#info)

  method private get_name ut =
    match ut with
    | UT_ident (pos,str) -> 
      self#cur_pos pos;
      str
    | _ -> error 0 (sprintf "UCI <%s>#get_name: found invalid name identifier." self#info)

  method private get_param ut =
    match ut with
    | UT_param (pos,str) -> 
      self#cur_pos pos;
      str
    | _ -> error 0 (sprintf "UCI <%s>#get_param: found invalid parameter identifier." self#info)
    
  method private get_type ut =
    let v str =
      try int_of_string str with
        _ -> error 0 (sprintf "UCI <%s>#get_type: invalid number found <%s>." self#info str) in
        
    match ut with
    | UT_type ('P',UT_param (pos,str)) -> 
      self#cur_pos pos;
      DT_object str
    | UT_type ('b',UT_empty) ->
      DT_bool
    | UT_type ('c',UT_empty) ->
      DT_char
    | UT_type ('l',UT_empty) ->
      DT_logic 1
    | UT_type ('l',UT_value (kind,pos,str)) -> 
      self#cur_pos pos;
      DT_logic (v str);
    | UT_type ('i',UT_value (kind,pos,str)) -> 
      self#cur_pos pos;
      DT_int (v str);
    | UT_type ('n',UT_value (kind,pos,str)) -> 
      self#cur_pos pos;
      DT_natural (v str);
    | UT_type ('O',UT_list [UT_ident (pos1,str1);UT_ident(pos2,str2)]) ->
      self#cur_pos pos1;
      DT_object (sprintf "%s.%s" str1 str2);
    | UT_type (c,_) -> 
      error 0 (sprintf "UCI <%s>#get_type: found invalid type specifier <%c>." self#info c)
    | _ -> error 0 (sprintf "UCI <%s>#get_type: found invalid type identifier." self#info)

  method private get_val ut =
    match ut with
    | UT_value (kind,pos,str) -> 
      self#cur_pos pos;
      kind,str
    | UT_ident (pos,str) -> 
      self#cur_pos pos;
      '$',str
    | UT_type (tdc,UT_empty) -> 
      'T',sprintf "%c" tdc
    | UT_type (tdc,UT_value (_,pos,str)) ->
      self#cur_pos pos; 
      'T',sprintf "%c[%s]" tdc str    
    | _ -> error 0 (sprintf "UCI <%s>#get_val: found invalid value." self#info)


  method private get_params utl =
    let pvl = ref [] in
    List.iter (fun ut ->
      match ut with
      | UT_param_def (lhs,rhs) ->
        let p = self#get_param lhs in
        let kind,v = self#get_val rhs in
        pvl := !pvl @ [p,v];
      | _ -> error 0 (sprintf "UCI <%s>#get_params: found unexpected statement." self#info)
    
      ) utl;
    !pvl
 
  method private get_arg_desc ut =
    match ut with
    | UT_type _ -> {
        arg_label = "";
        arg_type = Arg_lrhs;
        arg_data_type = self#get_type ut;
      }
    | UT_param_def (utd,UT_param (pos,pstr)) ->
        {
          arg_label = "";
          arg_type = (
            match pstr with
            | "LHS" -> Arg_lhs;
            | "RHS" -> Arg_rhs;
            | "LRHS" -> Arg_lrhs;
            | _ -> error 0 (sprintf "UCI <%s>#get_arg_desc: found unexpected argument descriptor parameter." self#info)
          );
          arg_data_type = self#get_type utd;
        }      
    | _ -> error 0 (sprintf "UCI <%s>#get_arg_desc: found unexpected argument descriptor." self#info)
 
  method private get_selector ut =
    match ut with
    | UT_selector sl ->
      List.map (fun us -> 
        match us with
        | UT_ident (pos,str) -> 
          self#cur_pos pos;
          Sel_obj str
        | UT_list ul ->
          Sel_index (List.map (fun ui ->
            match ui with
            | UT_value _ -> let _,v= self#get_val ui in Sel_val v;
            | UT_ident _ -> let _,o= self#get_val ui in Sel_obj o;
            | _ -> error 0 (sprintf "UCI <%s>#get_selector: TODO: found unexpected selector." self#info)
            ) ul);
        | _ -> error 0 (sprintf "UCI <%s>#get_selector: found unexpected selector." self#info) 
        ) sl 
    | _ ->  error 0 (sprintf "UCI <%s>#get_selector: found no selector." self#info)  


  method private dt_of_str str =
    let err str = error 0 (sprintf "UCI <%s>#dt_of_str: %s" self#info str) in
    let int_of_str str =
      try
        int_of_string str
      with _ -> err (sprintf "Invalid type width <%s> found." str) in 
    let dt_of_s s n =
      match s with
      | "l" -> DT_logic n
      | "i" -> DT_int n
      | "n" -> DT_natural n
      | "b" -> DT_bool
      | "c" -> DT_char
      | _ -> err "Unknown data type character." in
      
    match Str.split (Str.regexp "\[\|\]") str with
    | [t;n] -> dt_of_s t (int_of_str n);
    | [t] -> dt_of_s t 0;
    | _ -> err "Unexpected string format of type."     
    
  method print_env =
    let first = ref true in
    let str = ref "" in
    let env = self#get_env in
    List.iter (fun e ->
        if !first then str := sprintf "%s=%s%s" e.uce_name (if e.uce_type = '$' then "$" else "") e.uce_val
        else str := sprintf "%s,%s=%s%s" !str e.uce_name (if e.uce_type = '$' then "$" else "") e.uce_val;
        first := false;
      ) env;
    !str

  method get_env = 
    let l = ref [] in
    Hashtbl.iter (fun name uce ->
                l := !l @ [uce];
            ) env;
    !l

  method private new_co name dt =
      {
        co_name = name;
        co_module = self#modu;
        co_rules = None;
        co_process = None;
        co_type = dt;
        co_type_name = "";
        co_level = 1;
        co_init = V_null;
        co_default = V_int Int64.zero;
        co_guard = None;    (* resolved during synthesis *)
        co_reader = [];
        co_writer = [];
        co_array = [];
        co_struct = [];
        co_index = 0;
        co_size = 1;
        co_subsize=1;
        co_block = None;
        co_flags = [];
        co_bittype = None;
        co_domain = (self#modu).mod_name;
      } 
  method private add_block block co =
    let err str = error 0 (sprintf "UCI <%s>#add_block: %s" self#info str) in
    let size = co.co_size*co.co_subsize in
    let get_width obj =
        let w = 
            match co.co_type with
            | DT_logic w -> w;
            | DT_int w -> w;
            | DT_string _ -> 8;
            | DT_char -> 8;
            | DT_bool -> 1;
            | _ -> error 332307 "";
            in
        (*
        ** Fragmented objects have reduced data width
        *)
        w / co.co_subsize
        in

    if (sym_check (self#modu).mod_objs block) then
    begin
        (*
        ** Exists already. Add object..
        *)
        match (sym_lookup (self#modu).mod_objs block) with
        | Sym_block db ->
            db.db_width <- max db.db_width (get_width co);
            db.db_size <- db.db_size + size;
            db.db_objs <- db.db_objs @ [OT_var co];
            co.co_block <- Some db;
            debug "uci_in" with (sprintf "adding object to module block <%s>[s=%d w=%d]"
                             db.db_name db.db_size db.db_width);
        | _ ->  err (sprintf "Sym_block expected, but block <%s> is something different." block);
    end
    else if sym_check (self#pro).pro_objs block then
    begin
        (*
        ** Exists already. Add object..
        *)
        match (sym_lookup (self#pro).pro_objs block) with
        | Sym_block db ->
            db.db_width <- max db.db_width (get_width co);
            db.db_size <- db.db_size + size;
            db.db_objs <- db.db_objs @ [OT_var co];
            co.co_block <- Some db;
            debug "uci_in" with  (sprintf "adding object to process block <%s> [s=%d w=%d]"
                             db.db_name db.db_size db.db_width);
        | _ -> err (sprintf 
               "Sym_block expected, but block <%s> is something different." block);
    end
    else
        err(sprintf "Datablock <%s> required, but not found." block);
    
  method get_pro = self#pro

  method private get_sym name =
    let err str = error 0 (sprintf "UCI <%s>#get_sym: %s" self#info str) in
    if sym_check (self#pro).pro_objs name then
      sym_lookup (self#pro).pro_objs name
    else if sym_check (self#modu).mod_objs name then
      sym_lookup (self#modu).mod_objs name
    else
      err (sprintf "Unknown object <%s> found." name)
  (*
  ** Create MicroCode data object from ut syntax.
  ** For expression data type, a parent (LHS) object is required
  ** in the absence of an EXPR_DT parameter (expr_params).
  *)
  method private get_obj ut expr_params ud_parent =
    let err str = error 0 (sprintf "UCI <%s>#get_obj: %s" self#info str) in
    let int_of_str str =
      try
        int_of_string str
      with _ -> err (sprintf "Invalid integer value <%s> found." str) in 
    match ut with

    | UT_immed (UT_value (kind,pos,str),ut_params) ->
      self#cur_pos pos;
      let n = int_of_str str in
      UC_immed n

    | UT_temp (UT_ident (pos,str),ut_params) ->
    begin
      self#cur_pos pos;
      let name = str in
      let sym = self#get_sym name in
      let params = expr_params @ (self#get_params ut_params) in
      
      match sym with
      | Sym_obj ot ->
      begin
        let expr_dt = 
          match List.filter (fun (p,_) -> 
                match p with
                | "EXPR_DT" -> true;
                | _ -> false) params with
          | ["EXPR_DT",v] -> self#dt_of_str v
          | _ ->
          begin 
            (*
            ** Get it from parent object if any!!!
            *)
            match ud_parent with
            | Some ud -> 
              let uot = type_of_ud ud in
              uot.uo_expr_type
            | None ->
            begin 
              (*
              ** Self reference!!!
              *)
              match dt_of_ot ot with
              | Some dt -> dt;
              | None -> err ("Can't determine expression data type anyway!");
            end;  
            end in
        
        match ot with
        | OT_reg co | OT_var co -> 
          let dt = co.co_type in
          let pt = 
            match ot with
            | OT_var co -> 
              let db = 
                match co.co_block with
                | Some db -> db;
                | None -> err (sprintf "Variable <%s> without block in temporary statement found." name); in
              Some (DT_logic db.db_width)
            | _ -> None in
            
          let et = expr_dt in
          let st = None in
          let lt = None in
          let ct = ref None in
          let s = ref None in
          List.iter (fun (p,v) ->
            match p with
            | "CT" -> ct := Some (self#dt_of_str v);
            | "SIGN" -> 
                if v = "-" then s := Some DT_aneg
                else if v = "~" then s := Some DT_lneg
                else err (sprintf "Unknown SIGN parameter value <%s> found." v);
            | _ -> ();
            ) params;
          UC_temp {
            ut_type = {
              uo_data_type = dt;
              uo_phys_type = pt;
              uo_expr_type = et;
              uo_sub_type = st;
              uo_log_type = lt;
              uo_conv = !ct;
              uo_sign = !s;
            };
            ut_range = None;
            ut_name = name;
            ut_frame = None;
            ut_obj = Some co;
            ut_flags = [];
          }
        | _ -> err (sprintf "Unsupported object type <%s> in temporary operand found" name);
      end;
      | _ -> err (sprintf "Unsupported symbol <%s> in temporary operand found" name);
    end; 
(*
    | UT_alu (UT_value (kind,pos,str),ut_params) ->
*)
    | UT_op (UT_value (kind,pos,str),ut_params) ->
    begin
      let params = expr_params @ (self#get_params ut_params) in
      self#cur_pos pos;
      match kind with
      | 'd' | 'x' -> 
        let v = V_int (Int64.of_string str) in 
        let dt = dt_of_val v in
        let ct = ref None in
        List.iter (fun (p,v) ->
          match p with
          | "CT" -> ct := Some (self#dt_of_str v);
          | _ -> ();
          ) params;
        let et = 
          match List.filter (fun (p,_) -> 
                match p with
                | "EXPR_DT" -> true;
                | _ -> false) params with
          | ["EXPR_DT",v] -> self#dt_of_str v
          | _ ->
          begin 
            (*
            ** Get it from parent object if any!!!
            *)
            match ud_parent with
            | Some ud -> 
              let uot = type_of_ud ud in
              uot.uo_expr_type
            | None ->
            begin 
              (*
              ** Self reference!!!
              *)
              dt
            end;  
            end in
        UC_val {
          uv_type = {(uo_type dt et) with uo_conv = !ct};
          uv_val = v;
        };
      | _ ->  err (sprintf "Unsupported value <%s> type found" str);
    end;
    | UT_op (UT_ident (pos,str),ut_params) ->
    begin
      (*
      ** Find object...
      *)
      self#cur_pos pos;
      let params = expr_params @ (self#get_params ut_params) in
      let name = str in
      let sym = self#get_sym name in 
      match sym with
      | Sym_obj ot ->
      begin
        match ot with
        | OT_reg _ | OT_signal _ | OT_var _ ->
          let opl = 
            List.map (fun (p,v) ->
              match p with
              | "CT" -> OD_conv (self#dt_of_str v);
              | "SIGN" -> 
                if v = "-" then OD_aneg
                else if v = "~" then OD_lneg
                else err (sprintf "Unknown SIGN parameter value <%s> found." v);
              | _ -> progerr "get_obj"
              ) (List.filter (fun (p,_) -> 
                  match p with
                  | "CT" | "SIGN" -> true;
                  | _ -> false) params) in         
          let ufl = 
            List.map (fun (p,v) ->
              match p with
              | "LHS" -> UO_lhs
              | "RHS" -> UO_rhs
              | "LOC" -> UO_loc
              | _ -> progerr "get_obj"
              ) (List.filter (fun (p,_) -> 
                  match p with
                  | "LHS" | "RHS" | "LOC" -> true;
                  | _ -> false) params) in         
          let expr_dt = 
            match List.filter (fun (p,_) -> 
                  match p with
                  | "EXPR_DT" -> true;
                  | _ -> false) params with
            | ["EXPR_DT",v] -> self#dt_of_str v
            | _ ->
            begin 
              (*
              ** Get it from parent object if any!!!
              *)
              match ud_parent with
              | Some ud -> 
                let uot = type_of_ud ud in
                uot.uo_expr_type
              | None ->
              begin 
                (*
                ** Self reference!!!
                *)
                match dt_of_ot ot with
                | Some dt -> dt;
                | None -> err ("Can't determine expression data type anyway!");
              end;  
            end in
          ud_of_ot ot opl ufl expr_dt 
        | _ -> err ("Unknown object type found.");
      end;
      | _ -> err ("Unknown symbol found.");
    end;
    | _ -> err ("Unknown object found.")

  (*
  ** Expression operator list
  *)
  method private get_opl ops expr_params =
    [ops]@(if (List.filter (fun (p,v) -> p = "ALU") expr_params) <> [] then [OP_alu] else [])
    
    
  (*
  ************************************************************
  ** parameter section (optional)
  ************************************************************
  *)
  method private read_parameters = 
    let err str = error 0 (sprintf "UCI <%s>#read_parameters: %s" self#info str) in
    let utl = List.filter (fun ut -> 
      match ut with UT_s_parameter _ -> true | _ -> false ) tops in
    if (List.length utl) > 1 then
      err (sprintf "More than one parameter section found.")
    else  if (List.length utl) > 0 then match List.hd utl with
    | UT_s_parameter utl ->
      out (sprintf "UCI <%s>: found parameter section..." self#info);
      ind_incr ();
      let ml = ref [] in
      List.iter (fun ut ->
        match ut with 
        | UT_param_def (lhs,rhs) ->
        begin
          let env_name = self#get_name lhs in
          let env_type,env_val = self#get_val rhs in
          if Hashtbl.mem env env_name then 
          begin
            let uce = Hashtbl.find env env_name in
            uce.uce_var <- true;
            if uce.uce_type <> env_type then
              err (sprintf "Expected expression type <%c>, but got <%c>." uce.uce_type env_type);
            uce.uce_val <- env_val;
          end
          else
          begin
            let env_range = None in
            Hashtbl.add env env_name {
                uce_name = env_name;
                uce_val = env_val;
                uce_type = env_type;
                uce_var = false;
                uce_array = false;
              };  
          end;
        end;
        | _ -> err "Unexpected statement in parameter section.";
        ) utl;
      out (sprintf "[%s]" self#print_env);
      ind_decr ();
    | _ ->
      err "unknown toplevel statement found."

  (*
  ************************************************************
  ** module section 
  ************************************************************
  *)
  method private read_modules = 
    let err str = error 0 (sprintf "UCI <%s>#read_modules: %s" self#info str) in
    let utl = List.filter (fun ut -> 
      match ut with UT_s_modules _ -> true | _ -> false ) tops in
    if (List.length utl) = 0 then
      err(sprintf "No modules section found.")
    else if (List.length utl) > 1 then
      err (sprintf "More than one modules section found.")
    else  match List.hd utl with
    | UT_s_modules utl ->
      out (sprintf "UCI <%s>: found modules section..." self#info);
      ind_incr ();
      let ml = ref [] in
      List.iter (fun ut ->
        match ut with 
        | UT_obj ('M',modname,modtype,_,_) ->
        begin
          let modname = self#get_name modname in
          let modname' = of_mod modname in
          let modtype = 
            match self#get_type modtype with
            | DT_object name -> name;
            | _ -> err ("Found invalid module type.") in
          match modtype with
          | "PARENT" ->
            let modu = 
              if sym_check_mod top_syms modname 
                then sym_get_mod top_syms modname
                else err (sprintf "Unknown module <%s>." modname) in
            omodu <- Some modu;
            opro <- Some {
              pro_name = name;
              pro_module = modu;
              pro_syntax = T_empty;
              pro_objs = Hashtbl.create 100;
              pro_import = Hashtbl.create 100;
              pro_export = Hashtbl.create 100;
              pro_temps = [];
              pro_instr = [];
              pro_ucode = [];
              pro_states = [];
              pro_alu = [];
              pro_constr = [];
              pro_frame = [{nilbf with bf_name="PROCESS";
                                       bf_type=BF_compound;}];
              pro_ao = 
                {
                  ao_name = name;
                  ao_module = modu;
                  ao_type = {
                      ta_name = "process";
                      ta_rules = (
                          match !process_rules with
                          | Some rl -> rl;
                          | None -> error 353394 "");
                      ta_flags = [];
                  };
                  ao_procs = [];
                  ao_array = [];
                  ao_struct = [];
                  ao_obj = None;
                  ao_flags = [];
                  ao_objs = [];
                  ao_domain = modu.mod_name;
                  ao_env = [];
                };
              pro_domain = modu.mod_name;
              pro_control = {
                pro_start = [];
                pro_stop = [];
                pro_call = [];
                pro_raise = [];
                pro_catch = [];
                };
              };


            out (sprintf "Importing module <%s> as <%s>." modname modtype);
          | "IMPORT" ->
            if sym_check_rule top_syms modname then
            begin
              let rule = sym_get_rule top_syms modname in
              if not (List.mem rule (self#modu).mod_rules) then
              begin
                out (sprintf "Importing new module <%s>." modname);
                (self#modu).mod_rules <- (self#modu).mod_rules @ [rule];
              end
              else
                out (sprintf "Opening existing module <%s>." modname);
            end
            else
              err (sprintf "Unknown module <%s> for import unavailable." modname);
          | _ -> err (sprintf "Unknown type for module <%s> found." modname);
        end;
        | _ -> err "Unexpected statement in modules section.";
        ) utl;
      ind_decr ();
    | _ ->
      err "unknown toplevel statement found."
    
  (*
  ************************************************************
  ** import section 
  ************************************************************
  *)
  method private read_import = 
    let err str = error 0 (sprintf "UCI <%s>#read_import: %s" self#info str) in
    let utl = List.filter (fun ut -> 
      match ut with UT_s_import _ -> true | _ -> false ) tops in
    if (List.length utl) > 1 then
      err (sprintf "More than one import section found.")
    else  match List.hd utl with
    | UT_s_import utl ->
      out (sprintf "UCI <%s>: found import section..." self#info);
      ind_incr ();
      let ml = ref [] in
      List.iter (fun ut ->
        match ut with 
        | UT_obj ('r',oname,otype,oparams,_) ->
        begin
          let oname = self#get_name oname in
          let otype = self#get_type otype in
          if sym_check_obj (self#modu).mod_objs oname then
          begin
            let ot = 
              let ot = sym_get_obj (self#modu).mod_objs oname in
              match ot with
              | OT_reg co -> 
                if co.co_type <> otype then
                  err (sprintf "Expected data type <%s>, but got <%s>" 
                               (ui_sprint_dt co.co_type)
                               (ui_sprint_dt otype))
                else if sym_check_obj (self#pro).pro_import oname then
                  err (sprintf "Object <%s> exists already." oname)
                else
                  ot
              | _ -> err (sprintf "Object <%s> is not a register." oname) in
            sym_add (self#pro).pro_import (Sym_obj ot);
            out (sprintf "Importing register <%s>." oname);
          end;
        end;
        | UT_obj ('s',oname,otype,oparams,_) ->
        begin
          let oname = self#get_name oname in
          let otype = self#get_type otype in
          if sym_check_obj (self#modu).mod_objs oname then
          begin
            let ot = 
              let ot = sym_get_obj (self#modu).mod_objs oname in
              match ot with
              | OT_signal co -> 
                if co.co_type <> otype then
                  err (sprintf "Expected data type <%s>, but got <%s>" 
                               (ui_sprint_dt co.co_type)
                               (ui_sprint_dt otype))
                else if sym_check_obj (self#pro).pro_import oname then
                  err (sprintf "Object <%s> exists already." oname)
                else
                  ot
              | _ -> err (sprintf "Object <%s> is not a signal." oname) in
            sym_add (self#pro).pro_import (Sym_obj ot);
            out (sprintf "Importing signal <%s>." oname);
          end;
        end;
        | UT_obj ('o',oname,otype,oparams,_) ->
        begin
          let oname = self#get_name oname in
          let otype = self#get_type otype in
          let omod = 
            match otype with
            | DT_object o -> o;
            | _ -> err (sprintf "Unexpected object type found.") in
           
          if sym_check_obj (self#modu).mod_objs oname then
          begin
            
            let ot = 
              let ot = sym_get_obj (self#modu).mod_objs oname in
              match ot with
              | OT_object ao -> 
                let omod' = sprintf "%s.%s" ao.ao_type.ta_rules.rl_name ao.ao_type.ta_name in
                if omod <> omod' then
                  err (sprintf "Expected module <%s>, but got <%s>" omod' omod)
                else if sym_check_obj (self#pro).pro_import oname then
                  err (sprintf "Object <%s.%s> exists already." omod oname)
                else
                  ot
              | _ -> err (sprintf "Object <%s> is not an abstract object." oname) in
            sym_add (self#pro).pro_import (Sym_obj ot);
            out (sprintf "Importing abstract object <%s.%s>." omod oname);
          end;
        end;
        | _ -> err "Unexpected statement in modules section.";
        ) utl;
      ind_decr ();
    | _ ->
      err "unknown toplevel statement found."

  (*
  ************************************************************
  ** data section 
  ************************************************************
  *)
  method private read_data = 
    let err str = error 0 (sprintf "UCI <%s>#read_data: %s" self#info str) in
    let utl = List.filter (fun ut -> 
      match ut with UT_s_data _ -> true | _ -> false ) tops in
    if (List.length utl) > 1 then
      err (sprintf "More than one data section found.")
    else  match List.hd utl with
    | UT_s_data utl ->
      out (sprintf "UCI <%s>: found data section..." self#info);
      ind_incr ();
      let ml = ref [] in
      List.iter (fun ut ->
        match ut with 
        | UT_obj ('r',oname,otype,oparams,_) ->
        begin
          let oname = self#get_name oname in
          let otype = self#get_type otype in
          let params = self#get_params oparams in
          let init = V_null in
          
          if sym_check_obj (self#pro).pro_objs oname then
            err (sprintf "Local object <%s> exists already." oname)
          else
          begin
            let ot = OT_reg {(self#new_co oname otype) with 
                                co_init = init;
                                co_process = Some self#pro;
                                co_flags =            
                                 (if List.mem ("guard","false") params then
                                      [Obj_no_guard] else []) @
                                 (if List.mem ("schedule","fifo") params then
                                      [Obj_schedule "fifo"] else []) }in
            sym_add (self#pro).pro_objs (Sym_obj ot);
            out (sprintf "Local register <%s> added." oname);
          end;
        end;
        | UT_obj ('s',oname,otype,oparams,_) ->
        begin
          let oname = self#get_name oname in
          let otype = self#get_type otype in
          let params = self#get_params oparams in
          let init = V_null in
          
          if sym_check_obj (self#pro).pro_objs oname then
            err (sprintf "Local object <%s> exists already." oname)
          else
          begin
            let ot = OT_signal {(self#new_co oname otype) with 
                                co_init = init;
                                co_process = Some self#pro;
                                co_flags =  [];         
                            }in
            sym_add (self#pro).pro_objs (Sym_obj ot);
            out (sprintf "Local signal <%s> added." oname);
          end;
        end;
        | UT_obj ('v',oname,otype,oparams,_) ->
        begin
          let oname = self#get_name oname in
          let otype = self#get_type otype in
          let params = self#get_params oparams in
          let blockname,params = 
            match params with
            | (name,"") :: tl -> name,tl;
            | _ -> err (sprintf "No block specified."); in
          let init = V_null in
          
          if sym_check_obj (self#pro).pro_objs oname then
            err (sprintf "Local object <%s> exists already." oname)
          else
          begin
            let co = {(self#new_co oname otype) with 
                                co_init = init;
                                co_process = Some self#pro;
                                co_flags = [];         
                                }in
            self#add_block blockname co;
            let ot = OT_var co in
            sym_add (self#pro).pro_objs (Sym_obj ot);
            out (sprintf "Local variable <%s> added." oname);
          end;
        end;
        | _ -> err "Unexpected statement in data section.";
        ) utl;
      ind_decr ();
    | _ ->
      err "unknown toplevel statement found."

  (*
  ************************************************************
  ** types section 
  ************************************************************
  *)
  method private read_types = 
    let err str = error 0 (sprintf "UCI <%s>#read_types: %s" self#info str) in
    let utl = List.filter (fun ut -> 
      match ut with UT_s_types _ -> true | _ -> false ) tops in
    if (List.length utl) > 1 then
      err (sprintf "More than one types section found.")
    else  match List.hd utl with
    | UT_s_types utl ->
      out (sprintf "UCI <%s>: found types section..." self#info);
      ind_incr ();
      let ml = ref [] in
      List.iter (fun ut ->
        match ut with 
        | UT_obj ('Y',oname,_,oels,_) ->
        begin
          (*
          ** Symbol list (symbolic enumeration)
          ** Initially: binary coding
          *)
          let oname = self#get_name oname in
          let n = List.length oels in
          let s = const_width (V_int (i64 n)) in
          let i = ref 0 in
          let ell = List.map (fun oel ->
            match oel with
            | UT_obj ('E',ename,_,_,_) ->
              incr i;
              let ename = self#get_name ename in
              let co = self#new_co ename (DT_logic s) in
              co.co_init <- (V_int (i64 !i));
              OT_const co
            | _ -> progerr "read_types";
            ) oels in
          let tp = Type_const {
              tc_name = oname;
              tc_type = DT_logic s;
              tc_elems = ell;
              tc_params = [];
              } in
          
          if sym_check_type (self#pro).pro_objs oname then
            err (sprintf "Local type <%s> exists already." oname)
          else
          begin
            sym_add (self#pro).pro_objs (Sym_type tp);
            out (sprintf "Local type <%s> added." oname);
          end;
        end;
        | UT_obj ('S',oname,_,oels,_) ->
        begin
          (*
          ** Structure list 
          *)
          let oname = self#get_name oname in
          let n = List.length oels in
          let ell = List.map (fun oel ->
            match oel with
            | UT_obj ('E',ename,etype,_,_) ->
              let ename = self#get_name ename in
              let etype = self#get_type etype in
              {
                  te_name = ename;
                  te_type = etype,1;
                  te_port = None;
              }   
            | _ -> progerr "read_types";
            ) oels in
          let ts = Type_struct{
                  ts_name = oname;
                  ts_elems = ell;
              } in
          if sym_check_type (self#pro).pro_objs oname then
            err (sprintf "Local type <%s> exists already." oname)
          else
          begin
            sym_add (self#pro).pro_objs (Sym_type ts);
            out (sprintf "Local type <%s> added." oname);
          end;
        end;
        | UT_obj ('M',onames,_,oels,_) ->
        begin
          (*
          ** Structure list 
          *)
          let onames = self#get_selector onames in
          let tm_mod,tm_type =
            match onames with
            | [Sel_obj tm_mod; Sel_obj tm_type] -> tm_mod,tm_type
            | _ ->  err "Unexpected module type name in types section found." in
            
          let n = List.length oels in
          let ell = List.map (fun oel ->
            match oel with
            | UT_obj ('E',ename,_,eargs,_) ->
              let ename = self#get_name ename in
              let eargs = List.map self#get_arg_desc eargs in
              {
                  tm_method = ename;
                  tm_method_args = eargs;
              }   
            | _ -> progerr "read_types";
            ) oels in
          let tm = Type_module {
                  tm_module = tm_mod;
                  tm_type = tm_type;
                  tm_methods = ell;
              } in
          if sym_check_type (self#pro).pro_objs oname then
            err (sprintf "Local type <%s> exists already." oname)
          else
          begin
            sym_add (self#pro).pro_objs (Sym_type tm);
            out (sprintf "Module type <%s.%s> added." tm_mod tm_type);
          end;
        end;
        | _ -> err "Unexpected statement in types section.";
        ) utl;
      ind_decr ();
    | _ ->
      err "unknown toplevel statement found."

  (*
  ************************************************************
  ** code section 
  ************************************************************
  *)
  method private read_code = 
    let err str = error 0 (sprintf "UCI <%s>#read_code: %s" self#info str) in
    let utl = List.filter (fun ut -> 
      match ut with UT_s_code _ -> true | _ -> false ) tops in
    if (List.length utl) > 1 then
      err (sprintf "More than one code section found.")
    else  match List.hd utl with
    | UT_s_code utl ->
      out (sprintf "UCI <%s>: found code section..." self#info);
      ind_incr ();
      let main_bf = {
        bf_id=1;
        bf_name="MAIN";
        bf_src_start=nilsrc ();
        bf_src_end=nilsrc ();
        bf_parent=None;
        bf_childs=[];
        bf_time=FT_0;
        bf_loop=(FT_0,FT_0);
        bf_type=BF_compound;
        bf_params=[];
        } in
      let cnt_label,cnt_nop,cnt_move,cnt_expr,cnt_jump,cnt_fjump=
          ref 0,ref 0  , ref 0  ,ref 0   ,ref 0   , ref 0 in
          
      let ucl = List.map (fun ut ->
        match ut with
        | UT_nop -> 
          incr cnt_nop;
          {
            ui_code=Nop;
            ui_frame=main_bf;
          }
        | UT_label (UT_ident (pos,str)) ->
          incr cnt_label;
          self#cur_pos pos;
          {
            ui_code=Label str;
            ui_frame=main_bf;
          }
        | UT_move (ut_lhs,ut_rhs,ut_params) ->
          incr cnt_move;
          let params = self#get_params ut_params in
          let ud_lhs = self#get_obj ut_lhs (params@["lhs","1"]) None in
          let ud_rhs = self#get_obj ut_rhs (params@["rhs","1"]) (Some ud_lhs) in
          {
            ui_code=Move (ud_lhs,ud_rhs);
            ui_frame=main_bf;
          }
        | UT_expr (ut_lhs,ut_op1,ut_op,ut_op2,ut_params) ->
          incr cnt_expr;
          let params = self#get_params ut_params in
          let ud_lhs = self#get_obj ut_lhs (params@["lhs","1"]) None in
          let ud_op1 = self#get_obj ut_op1 (params@["rhs","1"]) (Some ud_lhs) in
          let ud_op2 = self#get_obj ut_op2 (params@["rhs","1"]) (Some ud_lhs) in
          let opl = self#get_opl ut_op params in
          {
            ui_code=Expr (opl,ud_lhs,ud_op1,ud_op2);
            ui_frame=main_bf;
          }
        | _ -> 
          {
            ui_code=Nop;
            ui_frame=main_bf;
          }
        ) utl in
      out (sprintf "Found %d ucode instructions [#nop=%d #label=%d #move=%d #expr=%d #jump=%d #fjump=%d]." 
                   (List.length ucl) !cnt_nop !cnt_label !cnt_move !cnt_expr !cnt_jump !cnt_fjump);
      ind_decr ();
    | _ ->
      err "unknown toplevel statement found."
 
end
