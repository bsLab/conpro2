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
**    $CREATED:     5.3.2006
**    $VERSION:     2.09
**
**    $INFO:
**
**  Analysis part of compiling: syntax and structure compiling.
**
**    $ENDOFINFO
**
*)
open Cp_syntax
open Cp_types
open Cp_symbol
open Cp_common
open Cp_utils
open Cp_data
open Cp_expr
open Cp_emi

open Unix
open Printf

open Cp_analysis_1
open Cp_analysis_2
open Cp_analysis_3

(*
** Adds an object to the current process and procedure
** local symbol table.
*)
let pro_sym_add ob =
        let ob_name = get_obj_name ob in
        let symtab = get_some an.a_pro_syms in
        if sym_check symtab ob_name then
                error 0 (sprintf 
                       "\nSymbol <%s> appears more then once in process <%s>."
                       ob_name an.a_pro_name);
        sym_add symtab (Sym_obj ob)

let fun_sym_add ob =
        let ob_name = get_obj_name ob in
        let symtab = get_some an.a_fun_syms in
        if sym_check symtab ob_name then
                error 0 (sprintf 
                       "\nSymbol <%s> appears more then once in procedure <%s>."
                       ob_name an.a_fun_name);
        sym_add symtab (Sym_obj ob)

(*
** Searches for an object symbol in the local process context...
*)
let pro_sym_check ob_name =
      match an.a_pro_syms with
      | Some symtab ->
        sym_check_obj symtab ob_name
      | None -> false

let pro_sym_lookup ob_name =
        let symtab = get_some an.a_pro_syms in
        sym_get_obj symtab ob_name

(*
** Extract typedefintion ...
*)
let get_typedef tp_name el =
        (*
        ** Initial settings for constants - updated later
        *)
        let c_val = T_ident ({f_name="";f_cpos=0},"0") in
        let c_dt = T_ident ({f_name="";f_cpos=0},"logic") in


        let oc_list = ref [] in
        let os_list = ref [] in
        let ob_list = ref [] in

        let rec check_kind el kind =       
            let rec this_kind e = 
                match e with    
                | T_instr e -> this_kind e;
                | T_OT (enl,dl)  ->
                    let en = List.hd enl in
                    let is_bittype =
                        (List.length dl) = 1 &&
                        (
                            match List.hd dl with
                            | T_ident (pos,str) ->
                                let ok = protects (__(int_of_string str)) in
                                ok;
                            | T_interval (a,b) ->
                                true;
                            | _ -> false;
                        ) in
                    let get_bitrange () =
                            match List.hd dl with
                            | T_ident (pos,str) ->
                                let w = ref 0 in
                                let ok = protects (w := int_of_string str) in
                                (!w, !w);
                            | T_interval (a,b) ->
                                let a = get_int a in
                                let b = get_int b in
                                a,b;
                            | _ -> error 415750 "" in

                    if not is_bittype then
                    begin
                      os_list := !os_list @ [
                        if (List.length dl) = 1 then {
                            te_name = get_name en;
                            te_type = get_type (List.hd dl);
                            te_port = None;
                        }
                        else if (List.length dl) = 2 then {
                            te_name = get_name en;
                            te_type = get_type (List.nth dl 1); 
                            te_port = Some (get_port (List.hd dl));
                        }
                        else
                            error 0 "\ninvalid element in type definition.";
                      ];
                      'S';
                    end
                    else
                    begin
                      ob_list := !ob_list @ [
                            {
                            tr_name = get_name en;
                            tr_range = get_bitrange ();
                            }];
                      'B';
                    end;
                | T_ident id -> 
                    let obj = List.hd 
                                (get_obj (T_OT_const ([e],c_dt,c_val))) in
                    assign_rule obj;
                    oc_list := !oc_list @ [obj];
                    'C';
                | _ -> error 0 "\nunexpected element in type definition.";
                in
            match el with
            | hd::tl ->
                let this = this_kind hd in
                if kind <>  '?' && kind <> this then
                    error 0 "\ninconsistent type definition.";
                    
                check_kind tl this
            | [] -> kind;
            in
        match check_kind el '?' with
        | 'C' ->
        begin
            (*
            ** Initially: binary coding
            *)
            let n = List.length !oc_list in
            let s = const_width (V_int (i64 n)) in
            let tp = Type_const {
                tc_name = tp_name;
                tc_type = DT_logic s;
                tc_elems = !oc_list;
                tc_params = [];
                } in
            (*
            ** Update constant objects...
            *)
            let i = ref 1 in
            List.iter (fun ot ->
                match ot with
                | OT_const co ->
                    co.co_init <- (V_int (i64 !i));
                    co.co_type_name <- tp_name;
                    incr i;
                | _ -> error 411058 "";
                ) !oc_list;
            debug "get_typedef" with (sprintf "added constant type <%s>" tp_name);
            if an.a_toplevel then
                sym_add an.a_modu.mod_objs (Sym_type tp)
            else
                sym_add (get_some an.a_pro_syms) (Sym_type tp);
            !oc_list
        end;
        | 'S' ->
            let ts = {
                    ts_name = tp_name;
                    ts_elems = !os_list;
                } in
            let tp = Type_struct ts in
            debug "get_typedef" with (sprintf "added structure type <%s>" tp_name);
            if an.a_toplevel then
                sym_add an.a_modu.mod_objs (Sym_type tp)
            else
                sym_add (get_some an.a_pro_syms) (Sym_type tp);
            []
        | 'B' ->
            let dt =
                let a = ref 9999999 in
                let b = ref 0 in
                List.iter (fun tr ->
                    let a',b' = tr.tr_range in
                    a := min !a a';
                    b := max !b b';
                    ) !ob_list;
                DT_logic (!b - !a + 1) in
            let tb = {
                    tb_name = tp_name;
                    tb_elems = !ob_list;
                    tb_dt = dt;
                } in
            let tp = Type_bit tb in
            debug "get_typedef" with (sprintf "added bit type <%s>" tp_name);
            if an.a_toplevel then
                sym_add an.a_modu.mod_objs (Sym_type tp)
            else
                sym_add (get_some an.a_pro_syms) (Sym_type tp);
            [];
        | _ -> error 0 "\nunsupported type definition."

      
let get_param name pl =
      let rec find_name pl =
            match pl with
            | (name',value')::tl ->
              if name' = name then Some value' else find_name tl;
            | [] -> None in
      find_name pl


(*
** Extract all objects and type definitions from syntax tree. Either global toplevel ones OR
** local ones inside a process.
*)
let rec get_objs synt =
  let objs = ref [] in
  let rec iter s =
      match s with
      | T_OT_const _
      | T_OT_sig _ 
      | T_OT_reg _
      | T_OT_var _
      | T_OT_chan _
      | T_OT_que _
      | T_OT_comp _
      | T_OT_object _ ->
          let obl = get_obj s in
          List.iter (fun ob ->
              assign_rule ob;
              let ob_name = get_obj_name ob in
              (*
              ** Smybol belongs either to the module
              ** or to the process environment.
              *)
              if an.a_fun_syms <> None then
              begin
                  debug "get_objs" with (sprintf "get_objs: adding <%s> to procedure symbol table <%s>"
                                   ob_name an.a_fun_name);
                  fun_sym_add ob;
              end
              else if an.a_toplevel then
              begin
                  debug "get_objs" with (sprintf "get_objs: adding <%s> to module symbol table <%s>"
                                   ob_name an.a_mname);
                  sym_add an.a_modu.mod_objs (Sym_obj ob);
              end
              else
              begin
                  debug "get_objs" with (sprintf "get_objs: adding <%s> to process symbol table <%s>"
                                   ob_name an.a_pro_name);
                  pro_sym_add ob;
              end;

              objs := !objs @ [ob];
          ) obl;
      | T_OT_array _ ->
          let obl = get_obj s in
          List.iter (fun ob ->
              assign_rule ob;
              let ob_name = get_obj_name ob in
              debug "get_objs" with (sprintf "get_objs: ob_name=<%s>" ob_name);
              let at_objs =
                match ob with
                | OT_array at ->
                  if (Array.length at.at_objs) > 1 then
                    Array.to_list at.at_objs
                  else
                    []
                | OT_component st ->
                    [ob];
                | OT_object ao ->
                    [ob];
                | _ -> error 648641 "";
                in

              (*
              ** Smybol belongs either to the module
              ** or to the process environment.
              *)

              if an.a_fun_syms <> None then
              begin
                  debug "get_objs" with (sprintf "get_objs: adding <%s> to procedure symbol table <%s>"
                                   ob_name an.a_fun_name);
                  fun_sym_add ob;
              end
              else if an.a_toplevel then
              begin
                  debug "get_objs" with (sprintf "get_objs: adding <%s> to module symbol table <%s>"
                                   ob_name an.a_mname);
                  sym_add an.a_modu.mod_objs (Sym_obj ob);
              end
              else
              begin
                  debug "get_objs" with (sprintf "get_objs: adding <%s> to process symbol table <%s>"
                                   ob_name an.a_pro_name);
                  pro_sym_add ob;
              end;
              objs := !objs @ [ob];
          ) obl;
      | T_typedef (vt,vl,vfl) ->
      begin
          (*
          ** Either constant or structure type definition
          *)
          let tp_name = get_name vt in
          match vl with
          | T_list li ->  
              let obl = get_typedef tp_name li in
              if vfl <> T_empty then
                set_param tp_name vfl;
              (*
              ** Smybol belongs either to the module
              ** or to the process environment.
              *)
              List.iter (fun ob ->
                  let ob_name = get_obj_name ob in
                  if an.a_fun_syms <> None then
                  begin
                      debug "get_objs" with (sprintf "get_objs: adding <%s> to procedure symbol table <%s>"
                                       ob_name an.a_fun_name);
                      fun_sym_add ob;
                  end
                  else if an.a_toplevel then
                  begin
                      debug "get_objs" with (sprintf "get_objs: adding <%s> to module symbol table <%s>"
                                   ob_name an.a_mname);
                      sym_add an.a_modu.mod_objs (Sym_obj ob);
                  end
                  else
                  begin
                      debug "get_objs" with (sprintf "get_objs: adding <%s> to process symbol table <%s>"
                                       ob_name an.a_pro_name);
                      pro_sym_add ob;
                  end;
                ) obl;
              objs := !objs @ obl;
          | _ -> error 0 "\nexpected T_list in T_typedef.";
      end;
      | T_exception vt ->
          let tp_name = get_name vt in
          an.a_exceptions <- an.a_exceptions + 1;
          let tp = Type_exc {
              tx_name = tp_name;
              tx_id = an.a_exceptions;
            } in
          sym_add an.a_modu.mod_objs (Sym_type tp);
      | T_data_block (vb,ve) ->
      begin
          let block = get_name vb in

          debug "get_objs" with (sprintf "adding DB <%s>" block);

          if an.a_toplevel && (sym_check an.a_modu.mod_objs block) then
              error 0 (sprintf "\nDatablock <%s> exists already (module)." block);
          if not an.a_toplevel && (sym_check (get_some an.a_pro_syms) block) then
              error 0 (sprintf "\nDatablock <%s> exists already (process)." block);

          (*
          ** Create new data_block!
          *)
          let emi = open_module "Ram" in
          let core_rules = get_some !core_rules in
          let paramenv = get_param_env ve in
          let ramenv = filter_env "Ram" paramenv in
          let port = 
            match List.filter (fun (n,e) -> n = "arch") ramenv with
            | [] -> Mp_singleport;
            | ["arch",ENV_str "singleport"] -> Mp_singleport;
            | ["arch",ENV_str "dualport"] -> Mp_dualport;
            | ["arch",_] -> error 0 (sprintf "Unexpected value of Ram parameter <arch> found.");
            | _ -> progerr "filter" in
            
          let obj = emi#new_obj an.a_modu block ramenv in
          let db = {
                  db_name=block;
                  db_rules = Some {core_rules with rl_child=Some obj};
                  db_width=0;
                  db_size=0;
                  db_objs=[];
                  db_params=[port];
                  db_domain=an.a_modu.mod_name;
				  db_flags=[];
              } in
          if an.a_fun_syms <> None then
              sym_add (get_some an.a_fun_syms) (Sym_block db)
          else if an.a_toplevel then
              sym_add an.a_modu.mod_objs (Sym_block db)
          else
              sym_add (get_some an.a_pro_syms) (Sym_block db); 
      end;

      (*
      ** Go deeper inside tree, but stop at process barrier.
      *)
      | T_topinstr i -> iter i;
      | T_instr i -> iter i;
      | T_block (i,_) -> iter i;
      | T_list li -> List.iter (fun i -> iter i) li;


      | T_module md ->
          let mdname = get_name md in
          (*
          ** Rule base modules...
          *)
          let internal = add_module mdname in
          if not internal then
          begin
            let mod' = open_module mdname in
            ()
          end;
      (*
      ** User function definition
      *)
      | T_Fun_def (fname,frets,fargs,finstr) ->
      begin
        let args = ref [] in
        let ndts = ref [] in
        let fname = get_name fname in
        let fnamepos = an.a_curpos in
        List.iter (fun va ->
          match va with
          | T_ident _ -> args := !args @ [get_name va];
          | T_OT_reg (vname,vtype,_,_) ->
              args := !args @ [get_name (List.hd vname)];
              let n = get_name (List.hd vname) in
              ndts := !ndts @ [an.a_curpos,n,vtype];
          | _ -> error 0 (sprintf "Unexpected function argument found in function definition <%s>."
                                  fname);
          ) fargs;
        let arg_objs =
          List.map (fun (pos,n,vtype) ->
              let vname = T_ident (pos_of_source pos,
                                      sprintf "ARG_FUN_%s_%s" fname n) in
              let rp = T_empty in
              match get_obj (T_OT_reg ([vname],vtype,T_empty,rp)) with
              | [OT_reg co] -> 
                co.co_rules <- !core_rules;
                co.co_flags <- co.co_flags @ [Obj_no_guard];
                co;
              | _ -> error 0 (sprintf "Unexpected function argument <%s> in <%s> found."
                                      n fname);
              ) !ndts in
        let ret_objs = List.map (fun fret ->
          match fret with
          | T_OT_reg (vname,vtype,_,_) ->
              let n = get_name (List.hd vname) in
              let vname = T_ident (pos_of_source an.a_curpos,
                                      sprintf "RET_FUN_%s_%s" fname n) in
              let rp = T_empty in
              begin match get_obj (T_OT_reg ([vname],vtype,T_empty,rp)) with
              | [OT_reg co] -> 
                co.co_rules <- !core_rules;
                co.co_flags <- co.co_flags @ [Obj_no_guard];
                co;
              | _ -> error 0 (sprintf "Unexpected function argument <%s> in <%s> found." n fname)
              end;
          | _ -> error 0 (sprintf "Unexpected return argument found in function <%s>." fname)) frets in
        let f = {
            fun_name = fname;
            fun_inline = false;
            fun_args = !args;
            fun_ret = [];
            fun_instr = finstr;
            fun_objs = Hashtbl.create 100;
            fun_args_obj = arg_objs;
            fun_ret_obj = ret_objs;
          } in
        let rets = List.map (fun fret ->
          match fret with
          | T_ident (fpos,name) -> name;
          | T_OT_reg (fname,_,_,_) -> get_name (List.hd fname); 
          | _ -> error 0 (sprintf "Unexpected return argument found in function <%s>." fname)) frets in
        f.fun_ret <- rets;

        let ftype = if f.fun_ret = [] then "procedure" else "function" in
        let fun_vparams = ref None in
        let rec get_instr_list syn =
          match syn with
          | T_block (syn,vparams) -> 
              let params = get_param_list (match vparams with Some vpe -> vpe| None -> T_empty) in
              f.fun_inline <-  (List.mem ("inline","true") params) ||
                               (List.mem ("inline","") params);
              fun_vparams := vparams;
              get_instr_list syn;
          | T_list li -> li
          | _ -> error 0 (sprintf
                      "\nMissing instruction list in procedure definition <%s>."
                      f.fun_name) in
        let il = get_instr_list finstr in
        if (compiler.t_ml || compiler.t_C) && f.fun_inline then
        begin
          let pname = sprintf "FUN_%s" fname in
          let args_desc =
            (List.map  (fun co ->
              {
                arg_label=co.co_name;
                arg_type=Arg_lhs;
                arg_data_type=co.co_type;
              }
              ) f.fun_ret_obj) @
            (List.map  (fun co ->
              {
                arg_label=co.co_name;
                arg_type=Arg_rhs;
                arg_data_type=co.co_type;
              }
              ) f.fun_args_obj) in
          let rec rules = {
            rl_name = "Function";
            rl_my = (fun sym ->
              match sym with
              | Sym_obj obj ->
              begin
                match obj with
                | OT_object ao -> ao.ao_type.ta_name = "function" 
                | _ -> false;
              end;
              | _ -> false);
            rl_obj_port = (fun sym modu pro  -> [],[]);
            rl_obj_map = (fun sym modu pro -> []);
            rl_obj_decl = (fun sym modu pro  -> [],[]);
            rl_obj_code = (fun sym modu pro -> []);
            rl_instr_ucode = (fun instr id pro -> []);
            rl_types = [{
                   ta_name = "function";
                   ta_rules = rules; 
                   ta_flags = [];
               }];
            rl_methods = [
              "call",args_desc;
              ];
            rl_fun_compile = (fun modu pro instr top -> ());
            rl_fun_scode = (fun instr label next modu pro -> []);
            rl_top_vcode = (fun instr modu pro -> [],[]);
            rl_time = (fun modu proo f -> FT_min 1);
            rl_new_obj = (fun _ _ _ -> rules);
            rl_interp = (fun _ -> "");
            rl_child=None;
          } in
          let ot = OT_object {
            ao_name = sprintf "function_%s" f.fun_name;
            ao_module = an.a_modu;
            ao_type = {
                ta_name = "function";
                ta_rules = rules;
                ta_flags = [];
                };
            ao_procs = [];
            ao_array = [];
            ao_struct = [];
            ao_obj = None;
            ao_flags = [];
            ao_objs = [];
            ao_domain = "";
            ao_env = [];
            } in
          let finstr = T_block (T_list il, !fun_vparams) in
          let pinstr = T_process (T_ident (pos_of_source fnamepos,pname),
                                           finstr) in  
          f.fun_instr <- pinstr;
          if an.a_toplevel then
          begin
            sym_add an.a_modu.mod_objs (Sym_fun f);
            sym_add an.a_modu.mod_objs (Sym_obj ot);
            List.iter (fun co ->
              sym_add an.a_modu.mod_objs (Sym_obj (OT_reg co))
              ) f.fun_args_obj;
            List.iter (fun co -> 
              sym_add an.a_modu.mod_objs (Sym_obj (OT_reg co))
              ) f.fun_ret_obj;
          end
          else
          begin
            sym_add (get_some an.a_pro_syms) (Sym_fun f);
            sym_add (get_some an.a_pro_syms) (Sym_obj ot);
            List.iter (fun co ->
              sym_add (get_some an.a_pro_syms) (Sym_obj (OT_reg co))
              ) f.fun_args_obj;
            List.iter (fun co -> 
               sym_add (get_some an.a_pro_syms) (Sym_obj (OT_reg co))
              ) f.fun_ret_obj;
          end;
        end
        else if f.fun_inline then 
        begin
            an.a_fun_name <- f.fun_name;
            an.a_fun_syms <- Some f.fun_objs;
            out (sprintf "Found inlinie %s macro <%s>." 
                           ftype f.fun_name);
            match frets with
            | [] ->
                out (sprintf "Analyzing procedure <%s>..." f.fun_name);
                List.iter (fun instr -> __(get_objs instr)) il;
                let n = Cp_symbol.length f.fun_objs in
                out (sprintf "  Found %d local object(s)." n);
                if an.a_toplevel then
                  sym_add an.a_modu.mod_objs (Sym_fun f)
                else
                  sym_add (get_some an.a_pro_syms) (Sym_fun f);
                an.a_fun_syms <- None;
                an.a_fun_name <- "";
            | _ ->
                out (sprintf "Analyzing function <%s>..." f.fun_name);
                List.iter (fun instr -> __(get_objs instr)) il;
                let n = Cp_symbol.length f.fun_objs in
                out (sprintf "  Found %d local object(s)." n);
                if an.a_toplevel then
                  sym_add an.a_modu.mod_objs (Sym_fun f)
                else
                  sym_add (get_some an.a_pro_syms) (Sym_fun f);
                an.a_fun_syms <- None;
                an.a_fun_name <- "";        
        end
        else 
        begin
          out (sprintf "Found shared %s block <%s> with %d parameter(s)."
                       ftype f.fun_name
                       (List.length f.fun_args_obj));
          (*
          ** Create function block process.
          *)
          let pname = sprintf "FUN_%s" fname in
          let lockname = sprintf "LOCK_FUN_%s" fname in
          let fhead = [] in
          let ftail = [] in
          let finstr = T_block (T_list (fhead @ il @ ftail), !fun_vparams) in
          let pinstr = T_process (T_ident (pos_of_source fnamepos,pname),
                                           finstr) in  
          f.fun_instr <- pinstr;

          if (add_module "Mutex") = false then
          begin
            __(open_module "Mutex");
            __(add_module "Mutex");
          end;
          let fl = get_objs  (T_OT_object ([T_ident (pos_of_source fnamepos,
                                                     lockname)], 
                                           T_ident (pos_of_source fnamepos,
                                                   "mutex"), 
                                           T_empty)) in


          (*
          ** Add function and function parameters.
          *)
          if an.a_toplevel then
          begin
              sym_add an.a_modu.mod_objs (Sym_fun f);
              List.iter (fun co ->
                sym_add an.a_modu.mod_objs (Sym_obj (OT_reg co))
                ) f.fun_args_obj;
              List.iter (fun co ->
                sym_add an.a_modu.mod_objs (Sym_obj (OT_reg co));
                ) f.fun_ret_obj ;
          end
          else
          begin
              sym_add (get_some an.a_pro_syms) (Sym_fun f);
              List.iter (fun co ->
                sym_add (get_some an.a_pro_syms) (Sym_obj (OT_reg co))
                ) f.fun_args_obj;
              List.iter (fun co -> 
                sym_add (get_some an.a_pro_syms) (Sym_obj (OT_reg co));
                ) f.fun_ret_obj;
          end;
        end;
      end;
      (*
      ** Stop.
      *)
      | _ -> ();
      in
  iter synt;
  !objs 

(*
** Evaluate object range expression
**
**  1. <x>
**  2. <x>..<y>
*)
let rec get_sub ra =
      match ra with
      | T_ident _ ->
      begin
        (*
        ** Just one number!
        *)
        let a = get_int ra in
        a,a
      end;
      | T_interval (a,b) ->
      begin
        let a = get_int a in
        let b = get_int b in
        a,b        
      end;
      | T_sub (_,ra) -> get_sub ra;
      | T_id -> an.a_pro_num,an.a_pro_num;
      | _ -> error 0 "Unexpected subrange expression found."



(*
** Do sanity checks for a bounded instruction block, mainly
** data dependencies...
*)
let check_bind pi =
  let objs = ref [] in
  let add_lhs pi =
        match pi with
        | PI_obj (opl,ot) ->
        begin
            let name = name_of_pi pi in
            let auxl = 
                if is_sel_obj opl then
                begin
                  let so = obj_sel_obj opl in
                  let rec iter so =
                    match so with
                    | PI_list li ->
                        List.map (fun obj' ->
                            name_of_pi obj';
                            ) li; 
                    | PI_obj (opl,ot) -> 
                    begin
                      match ot with
                      | OT_const _ | OT_value _ -> []
                      | _ ->
                        [name_of_pi so];
                    end;
                    | PI_arithm (_,op1,op2) ->
                      (iter op1) @ (iter op2);
                    | _ -> error 781939 "Unexpected object in object selector found." in
                  iter so
                end
                else if is_sel opl then
                begin
                    let il = obj_sel opl in
                    let str = ref "" in
                    Array.iter (fun i -> str := !str ^ "_" ^
                                                       (string_of_int i)) il;
                    [!str]
                end
                else [] in
            if (List.mem name !objs)  then
              warning (sprintf "\nTarget <%s> appears more than once in bounded block." name)
            else
                objs := !objs @ [name] @ auxl;
        end;
        | _ -> error 0 "\nUnexpected target in bounded block instruction."
        in
  let check_dep pi =
        match pi with
        | PI_obj (opl,_) ->
        begin
            let name = name_of_pi pi in
            let aux =
                if is_sel_obj opl then
                begin
                    let so = obj_sel_obj opl in
                    let rec iter so =
                      match so with
                      | PI_list li ->
                          List.map name_of_pi li;
                      | PI_obj _ -> [name_of_pi so];
                      | PI_arithm (_,op1,op2) -> 
                        (iter op1)@(iter op2) 
                      | _ -> error 0 (sprintf "Unexpected instruction in object selector in bounded block found.") in
                    iter so
                end
                else [] in
            let ext = 
                if is_sel opl then
                begin
                    let il = obj_sel opl in
                    let str = ref "" in
                    Array.iter (fun i -> str := !str ^ "_" ^
                                                       (string_of_int i)) il;
                    !str
                end
                else "" in
          if List.mem (name^ext) !objs then
            warning_  (sprintf "Dependency conflict for object <%s> in bounded block found [%s]."
                              (name^ext) (
                                let s = ref "" in
                                List.iter (fun n -> s := sprintf "%s%s%s"
                                  !s 
                                  (if !s <> "" then " " else "")
                                  n) !objs;
                                !s
                              ));
          List.iter (fun aux -> 
            if List.mem ext !objs then
              warning_  (sprintf 
                "Dependency conflict for auxilliary object <%s> in bounded block found [%s]."
                aux
                (
                   let s = ref "" in
                   List.iter (fun n -> s := sprintf "%s%s%s"
                     !s 
                     (if !s <> "" then " " else "")
                     n) !objs;
                   !s
                ))) aux;
        end;
        | _ -> ()
        in    
  let rec walk pi =
        match pi with
        | PI_block (il,bf) when (List.mem BP_bind bf.bf_params)->
          let pl = bf.bf_params in
          let rec iter il =
            match il with
            | ins::tl ->
            begin
              match ins with
              | PI_assign (source,dst,src) ->
                line source;
                walk (PI_block ([src],bf));
                add_lhs dst;
                check_dep src;
                iter tl;
              | PI_arithm (_,op1,op2) -> 
                check_dep op1;
                check_dep op2;
                walk (PI_block ([op1;op2],bf));
                iter tl;
              | PI_bool (_,_,op1,op2) ->
                check_dep op1;
                check_dep op2;
                walk (PI_block ([op1;op2],bf));
                iter tl;
              | PI_obj _  -> iter tl;
              | PI_fun (src,(opl,ot),sel,argl) -> 
                 let lookup_ot oname =
                   if sym_check_obj an.a_modu.mod_objs oname then
                     sym_get_obj an.a_modu.mod_objs oname
                   else if an.a_pro <> None && 
                           sym_check_obj (get_some an.a_pro).pro_objs oname then
                     sym_get_obj (get_some an.a_pro).pro_objs oname 
                   else 
                     error 0 (sprintf 
                       "Undefined abstract object <%s> in method call <%s> found." 
                       oname sel) in
                let oname = name_of_ot ot in
                let sname = sel in
                let ot = lookup_ot oname in
                let rec get_ao ot =
                  match ot with
                  | OT_object ao -> ao
                  | OT_array at -> get_ao at.at_objs.(0)
                  | _ -> error 0 (sprintf "No abstract object found for <%s>." oname) in
                let ao = get_ao ot in
                let at = ao.ao_type in
                let (_,parl) = 
                  try
                    List.find (fun (sel',_) -> sel = sel') at.ta_rules.rl_methods 
                  with _ -> error 0 (sprintf "Method <%s> not found for object <%s>."
                                             sname oname) in
                if (List.length parl) <> (List.length argl) then
                  error 0 (sprintf "Expected %d arguments, but found %d."
                                   (List.length parl) (List.length argl));
                
                iter tl;
              | PI_block (il',_) -> 
                iter il';
                iter tl;
              | _ -> error 0 "\nUnexpected instruction in bounded block.";
            end;
            | [] -> ()
            in
          iter il;      
        | _ -> error 676684 "";
        in
        walk pi
      
(*
** Lookup object...
*)
let get_obj_of_ident instr =
    let name = get_name instr in
    if check_const name then
        PI_obj ([],get_const instr)
    else
    begin
        let tmp_names = List.map (fun co -> co.co_name) 
                                            !(an.a_pro_temps) in
        (*  
        ** First check all local symbols...
        *)
        if (pro_sym_check name) then
        begin
            let obj = pro_sym_lookup name in
            PI_obj ([],obj);
        end
        else if List.mem name tmp_names then
        begin
            let co = List.find (fun co -> co.co_name = name) 
                                        !(an.a_pro_temps) in
            PI_obj ([],OT_reg co)
        end 
        else
        begin
            (*
            ** Module level
            *)
            if (sym_check_obj an.a_modu.mod_objs name) then
            begin
                let obj = sym_get_obj an.a_modu.mod_objs name in
                PI_obj ([],obj);
            end
            else
            begin
                (*
                ** External modules (included external modules)
                *)
                if sym_check_obj_ext an.a_modu name then
                    PI_obj ([],(sym_get_obj_ext an.a_modu name))
                else
                begin

                  (*
                  ** Maybe an exception ...
                  *)
                  if sym_check_type an.a_modu.mod_objs name then
                  begin
                    match sym_get_type an.a_modu.mod_objs name with
                    | Type_exc ex -> PI_obj ([],OT_value (V_int (Int64.of_int ex.tx_id)));
                    | _ ->    
                      error 0 (sprintf
                               "Unknown symbol <%s> found."
                               name);
                  end
                  else
                    error 0 (sprintf
                               "Unknown symbol <%s> found."
                               name);
                end;
            end;
        end;
    end


(*
** Evaluate function calls in assignments and boolean expressions. 
*)
let fun_eval instr =
 let aux = ref [] in
 let temps = ref [] in

 let replace = ref false in
 let rec fun_eval instr =
  let rec contains_fun instr =
    match instr with
    | T_OP_arith (op,op1,op2) ->
        (contains_fun op1) || (contains_fun op2)
    | T_OP_bool (kind,op,op1,op2) ->
        (contains_fun op1) || (contains_fun op2)
    | T_Fun _ -> true;
    | _ -> false in
  let fun_eval_check instr =
    if contains_fun instr then   
    begin
        replace := true;
        let instr' = fun_eval instr in  
        replace := false;
        instr'
    end
    else fun_eval instr in

  match instr with
  | T_ident _ -> instr;
  | T_concat (op1,op2) ->
    T_concat ((fun_eval  op1),(fun_eval  op2));
  | T_assign (lhs,rhs) -> 
    if not (contains_fun rhs) then
    begin
        let lhs' = fun_eval lhs in
        let rhs' = fun_eval rhs in
        T_assign (lhs',rhs')
    end
    else
    begin
      match rhs with
      | T_Fun (name,args,T_empty) -> 
        T_Fun (name,args,lhs);
      | _ -> 
        replace := true;
        let instr' = fun_eval rhs in
        replace := false;
        T_assign (lhs,instr');
    end;
  | T_typeconv (c,op) ->
    T_typeconv (c,(fun_eval  op));
  | T_map (lhs,rhs) ->
    T_map ((fun_eval  lhs),(fun_eval  rhs));
  | T_OP_arith (op,op1,op2) ->
    T_OP_arith (op,(fun_eval  op1),(fun_eval  op2));
  | T_OP_bool (kind,op,op1,op2) ->
    T_OP_bool (kind,op,(fun_eval  op1),(fun_eval  op2));
  | T_waitfor (w1,w2,w3,w4) ->
    T_waitfor ((fun_eval_check w1),
               (fun_eval_check w2),
               (fun_eval_check w3),
               (fun_eval_check w4));
  | T_time (ident,uni) ->
    T_time ((fun_eval  ident),uni);
  | T_freq (ident,uni) ->
    T_freq ((fun_eval  ident),uni);
  | T_branch (b1,b2,b3) ->
    T_branch (fun_eval_check b1,
              fun_eval b2,
              fun_eval b3)
  | T_forloop (f1,dir,f2,f3,f4) ->
    T_forloop ((fun_eval_check f1),
                dir,
               (fun_eval_check f2),
               (fun_eval_check f3),
               (fun_eval f4));
  | T_loop (kind,l1,l2) ->
    T_loop (kind, 
            (fun_eval_check l1),
            (fun_eval l2));
  | T_select (s1,s2) ->
    T_select ((fun_eval  s1),
              (fun_eval  s2));
  | T_case (c1l,c2) ->
    T_case (List.map (fun_eval ) c1l,
            (fun_eval  c2));
  | T_block (b1,bo) ->
    T_block ((fun_eval  b1),
             bo);
  | T_bind b ->
    T_bind (fun_eval  b);
  | T_instr i ->
    T_instr (fun_eval  i);
  | T_list li ->
    T_list (List.map (fun_eval ) li);
  | T_sub (s1,s2) ->
    T_sub ((fun_eval  s1),
           (fun_eval  s2));
  | T_interval (i1,i2) ->
    T_interval ((fun_eval  i1),
                (fun_eval  i2));
  | T_selector (s1,s2) ->
    T_selector ((fun_eval  s1),
                (fun_eval  s2));
  | T_Fun (f1,fl2,fret) -> 
    let f' = T_Fun ((fun_eval  f1),
               List.map (fun_eval ) fl2,
               (fun_eval fret)) in
    let fname = get_name f1 in
    if !replace then
    begin
      let name = get_name fret in
      let resl = ref [] in
      let m = an.a_modu in
      let f = 
        if sym_check_fun m.mod_objs fname then
          sym_get_fun m.mod_objs fname 
        else 
          error 0 (sprintf "Unknown function <%s> found." fname) in        
      List.iter (fun co ->
        let dt = co.co_type in
        let tmp = new_tmp' an.a_pro_temps dt in 
        temps := !temps @ [tmp];
        resl := !resl @ [T_ident (pos_of_source an.a_curpos,tmp.co_name)];
        ) f.fun_ret_obj;
      let res = 
        match !resl with
        | [res] -> res;
        | [] -> error 0 (sprintf "Found illegal procedure <%s> call in expression (missing return value)." fname); 
        | resl -> T_objlist resl; in
        
      aux := !aux @ [T_Fun (f1,fl2,res)];
      res
    end
    else
        f';
  | _ -> instr 
  in
 let instr' = fun_eval instr in
 List.iter (fun co ->
    co.co_flags <- List.filter (fun f ->
                                f <> Obj_inuse
                            ) co.co_flags;
    match an.a_pro with
    | Some pro -> 
        co.co_reader <- [pro];
        co.co_writer <- [pro];
        co.co_rules <- !core_rules;
    | None -> ();
    ) !temps;
 !aux @ [instr']

                                                                 
