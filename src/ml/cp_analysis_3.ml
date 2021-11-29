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
**    $VERSION:     2.13
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


(*
** Create a new object. In functions, the symbol is preceeded by
** the function name.
*)
let rec get_obj vobj = 
   (*
   ** Create or modify data_block
   *)
   let add_block block obj =
       let size = obj.co_size*obj.co_subsize in

       let get_width obj =
           let w = 
               match obj.co_type with
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
           w / obj.co_subsize
           in

       if (sym_check an.a_modu.mod_objs block) then
       begin
           (*
           ** Exists already. Add object..
           *)
           match (sym_lookup an.a_modu.mod_objs block) with
           | Sym_block db ->
               db.db_width <- max db.db_width (get_width obj);
               db.db_size <- db.db_size + size;
               db.db_objs <- db.db_objs @ [OT_var obj];
               obj.co_block <- Some db;
               debug "get_obj" with (sprintf "adding object to module block <%s>[s=%d w=%d]"
                                db.db_name db.db_size db.db_width);
           | _ -> error 0 (sprintf 
                  "\nSym_block expected, but block <%s> is something different." block);
       end
       else if an.a_pro_syms <> None && (sym_check (get_some an.a_pro_syms) block) then
       begin
           (*
           ** Exists already. Add object..
           *)
           match (sym_lookup (get_some an.a_pro_syms) block) with
           | Sym_block db ->
               db.db_width <- max db.db_width (get_width obj);
               db.db_size <- db.db_size + size;
               db.db_objs <- db.db_objs @ [OT_var obj];
               obj.co_block <- Some db;
               debug "get_obj" with (sprintf "adding object to process block <%s> [s=%d w=%d]"
                                db.db_name db.db_size db.db_width);
           | _ -> error 0 (sprintf 
                  "\nSym_block expected, but block <%s> is something different." block);
       end
       else
           error 0 (sprintf "\nDatablock <%s> required, but not found."
                          block);
       in
   let get_block obj = get_some obj.co_block in
   let get_fname v =
       if an.a_fun_name <> "" then
       sprintf "%s_%s" an.a_fun_name (get_name v)
       else get_name v in

   match vobj with
   | T_OT_const (vnamel,vtype,vinit) ->
     List.map (fun vname ->
       let name = get_fname vname in
       let defs = List.filter (fun (n,_) -> n=name) compiler.t_defs in
       
       match (get_name vtype) with
       | "value" ->
           let init = 
            match defs with
            | [_,init]  | (_,init) :: _ -> 
              get_val (T_ident (pos_of_source an.a_curpos,init));
            | [] -> 
              get_val vinit in
           OT_named_value (name,init);
       | _ -> 
           let dt,_ = get_type vtype in
           let dt_name = get_type_name vtype in
           let init = 
            match defs with
            | [_,init]  | (_,init) :: _ -> 
              get_val (T_ident (pos_of_source an.a_curpos,init));
            | [] -> 
              get_val vinit in
           OT_const {(new_obj name dt) with 
                       co_init = init;
                       co_process = an.a_pro;
                       co_type_name = dt_name;
                   }) vnamel;
   | T_OT_sig (vnamel,vtype,vinit) ->
   begin
     List.map (fun vname ->
       let name = get_fname vname in
       let dt,_ = get_type vtype in
       let dt_name = get_type_name vtype in
       let init = get_val vinit in
       match dt with
       | DT_object special ->
       begin
         (*
         ** Must be a structure type!
         *)
         let st = get_struc vobj in
         (*
         ** Publish structure element objects...
         *)
         List.iter (fun obj ->
           debug "get_obj" with (sprintf "adding structure element <%s> to symbol table"
                  (name_of_ot obj));
           if an.a_toplevel then
             sym_add an.a_modu.mod_objs (Sym_obj obj)
           else
             sym_add (get_some an.a_pro_syms) (Sym_obj obj)
           ) st.st_objs;
         OT_struct st
       end;
       | _ -> 
         OT_signal {(new_obj name dt) with 
                   co_init = init;
                   co_process = an.a_pro;
                   co_type_name = dt_name;
              };
       ) vnamel;
   end;
   | T_OT_reg (vnamel,vtype,vinit,vparams) ->
   begin
     List.map (fun vname ->
       let name = get_fname vname in
       let dt,_ = get_type vtype in
       let dt_name = get_type_name vtype in
       let init = get_val vinit in
       let params = get_param_list vparams in
       match dt with
       | DT_object special ->
       let is_bittype =
               let yes = check_tb special in
               yes in
       if not is_bittype then
       begin
         (*
         ** Must be a structure type!
         *)
         let st = get_struc (T_OT_reg ([vname],vtype,vinit,vparams)) in
         (*
         ** Publish structure element objects...
         *)
         List.iter (fun obj ->
           debug "get_obj" with (sprintf "adding structure element <%s> to symbol table"
                  (name_of_ot obj));
           if an.a_toplevel then
             sym_add an.a_modu.mod_objs (Sym_obj obj)
           else
             sym_add (get_some an.a_pro_syms) (Sym_obj obj)
           ) st.st_objs;
         OT_struct st
       end
       else
       begin
         let tb = get_tb special in
         OT_reg {(new_obj name tb.tb_dt) with 
                   co_init = init;
                   co_bittype = Some tb;
                   co_process = an.a_pro;
                   co_flags = 
                      (if List.mem ("guard","false") params then
                           [Obj_no_guard] else []);
                   

              };
       end;
       | _ -> 
         OT_reg {(new_obj name dt) with 
                   co_init = init;
                   co_process = an.a_pro;
                   co_flags =            
                      (if List.mem ("guard","false") params then
                           [Obj_no_guard] else []) @
                      (if List.mem ("schedule","fifo") params then
                           [Obj_schedule "fifo"] else []);
                   co_type_name = dt_name;
              };
     ) vnamel;
   end;
   | T_OT_chan (vnamel,vtype,vparams) ->
   begin
     List.map (fun vname ->
       let name = get_fname vname in
       let dt,_ = get_type vtype in
       let params = get_param_list vparams in
       let mo = ref Chan_unbuffered in
       List.iter (fun (n,v) ->
         match (n,v) with
         | "model","buffered" -> mo := Chan_buffered;
         | "model","unbuffered" -> mo := Chan_unbuffered;
         | _ -> error 0 (sprintf "\nUnknown channel <%s> parameter <%s=%s>!"
                                 name n v);
         ) params;
       let ao = core_abstract_object name in

       match dt with
       | DT_object special ->
           let is_bittype =
               let yes = check_tb special in
               yes in
           if not is_bittype then
           begin
             (*
             ** Must be a structure type!
             *)
             let st = get_struc (T_OT_chan ([vname],vtype,vparams)) in
             let ot = OT_channel {
                     ch_obj = new_obj name dt;
                     ch_ot = Some (OT_struct st);
                     ch_model = [!mo];
                     ch_ao = ao;
                  } in
             ao.ao_obj <- Some ot;
             ot;
           end
           else
           begin
             let tb = get_tb special in
             let ot = OT_channel {
                     ch_obj = {(new_obj name tb.tb_dt) with 
                           co_bittype = Some tb;
                           co_process = an.a_pro};
                     ch_ot = None;
                     ch_model = [!mo];
                     ch_ao = ao;
                  } in
             ao.ao_obj <- Some ot;
             ot;
           end;
       | _ ->    
           let ot = OT_channel {
                   ch_obj = {(new_obj name dt) with
                     co_flags =            
                      (if List.mem ("guard","false") params then
                           [Obj_no_guard] else []) @
                      (if List.mem ("schedule","fifo") params then
                           [Obj_schedule "fifo"] else []);
                       };
                   ch_ot = None;
                   ch_model = [!mo];
                   ch_ao = ao;
                   } in
           ao.ao_obj <- Some ot;
           ot
     ) vnamel;
   end;
   | T_OT_que (vnamel,vtype,vparams) ->
   begin
     List.map (fun vname ->
       let name = get_fname vname in
       let dt,_ = get_type vtype in
       let params = get_param_list vparams in
       let depth = ref 8 in
       List.iter (fun (n,v) ->
         match n with
         | "depth" -> depth := get_int_of_str v;
         | _ -> error 0 (sprintf "\nUnknown queue <%s> parameter <%s=%s>!"
                                 name n v);
         ) params;
       let ao = core_abstract_object name in

       match dt with
       | DT_object special ->
           let is_bittype =
               let yes = check_tb special in
               yes in
           if not is_bittype then
           begin
             (*
             ** Must be a structure type!
             *)
             let st = get_struc (T_OT_que ([vname],vtype,vparams)) in
             let ot = OT_queue {
                     qu_obj = new_obj name dt;
                     qu_ot = Some (OT_struct st);
                     qu_depth = !depth;
                     qu_ao = ao;
                  } in
             ao.ao_obj <- Some ot;
             ot;
           end
           else
           begin
             let tb = get_tb special in
             let ot = OT_queue {
                     qu_obj = {(new_obj name tb.tb_dt) with 
                           co_bittype = Some tb;
                           co_process = an.a_pro;
                           co_flags =            
                            (if List.mem ("guard","false") params then
                               [Obj_no_guard] else []) @
                            (if List.mem ("schedule","fifo") params then
                               [Obj_schedule "fifo"] else []);
                       };
                     qu_ot = None;
                     qu_depth = !depth;
                     qu_ao = ao;
                  } in
             ao.ao_obj <- Some ot;
             ot;
           end;
       | _ ->    
           let ot = OT_queue {
                 qu_obj = {(new_obj name dt) with
                           co_flags =            
                            (if List.mem ("guard","false") params then
                               [Obj_no_guard] else []) @
                            (if List.mem ("schedule","fifo") params then
                               [Obj_schedule "fifo"] else []);

                           };
                 qu_ot = None;
                 qu_depth = !depth;
                 qu_ao = ao;
              } in
           ao.ao_obj <- Some ot;
           ot;
     ) vnamel;
   end;
   | T_OT_var (vnamel,vtype,vblock,vinit,vparams) ->
   begin
     List.map (fun vname ->
       let name = get_fname vname in
       let dt,div = get_type vtype in
       let dt_name = get_type_name vtype in
       let block = get_name vblock in 
       let init = get_val vinit in
       let params = get_param_list vparams in
       match dt with
       | DT_object special ->
       begin
         (*
         ** Must be a structure type!
         *)
         let st = get_struc vobj in
         (*
         ** Publish structure element objects...
         *)
         List.iter (fun obj ->
           debug "get_obj"  with (sprintf "adding structure element <%s> to symbol table"
                  (name_of_ot obj));

           let co =
               match co_of_ot obj with
               | Some co -> co;
               | None -> error 256490 "";
               in

           add_block block co;
           let db = get_block co in
           co.co_index <- db.db_size - 1;

           if an.a_toplevel then
             sym_add an.a_modu.mod_objs (Sym_obj obj)
           else
             sym_add (get_some an.a_pro_syms) (Sym_obj obj)
           ) st.st_objs;
         OT_struct st
       end;
       | _ ->
           let ob = {(new_obj name dt) with 
                   co_init = init;
                   co_subsize = div;
                   co_process = an.a_pro;
                   co_type_name = dt_name;
                }in
           add_block block ob;
           let db = get_block ob in
           ob.co_index <- db.db_size - 1;
           OT_var ob;
     ) vnamel;
   end;
   | T_OT_array (vnamel,vkind,vobjrange,vtype,vblock,vparams) ->
   let objs = ref [] in
   let params = get_param_list vparams in
   let paramenv = get_param_env vparams in
   
   List.iter (fun vname -> objs := !objs @ (
     let name = get_fname vname in
     (*
     ** Array of Abstract Object ? 
     *)
     let is_obj,
         is_struc,
         is_comp,
         is_pro = 
           (vkind = 'o'),
           (vkind = ' ' && 
             (
                   let dt,_ = get_type vtype  in 
                   match dt with
                   | DT_object oname -> true;
                   | _ -> false;
             )
           ),
           (vkind = 'c'),
           (
               match vobjrange with
               | T_sub (T_proc,_) -> true;
               | _ -> false;
           ) in   
     (*
     ** Array of structure?
     *)
     let get_ts () = 
           let dt,_ = get_type vtype  in 
           match dt with
           | DT_object oname ->
               let tname = get_name vtype in
               let ts = get_ts tname in
               ts
           | _ -> error 884529 "" in

     let is_obj = is_obj or is_pro in

     if not is_obj && not is_struc && not is_comp then
     begin 
       let block = get_name vblock in
       let dt,div = get_type vtype in
       let dt_name = get_type_name vtype in
       let width = size_of_dt dt in
       let at = 
           match vobjrange with 
           | T_sub (vot,vsize) ->
           begin
             let sizes = 
               match vsize with
               | T_OP_arith _ -> error 0 (sprintf "Found unexpected expression in range specifier in array <%s>. Use constants instead." name);
               | T_ident _ -> [get_int vsize]
               | T_list vl -> List.map (fun vsize' -> 
                                   match vsize' with
                                   | T_ident _ -> get_int vsize';
                                   | T_OP_arith _ -> error 0 (sprintf "Found unexpected expression in range specifier in array <%s>. Use constants instead." name);
                                   | _ -> error 0 (sprintf "Found unexpected object in range specifier in array <%s>. Use constants instead." name);
                                   ) vl;
               | _ -> error 0 (sprintf "Found unexpected object in range specifier in array <%s>. Use constants instead." name) in
             let size = 
               let size = ref 1 in
               List.iter (fun si -> size := !size * si) sizes;
               !size in

             debug "get_obj" with (
               let sizestr= ref "" in
               List.iter (fun s -> sizestr := !sizestr ^ " " ^
                                   (string_of_int s)) sizes;
               sprintf "creating array: width=%d size=%s"
                            width !sizestr);

             match vot with
             | T_reg ->
               (*
               ** Create N different objects!
               *)
               let at = {
                   at_name = name;
                   at_dim = Array.of_list sizes;
                   at_objs = [||];
                   at_flags=[];
                   at_ao = core_abstract_object name;
                 } in
               at.at_objs <- (Array.init size (fun n ->
                     let name' = sprintf "%s_%d" name n in
                     OT_reg {(new_obj name' dt) with 
                             co_init = V_null;
                             co_index = n;
                             co_process = an.a_pro;
                             co_array = [at];
                             co_type_name = dt_name;
                         }
                     ));
               at.at_ao.ao_array <- [at];
               at
             | T_sig ->
               (*
               ** Create N different objects!
               *)
               let at = {
                   at_name = name;
                   at_dim = Array.of_list sizes;
                   at_objs = [||]; 
                   at_flags=[];
                   at_ao = core_abstract_object name;
                 } in
               at.at_objs <- (Array.init size (fun n ->
                     let name' = sprintf "%s_%d" name n in
                     OT_signal {(new_obj name' dt) with 
                             co_init = V_null;
                             co_index = n;
                             co_process = an.a_pro;
                             co_array = [at];
                             co_type_name = dt_name;
                         }
                     ));
               at.at_ao.ao_array <- [at];
               at
             | T_var ->
               let at = {
                   at_name = name;
                   at_dim = Array.of_list sizes;
                   at_objs = [||];
                   at_flags=[AT_block];
                   at_ao = core_abstract_object name;
                 } in
               at.at_ao.ao_array <- [at];
               let co = {(new_obj name dt) with 
                   co_init = V_null;
                   co_array = [at];
                   co_size = size;
                   co_process = an.a_pro;
                   co_type_name = dt_name;
                  } in
               at.at_objs <- [|OT_var co|];
               let block = 
                 if block <> "" then block
                 else 
                 begin
                   let block = sprintf "var_array_%s" name in
                   (*
                   ** Create new data_block!
                   *)
                   let emi = open_module "Ram" in
                   let core_rules = get_some !core_rules in
                   let obj = emi#new_obj an.a_modu block 
                              (if an.a_pro <> None then
                                ["LOCAL",ENV_int (Int64.one)] else []) in
                   let db = {
                       db_name=block;
                       db_rules = Some {core_rules with rl_child=Some obj};
                       db_width=width;
                       db_size=0;
                       db_objs=[];
                       db_params=[Mp_outline;Mp_singleport;Mp_readsync];
                       db_domain=an.a_modu.mod_name;
					   db_flags=[];
                   } in
                   if an.a_toplevel then
                     sym_add an.a_modu.mod_objs (Sym_block db)
                   else
                     sym_add (get_some an.a_pro_syms) (Sym_block db); 
                   block
                 end;
                 in
               add_block block co;
               at;
             | T_chan ->
               (*
               ** Create N different objects!
               *)
               let mo = ref Chan_unbuffered in
               List.iter (fun (n,v) ->
                   if n = "model" then
                   begin
                     match v with
                     | "buffered" -> mo := Chan_buffered;
                     | "unbuffered" -> mo := Chan_unbuffered;
                     | _ -> error 0 (sprintf "Unexpected model parameter <%s> found."
                                             v);
                   end;
                   ) params;
               let at = {
                   at_name = name;
                   at_dim = Array.of_list sizes;
                   at_objs = [||];
                   at_flags=[];
                   at_ao = core_abstract_object name;
                 } in
               at.at_objs <- (Array.init size (fun n ->
                     let name' = sprintf "%s_%d" name n in
                     let ao' = core_abstract_object name' in
                     OT_channel {
                         ch_obj = 
                             {(new_obj name' dt) with
                               co_init = V_null;
                               co_index = n;
                               co_process = an.a_pro;
                               co_array = [at]};
                         ch_ot = None;
                         ch_model = [!mo];      
                         ch_ao = ao';      
                         }
                     ));
               at.at_ao.ao_array <- [at];
               at
             | T_que ->
               (*
               ** Create N different objects!
               *)
               let depth = ref 8 in
               List.iter (fun (n,v) ->
                   match n with
                   | "depth" -> depth := get_int_of_str v;
                   | _ -> error 0 (sprintf 
                                     "Unexpected parameter <%s=%s> found."
                                     n v);
                   ) params;
               let at = {
                   at_name = name;
                   at_dim = Array.of_list sizes;
                   at_objs = [||];
                   at_flags=[];
                   at_ao = core_abstract_object name;
                 } in
               at.at_objs <- (Array.init size (fun n ->
                     let name' = sprintf "%s_%d" name n in
                     let ao' = core_abstract_object name' in
                     OT_queue {
                         qu_obj = 
                             {(new_obj name' dt) with
                               co_init = V_null;
                               co_index = n;
                               co_process = an.a_pro;
                               co_array = [at]};
                         qu_ot = None;
                         qu_depth = !depth;
                         qu_ao = ao';      
                         }
                     ));
               at.at_ao.ao_array <- [at];
               at
             | _ -> error 0 "Array: invalid object type or size.";
           end;
           | _ -> error 0 "Array: invalid object type or size.";
           in
       [OT_array at];
     end
     else if is_obj then
     begin
       let check_ao ot =
           match ot with
           | OT_object ao ->
           begin
             match ao.ao_type.ta_name with
             | "process" -> ();
             | _ when (ao.ao_type.ta_rules.rl_interp "array?") <> "ok" 
                  -> error 0
                         (sprintf "Abstract object <%s> can't be stored in (dynamic selected) array."
                                  ao.ao_name);
             | _ -> ()
           end;
           | _ -> error 880743 ""; in

       let is_ao_pro ot =
           match ot with
           | OT_object ao ->
           begin
               match ao.ao_type.ta_name with
               | "process" -> true;
               | _ -> false;
           end;
           | _ -> error 880744 ""; in

       let is_static ot =
           (*
           ** Static selection required?
           *)
           match ot with
           | OT_object ao ->
           begin
               match ao.ao_type.ta_name with
               | "process" -> false
               | _ -> (ao.ao_type.ta_rules.rl_interp "array?") <> "ok" ;
           end;
           | _ -> error 880745 ""; in


       let at,otl = 
           match vobjrange with 
           | T_sub (vot,vsize) ->
           begin

             let size = get_int vsize in
             let oname = get_name vot in
             debug "get_obj" with (sprintf "creating array: abstract object <%s> size=%d"
                            oname size);
             let at_flags = ref [] in
             let otl = ref [] in

             (*
             ** Create N different objects!
             *)
             let at = {
                   at_name = name;
                   at_dim = [|size|];
                   at_objs = [||];
                   at_flags=[];
                   at_ao = core_abstract_object name;
                 } in
             at.at_objs <- (Array.init size (fun n ->
                     let name' = sprintf "%s_%d" name n in
                     let tot = 
                       T_OT_object ([T_ident (pos_of_source an.a_curpos,
                                     name'
                                     )],
                                     vot,vparams) in
                     let ot = List.hd (get_obj tot) in

                     if is_static ot then
                       otl := !otl @ [ot];

                     if is_static ot && not (List.mem AT_static !at_flags)
                       then at_flags := !at_flags @ [AT_static;AT_temp];

                     if not (is_static ot) then
                       check_ao ot;

                     let aoo = 
                       match ot with
                       | OT_object ao -> 
                         ao.ao_array <- [at];
                         Some ao;
                       | _ -> None in

                     if is_ao_pro ot then
                       an.a_procs_to_compile <- an.a_procs_to_compile @
                           [n,name',vtype,aoo];

                     if not (is_ao_pro ot) then
                     begin
                       match aoo with
                       | Some ao -> 
                       begin
                         ao.ao_flags <- ao.ao_flags @ params;
                       end;
                       | None -> error 971231 "";                          
                     end;
                     
                     ot
                     ));
             at.at_ao.ao_array <- [at];
             at.at_flags <- !at_flags;
             Array.iter (fun ot ->
                   match ot with
                   | OT_object ao -> ao.ao_array <- [at];
                   | _ -> error 472856 ""; 
                   ) at.at_objs;
             at, !otl
           end;
           | _ -> error 0 "\ninvalid abstract object in array";
           in
       [OT_array at]@otl
     end
     else if is_struc then
     begin

       let ts = get_ts () in
       let block = get_name vblock in
       let arrays = 
           match vobjrange with 
           | T_sub (vot,vsize) ->
           begin
             let size = 
              try
                  get_int vsize 
              with _ -> error 0 (sprintf "Can't create multi-dimensional arrays of structures.") in
             debug "get_obj" with (sprintf "creating structure array <%s>: size=%d"
                            name size);
             let is_block = match vot with T_var -> true | _ -> false in
             let at_struc, st_struc =
               let st_struc = {
                               st_name=name;
                               st_type=ts;
                               st_objs=[];
                               st_connect = [];
                               st_array=None} in
               (*
               ** Temporary root array
               *)
               let at_struc = {
                   at_name = name;
                   at_dim = [|size|];
                   at_objs = [|OT_struct st_struc|];
                   at_flags=[AT_temp]@(if is_block then [AT_block] else []);
                   at_ao = core_abstract_object name;
               } in
               at_struc,st_struc in
             let atl = ref [OT_array at_struc] in 
             match vot with
             | T_reg ->
               (*
               ** Create N different arrays, one for each
               ** structure element!
               *)
               List.iter (fun te ->
                 let te_name = te.te_name in
                 let te_dt,_ = te.te_type in
                 let at = {
                   at_name = sprintf "%s_%s" name te_name;
                   at_dim = [|size|];
                   at_objs = [||];
                   at_flags=[];
                   at_ao = core_abstract_object name;
                 } in
                 at.at_objs <- (Array.init size (fun n ->
                     let name' = sprintf "%s_%s_%d" 
                                         name te_name n in
                     OT_reg {(new_obj name' te_dt) with 
                             co_init = V_null;
                             co_index = n;
                             co_process = an.a_pro;
                             co_array = [at;at_struc];
                             co_struct = [st_struc];
                         }
                     ));
                 at.at_ao.ao_array <- [at];
                 st_struc.st_objs <- st_struc.st_objs @ [OT_array at];
                 atl := !atl @ [OT_array at];
                ) ts.ts_elems;
               !atl
             | T_var ->
               (*
               ** Create N different arrays, one for each
               ** structure element!
               *)
               List.iter (fun te ->
                 let te_name = te.te_name in
                 let te_dt,_ = te.te_type in
                 let width = size_of_dt te_dt in
                 let at = {
                   at_name = sprintf "%s_%s" name te_name;
                   at_dim = [|size|];
                   at_objs = [||];
                   at_flags=[AT_block];
                   at_ao = core_abstract_object name;
                 } in
                 at.at_ao.ao_array <- [at];
                 let co = {(new_obj at.at_name te_dt) with 
                   co_init = V_null;
                   co_size = size;
                   co_process = an.a_pro;
                   co_array = [at;at_struc];
                   co_struct = [st_struc];
                  } in
                 at.at_objs <- [|OT_var co|];
                 st_struc.st_objs <- st_struc.st_objs @ [OT_array at];
                 let block = 
                   if block <> "" then block
                   else 
                   begin
                       let block = sprintf "var_array_%s" name in
                       (*
                       ** Create new data_block!
                       *)
                       let emi = open_module "Ram" in
                       let core_rules = get_some !core_rules in
                       let obj = emi#new_obj an.a_modu block 
                        ((if an.a_pro <> None then
                                ["LOCAL",ENV_int (Int64.one)] else [])
                          @(filter_env "Ram" paramenv)) in
                       let db = {
                           db_name=block;
                           db_rules = Some {core_rules with rl_child=Some obj};
                           db_width=width;
                           db_size=0;
                           db_objs=[];
                           db_params=[Mp_outline;Mp_singleport;Mp_readsync];
                           db_domain=an.a_modu.mod_name;
						   db_flags=[];
                       } in
                       if an.a_toplevel && not (sym_check an.a_modu.mod_objs block) then
                         sym_add an.a_modu.mod_objs (Sym_block db)
                       else if not an.a_toplevel && not (sym_check (get_some an.a_pro_syms) block) then
                         sym_add (get_some an.a_pro_syms) (Sym_block db); 
                       block
                     end;
                     in
                 add_block block co;    
                 atl := !atl @ [OT_array at];
                ) ts.ts_elems;
               !atl
             | _ -> error 0 "Array: unexpected object type or range.";
           end;
           | _ -> error 0 "Array: unexpected object type or range.";
           in
       arrays;
     end   
     else
     begin
       (*
       ** Component array -- only static selection!
       *)
       let otl = 
           match vobjrange with 
           | T_sub (vot,vsize) ->
           begin

             let size = get_int vsize in
             let oname = get_name vot in
             debug "get_obj" with (sprintf "creating array: component <%s> size=%d"
                            oname size);
             let otl = ref [] in

             (*
             ** Create N different objects!
             *)
             let at = {
                   at_name = name;
                   at_dim = [|size|];
                   at_objs = Array.init size (fun n ->
                     let name' = sprintf "%s_%d" name n in
                     let tot = 
                       T_OT_comp ([T_ident (pos_of_source an.a_curpos,
                                     name'
                                     )],
                                     vot,[]) in
                     let ot = List.hd (get_obj tot) in
                     ot
                     );
                   at_flags=[AT_static;AT_temp];
                   at_ao = core_abstract_object name;
                 } in
             at.at_ao.ao_array <- [at];
             Array.iter (fun ot ->
                   match ot with
                   | OT_component st -> 
                     otl := !otl @ [ot];
                     st.st_array <- Some at;
                   | _ -> error 472855 ""; 
                   ) at.at_objs;
             [OT_array at] @ !otl;
           end;
           | _ -> error 0 "\ninvalid component in array";
           in
       otl;

     end;
     )) vnamel;
   !objs;

   | T_OT_comp (vnamel,vtype,vinit) ->
   begin   
     List.map (fun vname ->
       let name = get_fname vname in
       let dt,_ = get_type vtype in
       let vobj = T_OT_comp ([vname],vtype,vinit) in
       match dt with
       | DT_object special ->
       begin
        (*
        ** Must be a structure type!
        *)
        let st = get_struc vobj in
        (*
        ** Publish structure element objects...
        *)
        List.iter (fun obj ->
           debug "get_obj" with (sprintf "adding component element <%s> to symbol table"
                  (name_of_ot obj));
           if an.a_toplevel then
             sym_add an.a_modu.mod_objs (Sym_obj obj)
           else
             sym_add (get_some an.a_pro_syms) (Sym_obj obj)
           ) st.st_objs;
        (*
        ** Imported module? (only architecture module context)
        *)
        if sym_check_mod an.a_modu.mod_import special then
        begin
          out_ (sprintf "Instantiating module <%s> as component <%s>..."
                        special name);
        end;
        OT_component st
       end;
       | _ -> 
         error 0 (sprintf "component <%s> must be of structure type."
                        name);
     ) vnamel;
   end;
   | T_OT_object (tnamel,ttype,tparams) ->
     List.map (fun tname ->
       let oname = get_fname tname in
       let otype = get_name ttype in
       let ta =
           if sym_check_type an.a_modu.mod_objs otype then
           begin
               match sym_get_type an.a_modu.mod_objs otype with
               | Type_abstract ta -> ta;
               | _ -> error 0 (sprintf "\nunknown abstract type <%s>" otype);
           end
           else
               error 0 (sprintf "\nunknown abstract type <%s>" otype);
           in
       let params = get_param_list tparams in
       let paramenv = get_param_env tparams in
       (*
       ** Object parameters with module selector <Objmod.parname>.
       *)
       let obj_params = List.map (fun (n,v) ->
          match Str.split (Str.regexp "\.") n with
          | [m;n] -> 
            n,v
          | _ -> progerr "Objmod.parname?";   
         ) (List.filter (fun (n,v) ->
         String.contains n '.'
         ) params) in
        
       let params = if obj_params <> [] then obj_params else params in
       let paramenv = if obj_params <> [] then filter_env ta.ta_rules.rl_name paramenv else paramenv in
       
       let ta' = 
         (*
         ** Derive new object rules from root rules...
         *)
         let rules = ta.ta_rules.rl_new_obj an.a_modu oname 
                                (paramenv@(if an.a_pro <> None then
                                  ["LOCAL",ENV_int (Int64.one)] else [])) in
         try
           List.find (fun ta -> ta.ta_name = otype) rules.rl_types
         with
           _ -> error 0 (sprintf "Can't find type <%s> in rule sets for object <%s>." otype oname) in

       let ao = {
               ao_name = oname;
               ao_type = ta';
               ao_module = an.a_modu;
               ao_procs = [];
               ao_array = [];
               ao_struct = [];
               ao_obj = None;
               ao_flags = ta'.ta_flags;
               ao_objs = [];
               ao_domain = an.a_modu.mod_name;
               ao_env = params;
           } in
       ao.ao_flags <- ao.ao_flags @ params;
       OT_object ao;
     ) tnamel;
   | _ -> error 911375 ""


