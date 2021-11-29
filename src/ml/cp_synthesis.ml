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
**    $INITIAL:     (C) 2006-2014 BSSLAB
**    $CREATED:     1.3.2006
**    $VERSION:     2.07
**
**    $INFO:
**
**  CONPRO synthesis of analyzed modules.
**
**
**  The design is splitted into a control and a data path for each process.
**  Each process forms a separate VHDL entity and file. 
**  All processes are included in a separate module VHDL entity:
**
**  Hierarchy:
**
**          M main
**      ---------------------
**      |   |   |   |   |
**      P1  P2  P3  M1  M2 ...
**  
**
**
** Analysis and Synthesis path:
**
**      1. Lexer and Parser -> syntax
**      2. Module analysis -> module
**      3. Module synthesis
**      4. CONPRO Instructions -> MicroCode
**      5. MicroCode -> States
**
**    $ENDOFINFO
**
*)

open Cp_version
open Cp_symbol
open Cp_types
open Cp_utils
open Cp_refstack
open Cp_analysis
open Cp_Core
open Cp_process
open Cp_module
open Cp_ucode
open Cp_alu
open Printf
open Cp_common
open Cp_block_frame
open Cp_optimize
open Cp_stat


(*
** Resolve all object guards.
*)

let resolve_guards main =
    an.a_curpos <- nilsrc();
    let guards = ref 0 in
    out "Resolving object guards...";        
    ind_incr ();
    let guard_it syms =
        List.iter (fun sym ->
          match sym with
          | Sym_obj obj -> 
          begin
            let rec obj_guard obj =
              match obj with
              | OT_channel ch ->
                    let co = ch.ch_obj in
                    co.co_guard <- Some {
                            gd_name = "guard_"^
                                      co.co_module.mod_name^
                                      "_"^
                                      co.co_name;
                            gd_procs = co.co_writer@co.co_reader;
                            gd_req = [GD_rd;GD_wr];
                        };
                    incr guards;
              | OT_queue qu ->
                    let co = qu.qu_obj in
                    co.co_guard <- Some {
                            gd_name = "guard_"^
                                      co.co_module.mod_name^
                                      "_"^
                                      co.co_name;
                            gd_procs = co.co_writer@co.co_reader;
                            gd_req = [GD_rd;GD_wr];
                        };
                    incr guards;
              | OT_reg co ->
                let r = List.length co.co_reader in
                let w = List.length co.co_writer in
                let x = 
                    let b = ref false in
                    let d = co.co_domain in
                    List.iter (fun pro' ->
                        b := !b or (pro'.pro_domain <> d);
                        ) co.co_reader;
                    List.iter (fun pro' ->
                        b := !b or (pro'.pro_domain <> d);
                        ) co.co_writer;
                    !b in
                if (co.co_guard = None && w > 1 || x) &&
                   not (List.mem Obj_no_guard co.co_flags) then
                begin
                    co.co_guard <- Some {
                            gd_name = "guard_"^
                                      co.co_module.mod_name^
                                      "_"^
                                      co.co_name;
                            (*
                            ** Only write access must be guarded!
                            *)
                            gd_procs = co.co_writer;
                            gd_req = [GD_wr];
                        };
                    incr guards;
                end;
              | OT_var co ->
                let r = List.length co.co_reader in
                let w = List.length co.co_writer in
                let x = 
                    let b = ref false in
                    let d = 
                        match co.co_block with
                        | Some db -> db.db_domain;
                        | None -> error 846777 "" in
                    List.iter (fun pro' ->
                        b := !b or (pro'.pro_domain <> d);
                        ) co.co_reader;
                    List.iter (fun pro' ->
                        b := !b or (pro'.pro_domain <> d);
                        ) co.co_writer;
                    !b in
                if co.co_guard = None then
                begin
                    co.co_guard <- Some {
                            gd_name = "guard_"^
                                      co.co_module.mod_name^
                                      "_"^
                                      co.co_name;
                            (*
                            ** Both read and write access must be guarded!
                            *)
                            gd_procs = (
                                let rl = co.co_reader in
                                let wl = co.co_writer in
                                let pl = ref [] in
                                List.iter (fun r ->
                                        if not (List.mem r !pl) then
                                            pl := !pl @ [r];
                                    ) rl;
                                List.iter (fun w ->
                                        if not (List.mem w !pl) then
                                            pl := !pl @ [w];
                                    ) wl;
                                !pl
                            );
                            gd_req = [GD_rd;GD_wr];
                        };
                    incr guards;
                end;
              | OT_array at ->
                Array.iter (fun ot -> obj_guard ot) at.at_objs;
              | _ -> ();
              in
            obj_guard obj;
          end;
          | _ -> ();
          ) syms;
    in
    (*
    ** Examine first all objects from the main module.
    *)
    guard_it (list_of_sym main.mod_objs);  
    guard_it (list_of_sym main.mod_export);  
    
    let rec iter mll =
        List.iter (fun ml ->
            guard_it (list_of_sym ml.mod_objs);
            iter ml.mod_external;
            ) mll;
        in
    (*
    ** Now the included module tree.
    *)
    iter main.mod_external;
    out (sprintf "Found %d guard(s)." !guards);
    ind_decr ()

    
(*
** Resolve RAM block access, assign addresses of OT_var objects.
*)
let resolve_ram main =
    an.a_curpos <- nilsrc();
    out "Resolving RAM blocks...";
    ind_incr ();
    let rams = ref [] in
    let find_ram pro syml =
      List.iter (fun sym ->
          match sym with
          | Sym_block db ->
              rams := !rams @ [db];
          | _ -> ();
        ) syml in

    let find_import pro syml =
      List.iter (fun sym ->
          match sym with
          | Sym_obj obj ->
          begin
            match pro with
            | Some pro ->
            begin
              (*
              ** Import Sym_block if Sym_obj was imported, too.
              *)
              let rec import obj =
                match obj with
                | OT_var co ->
                  if sym_check_obj pro.pro_import co.co_name then
                  begin
                    let db =
                      match co.co_block with
                      | Some db -> db;
                      | None -> error 796397 "Cp_synthesis.resolve_ram: OT_vat but no DB";
                      in
                    if not (sym_check_sym pro.pro_import (Sym_block db)) then
                      sym_add pro.pro_import (Sym_block db);
                  end;
                | OT_array at ->
                  import at.at_objs.(0);
                | OT_array_sel (at,sel) ->
                  import at.at_objs.(0);
                | _ -> ();
                in
              import obj;
            end; 
            | None -> ();
          end;
          | _ -> ();
        ) syml in
        
    let resolve_module mo =
      List.iter (fun pro ->
        let syml = list_of_sym pro.pro_objs in
        find_ram (Some pro) syml;
        let syml = list_of_sym pro.pro_import in
        find_import (Some pro) syml;
        ) mo.mod_procs;
      let syml = list_of_sym mo.mod_objs in  
      find_ram None syml;
      let syml = list_of_sym mo.mod_import in  
      find_import None syml;
      in
      
    let rec iter moexl =
        List.iter (fun mo ->
                iter mo.mod_external;
                resolve_module mo;
            ) moexl;
        in
    iter main.mod_external;
    resolve_module main;
    out (sprintf "Found %d RAM block(s)." (List.length !rams));
    List.iter (fun ram ->
        out (sprintf "<%s>: width=%d size=%d %s" 
                     ram.db_name ram.db_width ram.db_size
                     (
                       let pl = ref "" in
                       List.iter (fun pf ->
                         match pf with
                         | Mp_readsync -> pl := !pl ^ " SYNCREAD ";
                         | Mp_readasync -> pl := !pl ^ " ASYNCREAD ";
                         | Mp_inline -> pl := !pl ^ " INLINE ";
                         | Mp_outline -> pl := !pl ^ " OUTLINE ";
                         | Mp_CREW -> pl := !pl ^ " CREW ";
                         | Mp_EREW -> pl := !pl ^ " EREW ";
                         | Mp_singleport -> pl := !pl ^ " PORT1 ";
                         | Mp_dualport -> pl := !pl ^ " PORT2 ";
                         | Mp_multiport -> pl := !pl ^ " PORT2 ";
                         | Mp_bigendian -> pl := !pl ^ " BIG ";
                         | Mp_littleendian -> pl := !pl ^ " LITTLE ";
                         | _ -> ();
                         ) ram.db_params;
                       !pl
                     ));
        (*
        ** Updating object addresses
        *)
        let addr = ref 0 in
        List.iter (fun obj ->
            match obj with
            | OT_var co -> co.co_index <- !addr; 
                           addr := !addr + co.co_size*co.co_subsize;
            | _ -> error 702630 "Cp_synthesis.resolve_ram: unexpected OT";
          ) ram.db_objs; 
      ) !rams;
    ind_decr ()
 
(*
** Resolve dynamic array access in process pro.
*)
let resolve_array_dyn pro =
    an.a_curpos <- nilsrc();
    out "Resolving dynamic array access...";
    ind_incr ();
    ()

(*
** Post source graph transformations - before synthesis - after analysis and optimiziation on
** source graph level.
*)

let post_transform main =
  an.a_curpos <- nilsrc();
  ind_incr ();
  let rec resolve pro il =
    let resolve1 i =
        List.hd (resolve pro [i]) in
    let resolve2 i =
        match resolve pro [i] with
        | [hd] -> hd;
        | [] -> error 517263 "";
        | li -> PI_block (li,nilbf) in
    match il with
    | instr::tl ->
    begin
      match instr with
      | PI_fun _ ->
        (expand_fun_arg (Some pro) [instr])@
        (resolve pro tl)     
      | PI_assign (src,lhs,rhs) ->
      begin
        line src;
        let rhs' = resolve1 rhs in
        [PI_assign (src,lhs,rhs')] @ 
        (resolve pro tl);
      end;
      | PI_block (il,bf) ->
        let il' = resolve pro il in        
        PI_block (il',bf) :: (resolve pro tl);
      | PI_list il ->
        let il' = resolve pro il in
        PI_list il' :: (resolve pro tl);
      | PI_branch (src,i1,i2,i3) ->
        line src;
        PI_branch (src,
                     resolve1 i1,
                     resolve2 i2,
                     resolve2 i3) :: (resolve pro tl);
      | PI_forloop (src,i1,dir,i2,i3,i4) ->
        line src;
        PI_forloop (src,resolve1 i1,
                        dir,
                        resolve1 i2,
                        resolve1 i3,
                        resolve2 i4) :: (resolve pro tl);
      | PI_loop (src,k,i1,i2) ->
        line src;
        PI_loop (src,k,
                     resolve1 i1,
                     resolve2 i2) :: (resolve pro tl);
      | PI_try (i1,i2) ->
        PI_try (resolve1 i1,
                resolve2 i2) :: (resolve pro tl);
      | PI_select (src,i1,i2) ->
        line src;
        PI_select (src,
                     resolve1 i1,
                     resolve1 i2) :: (resolve pro tl);
      | PI_case (src,il1,i2) ->
        line src;
        PI_case (src,
                     resolve pro il1,
                     resolve2 i2) :: (resolve pro tl);
      | _ -> instr :: (resolve pro tl);
    end;
    | [] -> [] in
    
  let rec transform prol =
    match prol with
    | pro :: tl ->
      out (sprintf "Performing post-analysis transformations for process <%s>..." pro.pro_name);
      out_ (sprintf "Performing post-analysis transformations for process <%s>..." pro.pro_name);
      let il' = resolve pro pro.pro_instr in
      pro.pro_instr <- il';
      transform tl;
    | [] ->  () in
  let rec iter mll =
      List.iter (fun ml ->
          transform ml.mod_procs;
          iter ml.mod_external;
          ) mll;
      in
  (*
  ** Firsth main, than the included module tree.
  *)
  iter (main :: main.mod_external);

  ind_decr ()
    
 
(*
** Main entry: Synthesize the module tree. Start synthesizing with 
** deepest tree level and walk up:
**
**  A. Processes -> VHDL entity -> VHDL file
**                  Process objects
**
**  B. Module    -> VHDL entity -> VHDL file 
**                  Components: Processes, module objects
**
**  Tree hierarchy:
**
**                          Module Main
**              ------------------------------------
**              |               |                  | External Modules
**          Process main    Process ...         Module XYZ
**                                          ------------------------
**                                          |                      |
**                                      Process ...          External Modules
**              
**
**                                  ..............................
*)

let synthesize main =
  try
  begin
    let modu = Cp_module.modu main in
    an.a_curpos <- nilsrc();
    out_ (sprintf "Performing controll graph analysis for module <%s>..." main.mod_name);
    Cp_graph.call_graph_build main;
    out_ (sprintf "Synthesizing module <%s>..." main.mod_name);
    out (sprintf "Synthesizing main module <%s>..." main.mod_name);
    ind_incr ();
    
    out_ (sprintf "Optimization: %s %s %s" 
          (if List.mem "fold" compiler.t_opt then "[Expression Constant Folder]" else "")
          (if List.mem "move" compiler.t_opt then "[Invariant Code Mover]" else "")
          (if List.mem "dead" compiler.t_opt then "[Dead Object Remover]" else ""));
          
    let eval = ref true in
    let pass = ref 1 in
    if compiler.t_notty then out_ ("Optimizing and pre-scheduling..");
    while !eval && !pass < 10
    do
      out (sprintf "Optimizing and pre-scheduling, pass %d..." !pass);
      if not compiler.t_notty then progress "Optimizing and pre-scheduling" !pass 9      
      else  out_ (sprintf "[Pass %d/%d]" !pass 9);
      ind_incr();
      let progress1 = eval_ref_stack main in
      let progress2 = 
        (if List.mem "fold" compiler.t_opt then optimize_expr main else 0)+
        (if List.mem "move" compiler.t_opt then optimize_motion main else 0)+
        (if List.mem "dead" compiler.t_opt then optimize_dead_objs main else 0) in
      eval := (progress1 > 0) || (progress2 > 0);
      out (sprintf "... Progress of pass %d: %d." !pass (progress1+progress2));
      ind_decr ();
      incr pass;
    done;
    progress "Optimizing and pre-scheduling" (!pass-1) (!pass-1);
    optimize_summary ();
    
    out_ ("Extracting features and resources...");
    resolve_guards main;
    resolve_ram main;
    post_transform main;
    modu#compile;
    modu#uc_synth;
    ucode_summary ();
    
    if compiler.t_emit then
    begin
      if not compiler.t_ucode then
      begin
        let rec iter moexl =
            List.iter (fun mo ->
                iter mo.mod_external;
                let modu = Cp_module.modu mo in
                modu#vhdl_synth;
            ) moexl;
            in
        iter main.mod_external;
        modu#vhdl_synth;
      end;
      let rec iter moexl =
          List.iter (fun mo ->
              iter mo.mod_external;
              ui_emit_ucode mo;
          ) moexl;
          in
      iter main.mod_external;
      ui_emit_ucode main;
      
      List.iter (fun pro ->
        let pro = Cp_process.pro pro in
        pro#uci_out_emit;
        ) main.mod_procs;
    end;
    rtl_summary ();
    ind_decr ();
  end
  with
    | Synthesis str when not compiler.t_trace ->
      raise (Synthesis (sprintf "Synthesis failure:\n%s" str));
    | Exit when not compiler.t_trace ->
      raise Exit;  
    | _ when not compiler.t_trace -> 
      raise (Synthesis "Synthesis failed: Unknown error!")
        

let init_rules () =
    Cp_data_trans_table.init ();
    Cp_Core.init ();
    Cp_Process.init ();
    Cp_Clock.init ();
    Cp_Reset.init ();
    Cp_Sys.init ();
    
