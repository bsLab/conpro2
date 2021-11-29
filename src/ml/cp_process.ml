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
**    $INITIAL:     (C) 2006-2013 BSSLAB
**    $CREATED:     1.3.2006
**    $VERSION:     2.12
**
**    $INFO:
**
**  CONPRO process synthesis.
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
**    $ENDOFINFO
**
*)

open Cp_version
open Cp_symbol
open Cp_types
open Cp_utils
open Cp_data
open Cp_analysis
open Cp_Core
open Cp_ucode
open Cp_rtl
open Cp_block_frame
open Cp_expr_sched
open Cp_basicblock
open Printf
open Cp_common
open Cp_stat

(*
** Emit process state declaration
*)
let vhdl_states_decl pro =
    let srcloc st =
        let bf = st.s_block in
        let s = bf.bf_src_start in
        sprintf "-- %s%d[%s:%d]" bf.bf_name bf.bf_id s.s_file s.s_line in

    an.a_curpos <- nilsrc();
    out (sprintf "Emitting state list for process <%s>..." pro.pro_name);
    let rec states sbl = 
        match sbl with
        | sb::tl -> 
        begin
            match sb with
            | State s ->  s :: (states tl);
            | State_block sbl' -> (states sbl') @ (states tl);
            | State_top _ -> states tl;
        end;
        | [] -> [];
        in
    let states = states pro.pro_states in
    vhdl "type pro_states is (";
    vhdl_incr ();
    let len = List.length states in
    stat_set (sprintf "process.%s" pro.pro_name) "states" 0 len; 
    let n = ref 1 in
    List.iter (fun state -> 
        if !n < len then vhdl (sprintf "%s, %s" state.s_name (srcloc state))
        else vhdl (sprintf "%s %s" state.s_name (srcloc state));
        incr n;) states;
    vhdl ");";
    vhdl_decr ();
    let first = List.hd states in
    vhdl (sprintf "signal pro_state: pro_states := %s;"
                  first.s_name);
    vhdl (sprintf "signal pro_state_next: pro_states := %s;"
                  first.s_name)


(*
** Emit process states (control and data path). Return list of
** module toplevel expressions.
*)
let vhdl_states pro =
    let srcloc st =
        let bf = st.s_block in
        let s = bf.bf_src_start in
        sprintf "-- %s%d[%s:%d]" bf.bf_name bf.bf_id s.s_file s.s_line in

    an.a_curpos <- nilsrc();
    out (sprintf "Emitting state machine for process <%s>..." pro.pro_name);
    let state_name s =
      match s with
      | Next s -> s;
      | _ -> error 85264 "emit_states: unexpected state"
      in
      
    let pro_main = pro.pro_name = "main" in
    let tops = ref [] in
    (*
    ** Control path
    *)
    vhdl "-- Instruction Controlpath Unit - The Leitwerk";
    vhdl "control_path: process(";
    vhdl_ind := !vhdl_ind + 8;
    let sensl = ref [] in

    let rec find_sens dl =
        match dl with
        | hd::tl ->
        begin
            match hd with
            | Data_cond_sens sens -> 
                if not (List.mem sens !sensl) && sens <> "" then
                    sensl := !sensl @ [sens];
            | _ -> ();
        end;
        find_sens tl;
        | [] -> ();
        in


    let rec states sl = 
        match sl with
        | sb::tl -> 
        begin
            match sb with
            | State s ->  
            begin
                find_sens s.s_data;
                (
                    match s.s_next with
                    | Branch (dl,_,_) -> find_sens dl;
                    | Select (dl,_) -> find_sens dl;
                    | _ -> ();
                );
                states tl;
            end;
            | State_block sb' -> states sb'; 
                                 states tl;
            | State_top _ -> states tl;
        end;
        | [] -> ();
        in
    states pro.pro_states;

    List.iter (fun sens -> vhdl (sprintf "%s," sens)) !sensl;
    vhdl "pro_state";
    vhdl ")";
    vhdl_ind := !vhdl_ind - 8;

    vhdl "begin";
    vhdl_incr ();

    let rec states sl = 
        match sl with
        | sb::tl -> 
        begin
          match sb with
          | State state ->
          begin
            vhdl (sprintf "when %s => %s" state.s_name (srcloc state));
            vhdl_incr ();
            (
            match state.s_next with
            | Next s -> 
                vhdl (sprintf "pro_state_next <= %s;" s);
                if tl = [] then
                    vhdl (sprintf "PRO_%s_END <= '1';" pro.pro_name);                
            | Branch (dl,s1,s2) ->
                let rec get_cond dl =
                    match dl with 
                    | d::tl -> 
                    begin
                        match d with
                        | Data_cond str -> str;
                        | Data_top str -> 
						  if not (List.mem str !tops) then
						  	tops := !tops @ [str];
                          get_cond tl;
                        | _ -> get_cond tl;
                    end;
                    | [] -> error 355266 "";
                    in
                vhdl (sprintf "if %s then" (get_cond dl));
                vhdl_incr ();
                vhdl (sprintf "pro_state_next <= %s;" (state_name s1));
                vhdl_decr ();
                vhdl "else";
                vhdl_incr ();
                vhdl (sprintf "pro_state_next <= %s;" (state_name s2));
                vhdl_decr ();
                vhdl "end if;";
            | Next_instr -> error 892357 "";
            | Select (dl,csl) -> 
                let rec get_cond dl =
                    match dl with 
                    | d::tl -> 
                    begin
                        match d with
                        | Data_cond str -> str;
                        | Data_top str -> 
						  if not (List.mem str !tops) then
							tops := !tops @ [str];
                          get_cond tl;
                        | _ -> get_cond tl;
                    end;
                    | [] -> error 355267 "";
                    in
                let cond = 
                    match Str.split (Str.regexp "=") (get_cond dl) with
                    | cond::_ -> cond;
                    | _::[]
                    | [] -> error 912785 "";
                    in
                  
                vhdl (sprintf "case %s is" cond);
                vhdl_incr ();
                List.iter (fun cs ->
                    let expr = 
                        match Str.split (Str.regexp "=") 
                              (get_cond cs.cs_data) with
                        | _::(expr::_) -> expr;
                        | _::[] 
                        | [] -> error 912786 "";
                        in
                    let is_range,is_logic,ra,rb =
                      (*
                      ** Logic ranges must be expanded!
                      *)
                      let logic = String.contains expr '"' in
                      let sl = Str.split (Str.regexp " to ") expr in
                      let len = List.length sl in
                      if len = 1 then false,logic,"","" else
                      if len = 2 then true,logic,(List.nth sl 0),(List.nth sl 1)
                      else error 436975 "" in
                    (*
                    ** Logic vector ranges must be expanded into
                    ** incrementing logic vectors!!!
                    *)
                    if not is_range then
                      vhdl (sprintf "when %s => pro_state_next <= %s;" 
                                   expr (state_name cs.cs_next))
                    else
                    begin
                      let n = ref 1 in
                      let strl = if is_logic then 
                                  logic_range ra rb 
                                 else
                                  int_range ra rb in
                      let len = List.length strl in
                      List.iter (fun str -> 
                          if !n = 1 then vhdl (sprintf "when %s" str)
                          else if !n < len then  vhdl (sprintf "   | %s" str) 
                          else vhdl (sprintf "   | %s => pro_state_next <= %s;"
                                     str (state_name cs.cs_next));
                          incr n;) strl; 
                    end;
                    ) csl;
                vhdl_decr ();
                vhdl (sprintf "end case;");
            );
            vhdl_decr ();
          end;
          | State_block sl -> states sl;
          | State_top _ -> ();
        end;
        states tl;
        | [] -> ();
        in
    vhdl (sprintf "PRO_%s_END <= '0';" pro.pro_name);
    vhdl "case pro_state is";
    vhdl_incr ();
    states pro.pro_states;
    vhdl_decr ();
    vhdl "end case;";

    vhdl_decr ();
    vhdl "end process control_path;";
    vhdl "";
    
    (*
    ** Datapath
    *)
    vhdl "-- Instruction Datapath Combinational Unit";
    vhdl "data_path: process(";
    vhdl_ind := !vhdl_ind + 8;

    let dp_sensl = ref [] in
    let tp_sensl = ref [] in
    let dp_defl = ref [] in
    let tp_defl = ref [] in

    let rec find_sens dl =
        match dl with
        | hd::tl ->
        begin
            match hd with
            | Data_sens sens -> 
                if not (List.mem sens !dp_sensl) && sens <> "" then
                    dp_sensl := !dp_sensl @ [sens];
                find_sens tl;
            | Data_trans_sens sens -> 
                if not (List.mem sens !tp_sensl) && sens <> "" then
                    tp_sensl := !tp_sensl @ [sens];
                find_sens tl;
            | _ -> find_sens tl;

        end;
        | [] -> ();
        in
    let rec find_default dl =
        match dl with
        | hd::tl ->
        begin
            match hd with
            | Data_def def -> 
                if not (List.mem def !dp_defl) then
                    dp_defl := !dp_defl @ [def];
                find_default tl;
            | Data_trans_def def -> 
                if not (List.mem def !tp_defl) then
                    tp_defl := !tp_defl @ [def];
                find_default tl;
            | _ -> find_default tl;
        end;
        | [] -> ();
        in

    let rec states sl = 
        match sl with
        | sb::tl -> 
        begin
            match sb with
            | State s ->  
            begin
                find_sens s.s_data;
                find_default s.s_data;
                (
                    match s.s_next with
                    | Branch (dl,_,_) -> find_sens dl;
                                         find_default dl;
                    | _ -> ();
                );
                states tl;
            end;
            | State_block sb' -> states sb'; 
                                 states tl;
            | State_top _ -> states tl;
        end;
        | [] -> ();
        in

    states pro.pro_states;
    List.iter (fun sens -> vhdl (sprintf "%s," sens)) !dp_sensl;
    vhdl "pro_state";
    vhdl ")";
    vhdl_ind := !vhdl_ind - 8;

    vhdl "begin";
    vhdl_incr ();
    vhdl "-- Default values";
    List.iter (fun (signal,value) ->
            vhdl (sprintf "%s <= %s;" signal value);
        ) !dp_defl;

    let trans = ref 0 in

    let rec states sl = 
        match sl with
        | sb::tl -> 
        begin
          match sb with
          | State state ->
          begin
            vhdl (sprintf "when %s => %s" state.s_name (srcloc state));
            vhdl_incr ();
            let datas = ref 0 in
            if state.s_data <> [] then
            begin
              List.iter (fun sd -> 
                match sd with
                | Data_in str -> vhdl str; incr datas;
                | Data_out str -> vhdl str; incr datas;
                | Data_top str -> 
				  if not (List.mem str !tops) then
					tops := !tops @ [str];
                | Data_trans _ -> incr trans;
                | _ -> ();
                ) state.s_data;
              if !datas = 0 then vhdl "null;";
            end
            else
                vhdl "null;";

            vhdl_decr ();
          end;
          | State_block sl -> states sl;
          | State_top state -> 
          begin
            (*
            ** Toplevel instruction only
            *)
            List.iter (fun sd -> 
                match sd with
                | Data_top str -> 
				  if not (List.mem str !tops) then
					tops := !tops @ [str];
                | _ -> ();
                ) state.s_data;
          end;
        end;
        states tl;
        | [] -> ();
        in
    vhdl "case pro_state is";
    vhdl_incr ();
    states pro.pro_states;
    vhdl_decr ();
    vhdl "end case;";

    vhdl_decr ();
    vhdl "end process data_path;";
    vhdl  "";

    if !trans > 0 then
    begin
      (*
      ** Clock transition driven data transfer: RTL
      *)

      vhdl "-- Instruction Datapath Transitional Unit";
      vhdl "data_trans: process(";
      vhdl_ind := !vhdl_ind + 8;

      List.iter (fun sens -> vhdl (sprintf "%s," sens)) !tp_sensl;

      vhdl "conpro_system_clk,";
      vhdl "conpro_system_reset,";
      vhdl "pro_state";
      vhdl ")";
      vhdl_ind := !vhdl_ind - 8;
      vhdl "begin";
      vhdl_incr ();
      vhdl (sprintf "if conpro_system_clk'event and conpro_system_clk='%d' then"
                    (get_env_int "clock_level"));
      vhdl_incr ();
      vhdl (sprintf "if conpro_system_reset = '%d' then"
              (get_env_int "reset_level"));

      vhdl_incr ();

      List.iter (fun (s,v) -> vhdl (sprintf "%s <= %s;" s v)) !tp_defl;

      vhdl_decr ();
      vhdl "else";
      vhdl_incr ();
      let rec states sl = 
        match sl with
        | sb::tl -> 
        begin
          match sb with
          | State state ->
          begin
            vhdl (sprintf "when %s => %s" state.s_name (srcloc state));
            vhdl_incr ();
            let trans = ref 0 in
            if state.s_data <> [] then
            begin
              List.iter (fun sd -> 
                match sd with
                | Data_trans str -> vhdl str; incr trans;
                | _ -> ();
                ) state.s_data;
              if !trans = 0 then vhdl "null;";
            end
            else
                vhdl "null;";

            vhdl_decr ();
          end;
          | State_block sl -> states sl;
          | _ -> ();
        end;
        states tl;
        | [] -> ();
        in
      vhdl "case pro_state is";
      vhdl_incr ();
      states pro.pro_states;
      vhdl_decr ();
      vhdl "end case;";
      vhdl_decr ();
      vhdl "end if;";
      vhdl_decr ();
      vhdl "end if;";
      vhdl_decr ();
      vhdl "end process data_trans;";
      vhdl "";
    end;

    !tops




(*
** Emit process port header.
*)
let vhdl_port modu pro =
    an.a_curpos <- nilsrc();
    let tops = ref [] in
    let main = pro.pro_name = "main" in
    let vhdl_ind = ref 0 in
    let strl = ref [] in
    let vhdl str =
        let spaces n =
            String.make n ' ' in
        strl := !strl @ [(spaces !vhdl_ind)^str]
        in
           
    let port_bits = ref 0 in
    let add_port_bits str =
        let lbrak = Str.split (Str.regexp "(") str in
        match lbrak with
        | left::[right] ->
        begin
            match Str.split (Str.regexp ")") right with
            | range::[_] ->
            begin
                match Str.split (Str.regexp " downto ") range with
                | b::[a] -> 
                    let ai,bi=
                        int_of_string a,
                        int_of_string b in
                    port_bits := !port_bits + (bi-ai+1);
                | [range] ->
                begin
                    match Str.split (Str.regexp " to ") range with
                    | a::[b] -> 
                        let ai,bi=
                            int_of_string a,
                            int_of_string b in
                        port_bits := !port_bits + (bi-ai+1);
                    | _ -> error 326813 str;
                end;
                | _ -> error 326814 range;
            end;
            | _ -> error 326812 right;
        end;
        | _ -> port_bits := !port_bits + 1 in

    vhdl "port(";
    (*
    ** Entity port declaration: Signal connect
    *)
    vhdl_ind := 2;
    vhdl "-- Connections to external objects, components and the outside world";

    List.iter (fun sym ->
            let is_mon,is_debug,sym' = get_mon sym in
            match sym' with
            | Sym_obj obj ->
                let ports',tops'=(get_rules obj).rl_obj_port  
                                sym modu (Some pro) in
                tops := !tops @ tops';
                List.iter (fun str -> 
                    add_port_bits str;
                    vhdl str) ports';
                            
            | Sym_block db ->
                let ports',tops'= Cp_Core.rules.rl_obj_port  
                                sym modu (Some pro) in
                tops := !tops @ tops';
                List.iter (fun str -> 
                    add_port_bits str;
                    vhdl str) ports';
                          
            | Sym_pro pro' ->
                error 100276 "Obsolete";
                if not is_mon then
                    vhdl (sprintf "signal PRO_%s_ENABLE: out %s;"
                          pro'.pro_name (get_env_str "logic_type"))
                else
                begin
                    let states = get_state_names pro' in
                    let len = const_width (V_int (i64 (List.length states))) in
                    vhdl (sprintf "signal MON_PRO_%s_state: out %s_vector(%d downto 0);"
                          pro'.pro_name (get_env_str "logic_type")
                          (len-1));
                    vhdl (sprintf "signal MON_PRO_%s_ENABLE: out %s;"
                          pro'.pro_name (get_env_str "logic_type"));                
                end;
            | _ -> ();
          ) (list_of_sym pro.pro_import);
    (*
    ** Always present
    *)
    vhdl (sprintf "signal PRO_%s_ENABLE: in %s;"
                       pro.pro_name (get_env_str "logic_type"));
    vhdl (sprintf "signal PRO_%s_END: out %s;"
                       pro.pro_name (get_env_str "logic_type"));
    vhdl (sprintf "signal conpro_system_clk: in %s;"
                      (get_env_str "logic_type"));
    vhdl (sprintf "signal conpro_system_reset: in %s"
                      (get_env_str "logic_type"));
    port_bits := !port_bits + 3; 

    vhdl_ind := 0;
    vhdl ");";
    !strl, !tops, !port_bits

(*
** Emit process component mapping.
*)
let vhdl_map modu pro =
    an.a_curpos <- nilsrc();
    let main = pro.pro_name = "main" in
    let vhdl_ind = ref 0 in
    let strl = ref [] in
    let vhdl str =
        let spaces n =
            String.make n ' ' in
        strl := !strl @ [(spaces !vhdl_ind)^str]
        in
            
    vhdl (sprintf "PRO_MAP_%s: %s_%s port map("
                  pro.pro_name (of_mod modu.mod_name) pro.pro_name);
    (*
    ** Entity port declaration: Signal connect
    *)
    vhdl_ind := 2;

    List.iter (fun sym ->
            let is_mon,is_debug,sym' = get_mon sym in
            match sym' with
            | Sym_obj obj ->
                List.iter (fun str -> vhdl str)
                          ((get_rules obj).rl_obj_map  sym modu (Some pro));
                            
            | Sym_block db ->
                List.iter (fun str -> vhdl str)
                          (Cp_Core.rules.rl_obj_map sym modu (Some pro));
                          
            | Sym_pro pro' ->
                if not is_mon then
                begin
                    vhdl (sprintf "PRO_%s_ENABLE => PRO_%s_ENABLE,"
                          pro'.pro_name pro'.pro_name);
                    vhdl (sprintf "PRO_%s_END => PRO_%s_END,"
                          pro'.pro_name pro'.pro_name);
                end
                else
                begin
                    vhdl (sprintf "MON_PRO_%s_ENABLE => MON_PRO_%s_ENABLE,"
                          pro'.pro_name pro'.pro_name);
                    vhdl (sprintf "MON_PRO_%s_state => MON_PRO_%s_state,"
                          pro'.pro_name pro'.pro_name);
                end;
            | _ -> ();
          ) (list_of_sym pro.pro_import);
    (*
    ** Always present
    *)
    vhdl (sprintf "PRO_%s_ENABLE => PRO_%s_ENABLE,"
                       pro.pro_name pro.pro_name);
    vhdl (sprintf "PRO_%s_END => PRO_%s_END,"
                       pro.pro_name pro.pro_name);
    vhdl "conpro_system_clk => conpro_system_clk,";
    vhdl "conpro_system_reset => conpro_system_reset";

    vhdl_ind := 0;
    vhdl ");";
    !strl

(*
** Emit state transition process
*)
let vhdl_trans pro =
    an.a_curpos <- nilsrc();
    let main = pro.pro_name = "main" in
    vhdl "state_transition: process(";
    vhdl_ind := !vhdl_ind + 8;
    vhdl (sprintf "PRO_%s_ENABLE," pro.pro_name);
    vhdl "pro_state_next,";
    vhdl "conpro_system_clk,";
    vhdl "conpro_system_reset";
    vhdl_ind := !vhdl_ind - 8;
    vhdl ")";
    vhdl "begin";
    vhdl_incr();
    let rec get_first_state sl = 
        match List.hd sl with
        | State s -> s.s_name 
        | State_block sb ->  get_first_state sb;
        | _ -> error 759118 "Cp_synthesis.emit_trans: unexpected first state?";
        in
    let first_state = get_first_state pro.pro_states in

    vhdl (sprintf "if conpro_system_clk'event and conpro_system_clk='%d' then"
                      (get_env_int "clock_level"));
    vhdl_incr ();
    vhdl (sprintf 
              "if conpro_system_reset='%d' or PRO_%s_ENABLE='0' then"
              (get_env_int "reset_level")
              pro.pro_name);
    vhdl_incr ();
    vhdl (sprintf "pro_state <= %s;" first_state);
    vhdl_decr ();
    vhdl "else";
    vhdl_incr ();
    vhdl "pro_state <= pro_state_next;";
    vhdl_decr ();
    vhdl "end if;";
    vhdl_decr ();
    vhdl "end if;";
    vhdl_decr();
    vhdl "end process state_transition;"

(*
** Emit one process..
*)
let vhdl_synth modu pro  =
  an.a_curpos <- nilsrc();
  (*
  ** Monitors can create auxilliary signals.
  *)
  let top_expr = ref [] in
  let top_def = ref [] in 
  List.iter (fun pi ->
      let el,dl = Cp_Core.rules.rl_top_vcode pi modu (Some pro) in
      top_expr := !top_expr @ el;
      top_def := !top_def @ dl;
      ) pro.pro_instr;

  let mn = modu.mod_name in
  let pro_main = pro.pro_name = "main" in

  let en = (of_mod mn)^"_"^pro.pro_name in
  let ar = "main" in
  let fn = en^".vhdl" in
  out (sprintf "Creating entity <%s>..." en);
  ind_incr ();
  out_ (sprintf "Synthesizing VHDL-RTL for process <%s>..." pro.pro_name);

  if not compiler.t_check then 
  begin
      try
        vhdl_oc := open_out (sprintf "%s%s" compiler.t_output fn)
      with _ ->
       error 0 (sprintf "Can't open file <%s%s>." compiler.t_output fn)
  end;

  vhdl "--";
  vhdl "-- Automatically generated by";
  vhdl vhdl_version;
  vhdl (sprintf "-- Process implementation of process <%s> from module <%s>." 
            pro.pro_name mn);
  vhdl "--";

  List.iter (fun l -> vhdl l) (ieee ());
  if compiler.t_synth_tool.syn_vhdl_lib <> [] then vhdl "use work.ConPRO.all;";
  
  vhdl (sprintf "entity %s is" en);
  let ports',tops',port_bits = vhdl_port modu pro in
  List.iter (fun line -> vhdl line) ports';
  top_expr := !top_expr @ tops';
  vhdl (sprintf "end %s;" en);

  vhdl (sprintf "architecture %s of %s is" ar en);
  (*
  ** Architecture declaration: Object creation
  *)
  vhdl_incr ();
  vhdl "-- Local and temporary data objects";
  List.iter (fun sym ->
          match sym with
          | Sym_obj obj ->
              remove_attr_from_obj Obj_created obj;
              let s,t = (get_rules obj).rl_obj_decl  sym modu (Some pro) in
              List.iter (fun str -> vhdl str) s;
              top_expr := !top_expr @ t;                    
          | Sym_block db->
              let s,t = Cp_Core.rules.rl_obj_decl sym modu (Some pro) in
              List.iter (fun str -> vhdl str) s;
              top_expr := !top_expr @ t;
          | _ -> ();
        ) (list_of_sym pro.pro_objs);

  vhdl "-- Auxilliary ALU signals";

  let alu_aux,alu_top= 
      if pro.pro_alu <> [] then
          Cp_alu.emit_aux pro
      else
          [],[] in

  List.iter (fun str -> vhdl str) alu_aux;
  vhdl "-- State Processing";
  vhdl_states_decl pro;

  if pro.pro_alu <> [] then
      Cp_alu.emit_alu_states pro;

  vhdl "-- Auxilliary toplevel definitions";
  let rec find_top_def dl =
      match dl with
      | hd::tl ->
      begin
          match hd with
          | Data_top_def d -> 
            if is_vhdl_val d || (not (List.mem d !top_def)) then
              top_def := !top_def @ [d];
			find_top_def tl; 
          | _ -> find_top_def tl;
      end;
      | [] -> ();
      in
  let rec states sl = 
      match sl with
      | sb::tl -> 
      begin
          match sb with
          | State s ->  
          begin
              find_top_def s.s_data;
              (
                  match s.s_next with
                  | Branch (dl,_,_) -> find_top_def dl;
                  | Select (dl,csl) -> 
                  begin
                    find_top_def dl;
                    List.iter (fun cs -> find_top_def cs.cs_data) csl;
                  end;
                  | _ -> ();
              );
              states tl;
          end;
          | State_block sb' -> states sb'; 
                               states tl;
          | State_top _ -> states tl;
      end;
      | [] -> ();
      in
  states pro.pro_states;
  List.iter (fun d ->
      vhdl d;
    ) !top_def;


  vhdl_decr ();
  vhdl "begin";
  (*
  ** Process/Entity Content
  *)
  vhdl_incr ();

  vhdl_trans pro;

  vhdl "-- Process implementation";

  (*
  ** Control and data path units
  *)
  top_expr := !top_expr @ (vhdl_states pro);

  vhdl "-- Object implementation";
  (*
  ** Object implementations
  *)
  List.iter (fun sym ->
          match sym with
          | Sym_obj obj ->
              List.iter (fun str -> vhdl str)
                        ((get_rules obj).rl_obj_code sym modu (Some pro));

          | Sym_block db ->
              List.iter (fun str -> vhdl str)
                        ((match db.db_rules with
                          | Some r -> r;
                          | None -> error 293148 "").rl_obj_code sym modu (Some pro));

          | _ -> ();
        ) (list_of_sym pro.pro_objs);


  (*
  ** Emit alu if any
  *)
  List.iter (fun str -> vhdl str) alu_top;

  if pro.pro_alu <> [] then
      top_expr := !top_expr @ (Cp_alu.emit_alu pro);



  let el,dl = Cp_Core.top_code modu (Some pro) in
  top_expr := !top_expr @ el;
  top_def := !top_def @ dl;
  vhdl "";
  vhdl "-- Toplevel assignments";
  vhdl "-- Monitors";
  List.iter (fun top -> vhdl top) !top_expr;


  vhdl_decr (); 
  vhdl (sprintf "end %s;" ar);

  if not compiler.t_check then
      close_out !vhdl_oc;
  vhdl_oc := Pervasives.stdout;
  out (sprintf "Port width: %d bits" port_bits);
  stat_set (sprintf "process.%s" pro.pro_name) "port-width [bit]" 0 port_bits;
  ind_decr ()

(*
** Mark local core objects (registers,...), add temporary registers...
*)
let mark_and_add_local pro =
  an.a_curpos <- nilsrc();
  List.iter (fun tmp ->
      sym_add pro.pro_objs (Sym_obj (OT_reg tmp));
    ) pro.pro_temps;
    
  let syms = list_of_sym pro.pro_objs in
  List.iter (fun sym ->
      match sym with
      | Sym_obj obj ->
      begin
        match obj with
        | OT_array at ->
            Array.iter (fun obj ->
                match co_of_ot obj with
                | Some co -> if not (List.mem Obj_local co.co_flags) then
                          co.co_flags <- co.co_flags @ [Obj_local];
                | None -> ();
              ) at.at_objs; 
        | _ -> 
        begin
            match co_of_ot obj with
            | Some co -> if not (List.mem Obj_local co.co_flags) then
                      co.co_flags <- co.co_flags @ [Obj_local];
            | None -> ();
        end;
      end;
      | _ -> ();
    ) syms

(*
** Synthesize process instructions
*)
 
let uc_synth pro =  
  let exn = sprintf "PRO_%s_EXCEPTION" pro.pro_name in
  let uncaught = ref 0 in
  stat "process" "";
  (*
  ** Resolve Next references in jumps and transform list list
  ** to linear list.
  *)
  let rec unroll ucll =
      match ucll with
      | ucl::tl ->
      begin
          (List.map (fun uc -> 
              let next = 
                  match tl with
                  | ucl'::_ -> 
                  begin
                    let rec search_label li =
                      match li with
                      | hd::tl ->
                      begin
                        match hd.ui_code with
                        | Label label -> UC_label label;
                        | _ -> search_label tl;
                      end;
                      | [] -> UC_label "%END";
                      in
                    search_label ucl';
                  end;
                  | [] -> UC_label "%END";
                  in
              match uc.ui_code with
              | Falsejump (uo,l) -> 
                if l = UC_next then 
                  {uc with ui_code=Falsejump (uo,next)}
                else if l = (UC_label exn) then
                begin
                   warning (sprintf "Found raise of uncaught exception in process <%s>." 
                                pro.pro_name);
                  incr uncaught;
                  {uc with ui_code=Falsejump (uo,UC_label "%END")}  
                end
                else uc;
              | Jump l -> 
                if l = UC_next then 
                        {uc with ui_code=Jump next}
                else if l = (UC_label exn) then
                begin
                   warning (sprintf "Found raise of uncaught exception in process <%s>." 
                                pro.pro_name);
                  incr uncaught;
                  {uc with ui_code=Jump (UC_label "%END")}  
                end
                else uc;
              | _ -> uc;
              ) ucl) @ (unroll tl);
      end;
      | [] -> [];
      in
  let ucll = ref [] in
  let pro_main = pro.pro_name = "main" in
  out (sprintf "in process <%s.%s>..."
               pro.pro_module.mod_name
               pro.pro_name);
  ind_incr ();
  let id = ref 0 in

  pro.pro_frame <- [{nilbf with bf_name="PROCESS";
                                bf_type=BF_compound;}];
  (*
  ** Resolve Module dependencies. All instructions except
  ** the function call for abstract data types are handled
  ** by the Core module.
  *)
  List.iter (fun instr ->
      let rec get_mod instr =
          match instr with
          | PI_fun (src,(opl,ot),_,_) -> 
              line src; 
              let ao = ao_of_ot opl ot in
              ao.ao_type.ta_rules,Some src;
          | _ -> Cp_Core.rules,None;
          in
      (*
      ** Actually, there are unresolved state transitions
      ** from one state list fragment to the next.
      *)
      let modu,src = get_mod instr in
      let instr_ucl = modu.rl_instr_ucode
                           instr
                           id
                           pro in
      ucll := !ucll @ [instr_ucl];
    ) pro.pro_instr;

  pro.pro_ucode <- unroll !ucll;    


  if List.mem "ucomp" compiler.t_opt then 
      pro.pro_ucode <- ui_compact_ucode pro.pro_ucode;

  ui_resolve_bool pro;

  if List.mem "ubind" compiler.t_opt  then 
      pro.pro_ucode <- ui_bind_ucode pro pro.pro_ucode;

  ui_infer_alu pro; 
  (* 
  ** ui_expr_type_align pro; 
  ** Now handled fully by conversion function from data_trans?
  *)

  pro.pro_ucode <- ui_schedule_expr pro pro.pro_ucode;
  ui_resolve_temps pro;

  pro.pro_ucode <- ui_schedule_basicblock pro pro.pro_ucode;

  if compiler.t_emit && compiler.t_opt <> [] && 
     not compiler.t_ucode then
      rtl_of_ucode pro;

  mark_and_add_local pro;

  if compiler.t_bf_time_calc then
      frame_time_calc pro;

  if !uncaught > 0 then out_ (sprintf "Found <%d> uncaught exception raising in process <%s>."
                                      !uncaught pro.pro_name); 
  ind_decr ()
    



class process pro =
  object (self)    
  val modu = pro.pro_module
  method name = pro.pro_name
  method uc_synth = uc_synth pro
  method vhdl_map = vhdl_map modu pro
  method vhdl_port = vhdl_port modu pro
  method vhdl_synth = vhdl_synth modu pro
  inherit Cp_uci_out.uci_out pro as super
end

  
let pro_tab = Hashtbl.create 100

let pro pro =
  if Hashtbl.mem pro_tab pro.pro_name then
    Hashtbl.find pro_tab pro.pro_name 
  else
  begin
    let pro = new process pro in
    Hashtbl.add pro_tab pro#name pro;
    pro
  end 
