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
**    $CREATED:     23.5.2006
**    $VERSION:     2.06
**
**    $INFO:
**
**  Register-Transfer-Logic-Transformatio =>
**      State Processing, Mainly MicroCode -> State RTL synthesis.
**
**  Both Data- and Controlpath is compiled here.
**
**    $ENDOFINFO
**
*)

open Cp_types
open Cp_symbol
open Cp_utils
open Cp_analysis
open Cp_ucode
open Cp_expr
open Cp_Core
open Printf
open Cp_common
open Cp_stat

(*
** Print content of a state block, returns line list
*)
  
let print_state sb =
    let lines = ref [] in
    let out_ind = ref 0 in
    let ind_incr () = out_ind := !out_ind + 2 in
    let ind_decr () = out_ind := !out_ind - 2 in
    let out str =
        let spaces n =
            String.make n ' ' in
        lines := !lines @ [(spaces !out_ind)^str];
        in

    let print_sd sd =
        match sd with
        | Data_in str -> out (sprintf "DATA_IN <%s>" str);
        | Data_sens str -> out (sprintf "DATA_SENS <%s>" str);
        | Data_trans_sens str -> out (sprintf "DATA_TRANS_SENS <%s>" str);
        | Data_cond_sens str -> out (sprintf "DATA_COND_SENS <%s>" str);
        | Data_out str -> out (sprintf "DATA_OUT <%s>" str);
        | Data_signal str -> out (sprintf "DATA_SIGNAL <%s>" str);
        | Data_trans str -> out (sprintf "DATA_TRANS <%s>" str);
        | Data_top str -> out (sprintf "DATA_TOP <%s>" str);
        | Data_top_def str -> out (sprintf "DATA_TOP_DEF <%s>" str);
        | Data_cond str -> out (sprintf "DATA_COND <%s>" str);
        | Data_def (str1,str2) -> out (sprintf "DATA_DEF <%s=%s>" str1 str2);
        | Data_trans_def (str1,str2) -> out (sprintf "DATA_TRANS_DEF <%s=%s>" str1 str2);
        | Data_process _ -> () 
        in        


    let rec print_ns ns =
        let print_cs cs =
            out "CASE:";
            ind_incr ();
            out "Next:";
            ind_incr ();
            print_ns cs.cs_next;
            ind_decr ();
            out "Data:";
            ind_incr ();
            List.iter print_sd cs.cs_data;
            ind_decr ();
            ind_decr (); in

        match ns with
        | Next str -> out (sprintf "NEXT <%s>." str);
        | Next_instr -> out "NEXT <instr>.";
        | Branch(sdl,ns1,ns2) ->
            out "BRANCH:";
            ind_incr ();
            out "Data:";
            ind_incr ();
            List.iter print_sd sdl;
            ind_decr ();
            out "S1:";
            ind_incr ();
            print_ns ns1;
            ind_decr ();
            out "S2:";
            ind_incr ();
            print_ns ns2;
            ind_decr ();
        | Select (sdl,csl) ->
            out "SELECT:"; 
            ind_incr ();
            out "Data:";
            ind_incr ();
            List.iter print_sd sdl;
            ind_decr ();
            out "Cases:";
            ind_incr ();
            List.iter print_cs csl;
            ind_decr (); 
        in
            
    let print s =
        out (sprintf "State: %s" s.s_name);
        ind_incr ();
        out "Next state:";
        ind_incr ();
        print_ns s.s_next;
        ind_decr ();
        out "Data:";
        ind_incr ();
        List.iter print_sd s.s_data;
        ind_decr () in
    let rec state_iter sb =
        match sb with
        | State s -> print s;
        | State_block sbl -> List.iter state_iter sbl;
        | State_top s -> print s;
        in
    state_iter sb;
    !lines

let out_state sb =
    List.iter (fun str -> out str) (print_state sb)


(*
** Convert process MicroCode instruction list to state list.
** ALU and temporary registers must already evaluated.
*)
let rtl_of_ucode pro =
    let tempn = List.length pro.pro_temps in
    let pro_main = pro.pro_name = "main" in
    an.a_curpos <- nilsrc();
    out (sprintf "Translating MicroCode to RTL in process <%s>..."
         pro.pro_name);
    ind_incr();
    (*
    ** Tons of util functions...
    *)
    let last_label = sprintf "S_%s_end" pro.pro_name in



    (*
    ** MicroCode -> States transformation
    *)  

    
    let sl = ref [] in
    let add s = 
        if debug_it "rtl_of_ucode" then out_state s;
        sl := !sl @ [s] in
 
    let label = ref "" in
    

    let fix_relat_eq str =
      let str' = String.copy str in
      protect(
        let pos = String.index str' '>' in
        str'.[pos] <- '=');
      protect(
        let pos = String.index str' '<' in
        str'.[pos] <- '=');
      str' in


    prev_alu := None;



    let rec get_last l =
      match l with
      | hd::tl -> if tl = [] then hd else get_last tl;
      | [] -> error 416764 "" in
      
    (*
    ** State name
    *)
    let snum = ref 0 in
    let sname () =
      if !snum = 0 then
        sprintf "S_%s" !label
      else
        sprintf "S_%s_%d" !label !snum
      in
    let sname_next next = sprintf "S_%s" next in
    let sname_target label =
        match label with
        | UC_label str -> 
            if str <> "%END" then sprintf "S_%s" str else last_label;
        | _ -> error 501386 "";
        in

    (*
    ** Evaluate one single instruction or multiple bounded
    ** instructions. Only one state results. This limits the
    ** way instructions are bound.
    *)
    let state = ref None in 

    let rec eval uil next =
      debug "rtl_of_ucode" with (sprintf "eval uil=%d" (List.length uil));

      let add_state new_state rest =
        if debug_it "rtl_of_ucode" then
        begin
            out "add_state:";
            out_state (State new_state);
        end;

        (*
        ** Merge states. Note: only one Branch may exist, except in
        ** Select!
        *)
        let is_branch s =
          match s.s_next with
          | Branch _ -> true;
          | _ -> false in

        let is_select s =
          match s.s_next with
          | Select _ -> true;
          | _ -> false in
          
        let merge_data sd sd' = 
            let sd'' = 
              (List.filter (fun s ->
                match s with
                | Data_cond "X = others" -> false;
                | _ -> true) sd) @ 
              (List.filter (fun s ->
                match s with
                | Data_cond "X = others" -> false;
                | _ -> true) sd') in
            sd''
            in
        let merge_next n n' = 
          match n with
          | Branch (cp,n1,n2) ->
          begin
            match n' with
            | Branch (cp',n1',n2') ->
            begin
              let cond = List.find (fun s -> match s with Data_cond _ -> true|_->false) cp in
              let sens = List.filter (fun s -> match s with Data_sens _ -> true|_->false) cp in
              let cond' = List.find (fun s -> match s with Data_cond _ -> true|_->false) cp' in
              let sens' = List.filter (fun s -> match s with Data_sens _ -> true|_->false) cp' in
              match cond with
              | Data_cond cs -> 
              begin
                match cond' with
                | Data_cond cs' ->
                  Branch ([Data_cond (sprintf "(%s) and (%s)" cs cs')]@sens@sens',n1,n2)
                | _ -> progerr "Data_cond";
              end;
              | _ -> progerr "Data_cond";
            end
            | _ -> error 0 (sprintf "Can't merge next state control in RTL.") 
          end;
          | _ -> error 0 (sprintf "Can't merge next state control in RTL.") in
          
          
            (*
            ** Split both lists with respect to Data_trans occurence,
            ** and merge these four partial lists again in correct order.
            *)
(*************
            let sd1 = ref [] in
            let sd2 = ref [] in
            let sd1' = ref [] in
            let sd2' = ref [] in
            let trans = ref false in
            let trans' = ref false in
            List.iter (fun s ->
                match s with
                | Data_trans _ -> trans := true;
                                  sd2 := !sd2 @ [s];
                | _ -> if not !trans then
                        sd1 := !sd1 @ [s]
                       else
                        sd2 := !sd2 @ [s];
                ) sd;
            List.iter (fun s ->
                match s with
                | Data_trans _ -> trans' := true;
                                  sd2' := !sd2' @ [s];
                | _ -> if not !trans' then
                        sd1' := !sd1' @ [s]
                       else
                        sd2' := !sd2' @ [s];
                ) sd';
            !sd1 @ !sd1' @ !sd2 @ !sd2'
            in
***************)

        match !state with
        | Some s -> 
        begin
            if is_select s then
            begin
                match s.s_next with
                | Select (sd,csl) ->
                    if is_select new_state then
                    begin
                        match new_state.s_next with
                        | Select (sd',csl') ->
                            let csl'' = csl' @ csl in
                            let sd'' =
                              List.map (fun sd ->
                                match sd with
                                | Data_cond c -> Data_cond (fix_relat_eq c);
                                | _ -> sd) sd in
                            s.s_next <- Select (sd'',csl'');
                        | _ -> error 195942 "";
                    end
                    else if is_branch new_state then
                    begin
                        (*
                        ** Sensivity and auxilliary signals of expression are
                        ** required
                        *)
                        if s.s_data = [] then 
                          s.s_data <- (List.filter (fun sd ->
                            match sd with
                            | Data_out _ | Data_sens _ | Data_def _ -> true
                            | _ -> false
                                ) new_state.s_data);  
                        
                        match new_state.s_next with
                        | Branch (sd',s1,s2) ->
                        begin
                            (*
                            ** contains expression 
                            *)
                            if sd = [] then 
                            begin
                                (*
                                ** when others...
                                *)
                                debug "rtl_of_ucode" with "when others...";

                                let csl'' = csl @ [{
                                        cs_data=[Data_cond "X = others"];
                                        cs_next=s2;
                                    }] in
                                s.s_next <- Select (new_state.s_data,csl'');
                            end;
                            if sd = [Data_cond "X = others"] then
                                s.s_next <- Select (new_state.s_data,csl);

                            let lowerbound,upperbound =
                              (*
                              ** Part of range expression?
                              *)
                              match new_state.s_data with
                              | (Data_cond c)::_ -> 
                                String.contains c '<',
                                String.contains c '>'
                              | _ -> false,false in
                              
                            match csl with
                            | this::_ -> 
                                if not lowerbound && not upperbound then
                                  this.cs_data <- new_state.s_data
                                else
                                begin
                                  (*
                                  ** Range expression
                                  *)
                                                       
                                  match new_state.s_data with
                                  | (Data_cond c)::tl ->
                                  if lowerbound then
                                  begin
                                    let pos = String.index c '<' in
                                    let len = String.length c in
                                    let c' = String.sub c (pos+1) (len-pos-1) in
                                    this.cs_data <- [Data_cond c'] @ tl; 
                                  end
                                  else
                                  begin
                                    let c = fix_relat_eq c in
                                    match this.cs_data with
                                    | (Data_cond c') :: tl' ->
                                      let rc = sprintf "%s to %s" c c' in
                                      this.cs_data <- [Data_cond rc] @ tl;
                                      debug "rtl_of_ucode" with (sprintf "Range condition found: <%s>" rc);
                                    | _ -> error 708005 "";
                                  end;
                                  | _ -> error 78154 "";
                                end;
                            | [] -> error 699355 "";
                        end;
                        | _ -> error 938960 "";
                    end
                    else
                        error 731035 "";
                | _ -> error 972115 "";
            end
            else if is_branch s &&
                    is_branch new_state then
            begin
              rtl_log (sprintf "[%s.%s] %s" pro.pro_module.mod_name pro.pro_name (where ()))
                      "Bounded instruction block contains more than one control statement.\n";
              (*
              ** Only assignments and abstract object method calls possible. Merge
              ** control and data path
              *)  
              s.s_data <- merge_data s.s_data new_state.s_data;  
              s.s_next <- merge_next s.s_next new_state.s_next;
            end
            else if is_branch new_state then
            begin
                s.s_data <- merge_data s.s_data  new_state.s_data;
                s.s_next <- new_state.s_next;
            end
            else
            begin
                s.s_data <- merge_data s.s_data  new_state.s_data;
            end;
            if rest = [] then 
            begin
              add (State s);
              state := None;
            end;
        end;
        | None -> if rest = [] then
                    add (State new_state)
                  else
                    state := Some new_state;
        in
      debug "rtl_of_ucode" with (sprintf "eval next=%s" next);

      let n = List.length uil in
      let single = n = 1 in
      let is_case,is_others,is_range =
        if n >= 3 then
        begin
            let uil = List.rev uil in
            let is_jump ui =
                match ui.ui_code with
                | Jump _ -> true;
                | _ -> false in
            let is_falsejump ui =
                match ui.ui_code with
                | Falsejump _ -> true;
                | _ -> false in
            let is_expr ui =
                match ui.ui_code with
                | Expr _ -> true;
                | _ -> false in
            let is_nop ui =
                match ui.ui_code with
                | Nop -> true;
                | _ -> false in
              
            (*
            ** One case sequence (1) in select statement (match with):
            **   EXPR
            **   FJUMP
            **   JUMP/NOP
            ** 
            ** Special case (2): default
            **
            **   JUMP
            **
            ** Special case (3): range
            **   EXPR
            **   FJUMP
            **   EXPR
            **   FJUMP
            **   JUMP
            *)
            let is_others = 
                (n > 4 &&                               (* (2) *)
                 (is_jump (List.nth uil 0)) &&
                 (is_jump (List.nth uil 2)) &&
                 (is_falsejump (List.nth uil 3)) &&
                 (is_expr (List.nth uil 4))) in

            let is_range =
                (n > 4 &&                               (* (2) *)
                 (is_jump (List.nth uil 0)) &&
                 (is_falsejump (List.nth uil 1)) &&
                 (is_expr (List.nth uil 2)) &&
                 (is_falsejump (List.nth uil 3)) &&
                 (is_expr (List.nth uil 4))) in

            ((is_jump (List.nth uil 0)) &&          (* (1A) *)
             (is_falsejump (List.nth uil 1)) &&
             (is_expr (List.nth uil 2))) ||
            ((is_nop (List.nth uil 0)) &&          (* (1B) *)
             (is_falsejump (List.nth uil 1)) &&
             (is_expr (List.nth uil 2))) ||
            is_others ||
            is_range,
            is_others,
            is_range
        end
        else
            false,false,false
        in
      let ui,instr,block = 
        let last = get_last uil in
        last,last.ui_code,last.ui_frame in

      debug "rtl_of_ucode" with (sprintf "is_case=%b is_others=%b is_range=%b" 
                     is_case is_others is_range);

      line block.bf_src_start;
      match instr with
      | Expr (ops,dst,src1,src2) -> 
        if single then
        begin
          (*
          ** Simple assignment: register transfer
          *)
          let dp,cp_wr,cp_rd = expr_assign pro ops dst src1 src2 in
          if cp_wr = [] && cp_rd = [] then
          begin
            let s = {
                      s_name = sname ();
                      s_next = Next (sname_next next);
                      s_data = dp;
                      s_block = block;
                    } in
            incr snum;
            add_state s [];
          end
          else if cp_wr <> [] then
          begin
            let rec s = {
                      s_name = sname ();
                      s_next = Branch (cp_wr,
                                       Next (sname ()),
                                       Next (sname_next next));
                      s_data = dp;
                      s_block = block;
                    } in
            incr snum;
            add_state s [];
          end
          else if cp_rd <> [] then
          begin
            let rec s = {
                      s_name = sname ();
                      s_next = Branch (cp_rd,
                                       Next (sname ()),
                                       Next (sname_next next));
                      s_data = dp;
                      s_block = block;
                    } in
            incr snum;
            add_state s [];
          end
          else
            error 126248 "";
        end
        else
        begin
            (*
            ** Must be an immediate expression list. Can be evaluated
            ** concurrently.
            *)
          let rec get_expr_type obj = 
                match obj with
                | UC_reg uo -> 
                    uo.uo_type.uo_expr_type;
                | UC_var uo -> 
                    uo.uo_type.uo_expr_type;
                | UC_sig uo ->
                    uo.uo_type.uo_expr_type;
                | UC_temp ut ->
                    ut.ut_type.uo_expr_type;
                | UC_queue uo -> 
                    uo.uo_type.uo_expr_type;
                | UC_chan uo -> 
                    uo.uo_type.uo_expr_type;
                | UC_sel us ->
                    get_expr_type us.us_obj;
                | _ -> 
                    error 906040 (ui_sprint_ud obj);
                in
          let target = get_expr_type dst in
          let dp,cp_wr,rest = expr_bind pro uil in

          if cp_wr = [] then
          begin
            let s = {
                      s_name = sname ();
                      s_next = Next (sname_next next);
                      s_data = dp;
                      s_block = block;
                    } in
            add_state s rest;
          end
          else 
          begin
            out (sprintf "!!! Warning: guarded access in bounded block <%s>."
                         (sname ()));
            out "!!! Warning: Neither one cycle nor data consistency is guaranteed.";
            let rec s = {
                      s_name = sname ();
                      s_next = Branch (cp_wr,
                                       Next (sname ()),
                                       Next (sname_next next));
                      s_data = dp;
                      s_block = block;
                    } in
            add_state s rest;
          end;
          if rest <> [] then eval rest next else incr snum;
        end;
      | Falsejump (cond,target) -> 
            if single then
                error 997410 "";
            let dp,_,rest = expr_bind pro uil in
            let rec s = {
                      s_name = sname ();
                      s_next = Branch (dp,
                                       Next (sname_next next),
                                       Next (sname_target target));
                      s_data = dp;
                      s_block = block;
                    } in
            add_state s rest;
            if rest <> [] then eval rest next;
      | Jump target -> 
          let rest = List.rev (List.tl (List.rev uil)) in
          if is_case then
          begin
            (*
            ** Must be a select-case list.
            *)
            let sd = if not is_others then [] else
                        [Data_cond "X = others"] in
            let s = {
                    s_name = sname ();
                    s_next = Select (
                                sd,
                                [{
                                    cs_data=sd;
                                    cs_next=Next (sname_target target);
                                }]);
                    s_data = [];
                    s_block = block;
                } in
            add_state s rest;
          end
          else (* if not single then *)
          begin
            let s = {
                    s_name = sname ();
                    s_next = Next (sname_target target);
                    s_data = [];
                    s_block = block;
                } in
            add_state s rest;
          end; (*
          else
            error 689370 (sprintf "%s" (ui_sprint_uc ui)); *)
          if single then incr snum;
          if rest <> [] then eval rest next;
      | Move (dst,src) -> ();
          (*
          ** Simple assignment: register transfer
          *)
          let rest = List.rev (List.tl (List.rev uil)) in
          let dp,cp_wr,cp_rd = simple_assign pro dst src in
          debug "rtl_of_ucode" with (sprintf "simple_assign %s returns: cp_wr:%b cp_rd:%b"
                         an.a_errlabel (cp_wr <> []) (cp_rd <> []));
          if cp_wr = [] && cp_rd = [] then
          begin
            let s = {
                      s_name = sname ();
                      s_next = Next (sname_next next);
                      s_data = dp;
                      s_block = block;
                    } in
            if single then incr snum;
            add_state s rest;
          end
          else if cp_wr <> [] && cp_rd = [] then
          begin
            let rec s = {
                      s_name = sname ();
                      s_next = Branch (cp_wr,
                                       Next (sname ()),
                                       Next (sname_next next));
                      s_data = dp;
                      s_block = block;
                    } in
            if single then incr snum;
            add_state s rest;
          end
          else if cp_wr = [] && cp_rd <> [] then
          begin
            let rec s = {
                      s_name = sname ();
                      s_next = Branch (cp_rd,
                                       Next (sname ()),
                                       Next (sname_next next));
                      s_data = dp;
                      s_block = block;
                    } in
            if single then incr snum;
            add_state s rest;
          end
          else
            error 896419 "";
          if rest <> [] then eval rest next;
      | Fun ((opl,ot),sel,args) ->
        let rest = List.rev (List.tl (List.rev uil)) in
        let ao = ao_of_ot opl ot in
        let ao_mod = ao.ao_type.ta_rules in 
        let modu = pro.pro_module in
        let scl = ao_mod.rl_fun_scode ui (sname ()) 
                                     (sname_next next)
                                      modu pro in
        line block.bf_src_start;
        if single then 
            List.iter (fun s ->
                s.s_block <- block;
                add_state s [];
                ) scl
        else
          List.iter (fun s ->
                s.s_block <- block;
                add_state s rest;
                ) scl;
   
        if single then incr snum;
        if rest <> [] then eval rest next;
      | Label _ 
      | Nop -> 
          let rest = List.rev (List.tl (List.rev uil)) in
          if is_case then
          begin
            (*
            ** Must be a select-case list, here last case with immediately
            ** following true block.
            *)
            let sd = if not is_others then [] else
                        [Data_cond "X = others"] in
            let s = {
                    s_name = sname ();
                    s_next = Select (
                                sd,
                                [{
                                    cs_data=sd;
                                    cs_next=Next (sname_next next);
                                }]);
                    s_data = [];
                    s_block = block;
                } in
            add_state s rest;
          end;
          if rest <> [] then eval rest next
          else
          begin
            match !state with
            | Some s -> add (State s);
                        state := None;
            | None -> ();
          end;
      | Special _ -> error 0 "Found unexpected special uCode instruction within process?";
      | Bind _ -> error 0 "Found unexpected bind-uCode instruction within process?";
      in
     

    (*
    ** Special instructions
    *)
    let eval_special pi next =
        match pi with
        | _ -> (); 
        in
 
    (*
    ** Bind instructions into list
    *)
    let rec bind l n =
      match l with
      | hd::tl -> if n = 1 then [hd],tl else
                  begin
                    let l',tl' = bind tl (n-1) in
                    [hd]@l',tl' 
                  end;
      | [] -> [],[];
      in
      
    (*
    ** Get next instruction label - either real label (=true) or
    ** label name for next instruction (using !label and !snum) (=false)
    *)
    let not_end l =
        let str_end = "_end" in
        let n = String.length l in
        (n < 4) ||
        ((String.sub l (n-4) 4) <> str_end) in

    let rec get_next_label uil reallabel =
      let fix_last l =
        if l <> "%END" then l else sprintf "%s_end" pro.pro_name in

      let next_is_label li =
        match li with
        | ui::_ ->
        begin
            match ui.ui_code with
            | Label _ -> true;
            | _ -> false;
        end;
        | _ -> false in
        
      let next_is_nop li =
        match li with
        | ui::_ ->
        begin
            match ui.ui_code with
            | Nop -> true;
            | _ -> false;
        end;
        | _ -> false in
        
    
      match uil with
      | ui::tl -> 
      begin
        line ui.ui_frame.bf_src_start;
        match ui.ui_code with
          | Label l ->  if (not_end l) && not (next_is_label tl) && 
                                          not (next_is_nop tl) then 
                            (fix_last l) 
                        else get_next_label tl reallabel
          | Nop -> get_next_label tl reallabel;
          | Jump ul ->
          begin
            match ul with
            | UC_label label -> 
                if not_end label then (fix_last label)
                else 
                begin
                    (*
                    ** Search next label/jump after _end label...
                    ** But first find _end label:
                    *)
                    let rec find li =
                        match li with
                        | hd'::tl' ->
                        begin
                            match hd'.ui_code with
                            | Label l -> if l <> label then find tl'
                                         else tl';
                            | _ -> find tl';
                        end;
                        | [] -> error 0 
                                (sprintf "rtl_of_ucode: can't find label %s"
                                         label);
                        in
                    let tl' = find tl in
                    get_next_label tl' reallabel
                end;
            | _ -> error 894102 "";
          end;
          | _ -> if reallabel then get_next_label tl true
                    else 
                 if tl <> [] then
                    sprintf "%s_%d" !label (!snum+1)
                 else
                 begin
                    sprintf "%s_end" pro.pro_name
                 end;
      end;
      | [] -> sprintf "%s_end" pro.pro_name in
      
    (*
    ** Iterate MicroCode instruction list
    *)
    let rec iter uil =
      match uil with
      | this::tl ->
      begin
        match this.ui_code with
        | Bind n -> 
            let uil',tl' = bind tl n in
            (*
            ** Commonly only data path instructions are bound (and 
            ** abstract object calls).
            ** The next label will be in tl. But in the case
            ** there is (only) a jump at the end of this
            ** bounded block, we must select the jump target
            ** as the next label!
            *)
            let next_label =
              let rec jump_label uil =
                match uil with
                | this :: tl -> 
                begin
                  match this.ui_code with
                   | Label _ | Nop -> jump_label tl;
                   | Jump l ->
                   begin
                    match l with
                    | UC_label ls -> Some ls;
                    | _ -> None 
                   end;
                   | _ -> None;
                end;
                | [] -> None in 
              match jump_label (List.rev uil') with
              | Some l -> l;
              | None -> get_next_label tl' false in 
            eval uil' next_label;
            iter tl';
        | Label str -> 
            label := str;
            an.a_errlabel <- str;
            snum := 0;
            iter tl;
        | Special pi -> 
            eval_special pi (get_next_label tl false); 
            iter tl;
        | Jump _ ->
            if not_end !label then 
            begin
                (match this.ui_frame.bf_src_start with
                 | {s_file="";s_line=1} -> ();
                 | _ ->
                    an.a_errlabel <- sprintf "%s:<%s:%d>" an.a_errlabel 
                                        this.ui_frame.bf_src_start.s_file
                                        this.ui_frame.bf_src_start.s_line;
                );
                eval [this] (get_next_label tl false);
                iter tl;
            end
            else iter tl;
        | _ -> 
            (match this.ui_frame.bf_src_start with
            | {s_file="";s_line=1} -> ();
            | _ ->
                an.a_errlabel <- sprintf "%s:<%s:%d>" an.a_errlabel 
                                        this.ui_frame.bf_src_start.s_file
                                        this.ui_frame.bf_src_start.s_line;
            );
            eval [this] (get_next_label tl false);
            iter tl;
      end;
      | [] -> ();
      in
    iter pro.pro_ucode;

    let block = List.hd pro.pro_frame in
    let last = State {
      s_name= last_label;
      s_next=Next last_label;
      s_data=[];
      s_block = block;
      } in

        
    sl := !sl @ [last];
    
    let start = State {
      s_name=sprintf "S_%s_start" pro.pro_name;
      s_next=(
                    Next (
                            match !sl with
                            | hd::_ ->
                            begin
                                match hd with 
                                | State s -> s.s_name;
                                | _ -> error 254290 "";
                            end;
                            | _ -> last_label;
               ));
      s_data=[];
      s_block = block;
      } in
    sl := start :: !sl;
    pro.pro_states <- [State_block (!sl)];
    out (sprintf "%d states created." (List.length !sl));
    let tempn' = List.length pro.pro_temps in
    if tempn' > tempn then
    begin
      out (sprintf "%d additional temporary register(s) created:"
                   (tempn'-tempn));
      ind_incr ();
      let i = ref 0 in
      List.iter (fun t ->
          incr i;
          let size,tname = 
                match t.co_type with
                | DT_logic n -> n,"logic";
                | DT_int n -> n,"int";
                | DT_bool -> 1,"bool";
                | DT_char -> 8,"char";
                | _ -> error 400572 "";
                in
          if !i > tempn then
            out (sprintf "%s[%d]" tname size);
        ) pro.pro_temps;
      ind_decr ();
    end;
    an.a_errlabel <- "";
    ind_decr()
