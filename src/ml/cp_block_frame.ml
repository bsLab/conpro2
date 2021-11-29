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
**    $CREATED:     9.5.2007
**    $VERSION:     2.05
**
**    $INFO:
**
** Block frame module. Process instructions consists of 
** block environments. A block environment encapsulates at least one
** instruction (basic block), or is build from a list of blocks.
**
**
**    $ENDOFINFO
**
*)

open Cp_syntax
open Cp_types
open Cp_utils
open Cp_expr
open Cp_common

open Printf

let nilsrc_v = nilsrc ()

let nilbf = {bf_id=0 ;bf_name="";bf_src_start=nilsrc ();
                                 bf_src_end=nilsrc ();
                                 bf_parent=None;
                                 bf_childs=[];
                                 bf_time=FT_0;
                                 bf_loop=(FT_0,FT_0);
                                 bf_type=BF_empty;
                                 bf_params=[]}

let src_copy src dst =
  dst.s_file <- src.s_file;
  dst.s_line <- src.s_line;
  dst.s_cpos <- src.s_cpos 
  
let out_stdout = ref false 
let info = Cp_common.info 
let out = if !out_stdout then Cp_common.out else Cp_common.vhdl 
let ind_incr = if !out_stdout then ind_incr else vhdl_incr
let ind_decr = if !out_stdout then ind_decr else vhdl_decr

(*
** Update block parameters from parent block.
*)
let inherit_params bf =
 match bf.bf_parent with
 | Some bf' -> 
 begin
  let cmp bp' bp = 
    match bp with
    | (BP_expr _) -> 
    begin
      match bp' with
      | BP_expr _ -> true;
      | _ -> false;
    end;
    | (BP_temp _) -> 
    begin
      match bp' with
      | BP_temp _ -> true;
      | _ -> false;
    end;
    | (BP_alu_min_width _) -> 
    begin
      match bp' with
      | BP_alu_min_width _ -> true;
      | _ -> false;
    end;
    | (BP_alu_ops _) -> 
    begin
      match bp' with
      | BP_alu_ops _ -> true;
      | _ -> false;
    end;
    | (BP_alu_type _) -> 
    begin
      match bp' with
      | BP_alu_type _ -> true;
      | _ -> false;
    end;
    | (BP_bind) -> 
    begin
      match bp' with
      | BP_bind -> true;
      | _ -> false;
    end;
    | (BP_schedule _) -> 
    begin
      match bp' with
      | BP_schedule _ -> true;
      | _ -> false;
    end
    | _ -> false in
 

  let rec find bp' bpl =
    match bpl with
    | bp::tl -> if (cmp bp bp') then true else find bp' tl;
    | [] -> false in

  List.iter (fun bp' ->
     match bp' with
     | BP_expr _ 
     | BP_bind 
     | BP_temp _
     | BP_alu_min_width _ 
     | BP_alu_ops _ 
     | BP_alu_type _ 
     | BP_schedule _ ->
     begin
       let exists = find bp' bf.bf_params in
       if not exists then bf.bf_params <- bf.bf_params @ [bp'];
     end;
     | _ -> ();
     ) bf'.bf_params
 end;
 | None -> ()

(*
** Create a new frame of a block environment. Resolves source code
** locations (start & end line of block).
*)
let create_frame pil =
  let bf = {
    bf_id=Random.int 100000;
    bf_name="BLOCK";
    bf_src_start=nilsrc ();
    bf_src_end=nilsrc ();
    bf_parent=None;
    bf_childs=[];
    bf_time=FT_0;
    bf_loop=(FT_0,FT_0);
    bf_type=BF_compound;
    bf_params=[];
    } in

  (*
  ** Find sourcecode location in process instruction list.
  *)
  let rec get_src pil =
    match pil with
    | pi::tl -> 
    begin
      match pi with
      | PI_list pil' 
      | PI_block (pil',_) -> 
        let src = get_src pil' in
        if src = nilsrc_v then get_src tl else src;
      | PI_monitor (src,_,_)
      | PI_fun (src,_,_,_)
      | PI_map (src,_,_)
      | PI_case (src,_,_)
      | PI_select (src,_,_)
      | PI_loop (src,_,_,_)
      | PI_forloop (src,_,_,_,_,_)
      | PI_branch (src,_,_,_) 
      | PI_waitfor (src,_,_,_,_,_) 
      | PI_assign (src,_,_) 
        ->
        if src = nilsrc_v then get_src tl else src;
      | _ -> nilsrc ()      
    end;
    | [] -> nilsrc () in
  let  rec iter pil =
    match pil with
    | hd::tl ->
    begin
      let src = get_src [hd] in
      if bf.bf_src_start = nilsrc_v then
        bf.bf_src_start <- src;
      if src <> nilsrc_v then
        bf.bf_src_end <- src;
      iter tl;
    end;
    | [] -> () in
  iter pil;
  bf
  
let frame_of_block pi =
    match pi with
    | PI_block (_,bf) -> bf;
    | _ -> error 0 "frame_of_block: no PI_Block found"

(*
** Encapsulate all unblocked instructions. Basic instructions
** (assignments) are encapsulated into basic blocks.
*)
let rec instr_frame pil parent =
    let block_frame pi parent =
        match pi with
        | PI_block (il,bf) -> 
          bf.bf_parent <- parent;
          inherit_params bf;
          PI_block (instr_frame il (Some bf),bf);
        | _ -> 
          let bf = {(create_frame [pi]) with bf_type=BF_data} in
          bf.bf_parent <- parent;
          inherit_params bf;
          PI_block (instr_frame [pi] (Some bf),bf)
        in

    match pil with
    | (PI_block (il,bf))::tl -> 
    begin
        match il with
        | [] -> instr_frame tl parent;
        | _ ->
            bf.bf_parent <- parent;
            inherit_params bf;
            (PI_block (instr_frame il (Some bf),bf)) :: (instr_frame tl parent);
    end;
    | (PI_assign (src,lhs,rhs))::tl ->
    begin
        line src;
        let encaps = 
            match parent with
            | Some bf' -> bf'.bf_name <> "ASSIGN";
            | _ -> true in
        if encaps then
        begin
            let pi = PI_assign (src,lhs,rhs) in
            let bf = {(create_frame [pi]) with bf_type=BF_data;
                                           bf_name="ASSIGN";} in
            bf.bf_parent <- parent;
            inherit_params bf;
            [PI_block ([pi],bf)] @ (instr_frame tl parent);
        end
        else
            [PI_assign (src,lhs,rhs)] @ (instr_frame tl parent);
    end;
    | (PI_fun (src,optl,sel,al))::tl ->
        line src;
        let encaps = 
            match parent with
            | Some bf' -> bf'.bf_name <> "ASSIGN";
            | _ -> true in
        if encaps then
        begin
            let pi =  PI_fun (src,optl,sel,al) in
            let bf = {(create_frame [pi]) with bf_type=BF_data;
                                           bf_name="FUN";} in
            bf.bf_parent <- parent;
            inherit_params bf;
            [PI_block ([pi],bf)] @ (instr_frame tl parent);
        end
        else
            [PI_fun (src,optl,sel,al)] @ (instr_frame tl parent);

    | (PI_branch (src,i1,i2,i3))::tl ->
        line src;
        let encaps = 
            match parent with
            | Some bf' -> bf'.bf_name <> "BRANCH";
            | _ -> true in
        let bf = 
          if encaps then
            {(create_frame [PI_branch (src,i1,i2,i3)]) 
                       with bf_type=BF_branch;
                            bf_name="BRANCH";} 
          else
            (get_some parent) in

        if encaps then
          bf.bf_parent <- parent;                                           
        inherit_params bf;

        let i2' = block_frame i2 (Some bf) in
        let i3' = block_frame i3 (Some bf) in

        (frame_of_block i2').bf_name <- "BRANCH_TRUE"; 
        (frame_of_block i2').bf_type <- BF_conditional; 
        (frame_of_block i3').bf_name <- "BRANCH_FALSE"; 
        (frame_of_block i3').bf_type <- BF_conditional; 
        let pi = PI_branch (src,i1,i2',i3') in

        if encaps then
        begin
            [PI_block ([pi],bf)] @ (instr_frame tl parent);
        end
        else
            [pi] @ (instr_frame tl parent);

    | (PI_waitfor (src,i1,n,tu,i2,i3))::tl ->
        line src;
        let encaps = 
            match parent with
            | Some bf' -> bf'.bf_name <> "COND_LOOP";
            | _ -> true in
        let bf = 
          if encaps && (n=0 || n>1) then
                {(create_frame [PI_waitfor (src,i1,n,tu,i2,i3)]) 
                                      with bf_type=BF_loop;
                                           bf_name="COND_LOOP";}
          else if encaps  then
                {(create_frame [PI_waitfor (src,i1,n,tu,i2,i3)]) 
                                      with bf_type=BF_compound;
                                           bf_name="DELAY";}
          else
            (get_some parent) in

        if encaps then
          bf.bf_parent <- parent;                                           
        inherit_params bf;

        let i2' = block_frame i2 (Some bf) in
        let i3' = block_frame i3 (Some bf) in
        (frame_of_block i2').bf_name <- "LOOP_BODY"; 
        (frame_of_block i2').bf_type <- BF_conditional; 
        (frame_of_block i3').bf_name <- "DEFAULT"; 
        (frame_of_block i3').bf_type <- BF_conditional; 
        let pi = PI_waitfor (src,i1,n,tu,i2',i3') in
        let n_cyc = Int64.to_int (cyc_of_time (Int64.of_int n) tu) in
        bf.bf_loop <- 
            (
                if n <> 0 then
                    FT 0
                else FT_0;
            ),
            (
                if n <> 0 then
                    FT n_cyc
                else FT_0;
            );

        if encaps then
            [PI_block ([pi],bf)] @ (instr_frame tl parent)
        else
            [pi] @ (instr_frame tl parent)

    | (PI_forloop (src,i1,dir,i2,i3,i4))::tl ->
        line src;
        let encaps = 
            match parent with
            | Some bf' -> bf'.bf_name <> "COUNT_LOOP";
            | _ -> true in
        let bf = 
          if encaps then
            {(create_frame [PI_forloop (src,i1,dir,i2,i3,i4)]) 
                                      with bf_type=BF_loop;
                                           bf_name="COUNT_LOOP";}
          else
            (get_some parent)  in

        if encaps then
          bf.bf_parent <- parent;                                           
        inherit_params bf;

        let i4' = block_frame i4 (Some bf) in
        (frame_of_block i4').bf_name <- "LOOP_BODY"; 
        (frame_of_block i4').bf_type <- BF_compound; 
        let pi = PI_forloop (src,i1,dir,i2,i3,i4') in

        bf.bf_loop <- 
            (
                if is_const_expr i2 then
                    FT (Int64.to_int (get_const_expr i2))
                else FT_0;
            ),
            (
                if is_const_expr i3 then
                    FT (Int64.to_int (get_const_expr i3))
                else FT_0;
            );
        if encaps then
            [PI_block ([pi],bf)] @ (instr_frame tl parent)
        else
            [pi] @ (instr_frame tl parent);

    | (PI_loop (src,kind,i1,i2))::tl ->
        line src;
        let lname = if i1 <> PI_nop then
                        "COND_LOOP"
                    else 
                        "LOOP" in
        let encaps = 
            match parent with
            | Some bf' -> bf'.bf_name <> lname;
            | _ -> true in

        let bf = 
          if encaps then
                {(create_frame [PI_loop (src,kind,i1,i2)]) 
                                      with bf_type=BF_loop;
                                           bf_name=lname;}
          else
            (get_some parent) in

        if encaps then
          bf.bf_parent <- parent;                                           
        inherit_params bf;

        let i2' = block_frame i2 (Some bf) in
        (frame_of_block i2').bf_name <- "LOOP_BODY"; 
        (frame_of_block i2').bf_type <- BF_compound; 
        let pi = PI_loop (src,kind,i1,i2') in
        if encaps then
            [PI_block ([pi],bf)] @ (instr_frame tl parent)
        else
            [pi] @ (instr_frame tl parent);
    
    | (PI_select (src,i1,i2))::tl ->
        line src;
        let encaps = 
            match parent with
            | Some bf' -> bf'.bf_name <> "SELECT";
            | _ -> true in

        let bf = 
          if encaps then
                {(create_frame [PI_select (src,i1,i2)]) 
                                      with bf_type=BF_branch;
                                           bf_name="SELECT";}
          else
            (get_some parent)  in

        if encaps then 
          bf.bf_parent <- parent;                                           
        inherit_params bf;

        let i2' = block_frame i2 (Some bf) in
        (frame_of_block i2').bf_name <- "CASE_LIST"; 
        (frame_of_block i2').bf_type <- BF_branch; 
        let pi = PI_select (src,i1,i2') in
        if encaps then
            [PI_block ([pi],bf)] @ (instr_frame tl parent)
        else
            [pi] @ (instr_frame tl parent);

    | (PI_case (src,el,i2))::tl ->
        line src;
        let cname = if el <> [] then
                        "SELECT_CASE"
                    else
                        "SELECT_DEF" in
        let encaps = 
            match parent with
            | Some bf' -> bf'.bf_name <> cname;
            | _ -> true in
        
        let bf = 
          if encaps then
                {(create_frame [PI_case (src,el,i2)]) 
                                      with bf_type=BF_branch;
                                           bf_name=cname;}
          else
            (get_some parent) in

        if encaps then
          bf.bf_parent <- parent;                                           
        inherit_params bf;

        let i2' = block_frame i2 (Some bf) in
        (frame_of_block i2').bf_name <- "CASE_BODY"; 
        (frame_of_block i2').bf_type <- BF_conditional; 
        let pi = PI_case (src,el,i2') in
        if encaps then
            [PI_block ([pi],bf)] @ (instr_frame tl parent)
        else
            [pi] @ (instr_frame tl parent);

    | (PI_try (i1,i2))::tl ->
        let encaps = 
            match parent with
            | Some bf' -> bf'.bf_name <> "TRY";
            | _ -> true in

        let bf = 
          if encaps then
                {(create_frame [PI_try (i1,i2)]) 
                                      with bf_type=BF_branch;
                                           bf_name="TRY";}
          else
            (get_some parent)  in

        if encaps then 
          bf.bf_parent <- parent;                                           
        inherit_params bf;

        let i1' = block_frame i1 (Some bf) in
        let i2' = block_frame i2 (Some bf) in
        (frame_of_block i1').bf_name <- "TRY_BODY"; 
        (frame_of_block i1').bf_type <- BF_compound; 
        (frame_of_block i2').bf_name <- "CASE_LIST"; 
        (frame_of_block i2').bf_type <- BF_branch; 
        let pi = PI_try (i1',i2') in
        if encaps then
            [PI_block ([pi],bf)] @ (instr_frame tl parent)
        else
            [pi] @ (instr_frame tl parent);
    | hd::tl -> hd :: (instr_frame tl parent);
    | [] -> []

let rec sprint_time ft =
  let get_time = sprint_time in
  match ft with
  | FT_0 -> 
    "?"
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
    sprintf "%d" n 

(*
** Update blockframe time estimations.
*) 
let rec add_time ft ft' = 
  let add = add_time in
    match ft with
    | FT_0 -> ft';
    | FT n ->
    begin
      match ft' with
      | FT n' -> FT (n+n');
      | FT_min n' -> FT_min (n+n');
      | FT_max n' -> FT_max (n+n');
      | FT_minmax (ft1',ft2') -> FT_minmax (add ft ft1',add ft ft2');
      | FT_list ftl -> 
        let ftr = ref ft in
        List.iter (fun ft'' -> 
          ftr := add !ftr ft'') ftl;
        !ftr;
      | FT_n ftt -> FT_list [ft;ft'];
      | FT_0 -> ft;
    end;
    | FT_min n ->
    begin
      match ft' with
      | FT n' -> FT_min (n+n');
      | FT_min n' -> FT_min (n+n');
      | FT_max n' -> FT_min (n+n');
      | FT_minmax (ft1',ft2') -> FT_minmax (add ft ft1',add ft ft2');
      | FT_list ftl -> 
        let ftr = ref FT_0 in
        List.iter (fun ft'' -> 
          ftr := add !ftr ft'') ftl;
        !ftr;
      | FT_n ftt -> FT_list [ft;ft'];
      | FT_0 -> ft;
    end;
    | FT_max n ->
    begin
      match ft' with
      | FT n' -> FT_max (n+n');
      | FT_min n' -> FT_min (n+n');
      | FT_max n' -> FT_max (n+n');
      | FT_minmax (ft1',ft2') -> FT_minmax (add ft ft1',add ft ft2');
      | FT_list ftl -> 
        let ftr = ref FT_0 in
        List.iter (fun ft'' -> 
          ftr := add !ftr ft'') ftl;
        !ftr;
      | FT_n ftt -> FT_list [ft;ft'];
      | FT_0 -> ft;
    end;
    | FT_minmax (ft1,ft2) ->
    begin
      match ft' with
      | FT n' -> FT_minmax (add ft1 ft',add ft2 ft');
      | FT_min n' -> FT_minmax (add ft1 ft',add ft2 ft');
      | FT_max n' -> FT_minmax (add ft1 ft',add ft2 ft');
      | FT_minmax (ft1',ft2') -> FT_minmax (add ft1 ft1',add ft2 ft2');
      | FT_list ftl -> 
        let ftr = ref FT_0 in
        List.iter (fun ft'' -> 
          ftr := add !ftr ft'') ftl;
        !ftr;
      | FT_n ftt -> FT_list [ft;ft'];
      | FT_0 -> ft;
    end;
    | FT_n ftt -> FT_list [ft;ft'];
    | FT_list ftl -> 
    begin
      match ft' with
      | FT_list ftl' -> FT_list (ftl@ftl');
      | _ -> FT_list (ftl@[ft']);
    end



let rec compact_time ft =
    let is_val ft =
        match ft with
        | FT _
        | FT_min _
        | FT_max _
        | FT_minmax _
        | FT_0 -> true;
        | _ -> false in
        
    match ft with
    | FT _
    | FT_min _
    | FT_max _ 
    | FT_0 -> ft;
    | FT_minmax (n1,n2) ->
        if n1 <> n2 then ft else n1
    | FT_n (ftn,fte) -> 
    begin
      let fte = compact_time fte in
      let rec eval ftn fte =
        match fte with
        | FT n ->
        begin
            match ftn with
            | FT n' -> FT (n*n');
            | FT_min n' -> FT_min (n*n');
            | FT_max n' -> FT_max (n*n');
            | _ -> error 781919 "";
        end;
        | FT_min n ->
        begin
            match ftn with
            | FT n' -> FT_min (n*n');
            | FT_min n' -> FT_min (n*n');
            | FT_max n' -> FT_min (n*n');
            | _ -> error 781920 "";
        end;
        | FT_max n ->
        begin
            match ftn with
            | FT n' -> FT_max (n*n');
            | FT_min n' -> FT_min (n*n');
            | FT_max n' -> FT_max (n*n');
            | _ -> error 781922 "";
        end;
        | FT_minmax (ft1,ft2) ->
        begin
            let ft1,ft2 = compact_time ft1,
                          compact_time ft2 in
            FT_minmax (eval ftn ft1,eval ftn ft2);
        end;
        | _ -> ft;
        in
      eval ftn fte;
    end;
    | FT_list ftl -> 
        let rec iter ftv ftr ftl =
            match ftl with
            | hd :: tl ->
            begin
              let hd = compact_time hd in
              if is_val hd then iter (hd::ftv) ftr tl
              else iter ftv (ftr@[hd]) tl;
            end;
            | [] -> ftv,ftr in
        let ftv,ftr = iter [] [] ftl in
        let ftv' = 
            let ft = ref (List.hd ftv) in
            List.iter (fun ft' -> ft := add_time !ft ft')
                        (List.tl ftv);
            !ft in
        let ftl' = ftv' :: ftr in
        if (List.length ftl') > 1 then
            FT_list (ftv' :: ftr)
        else
            (List.hd ftl')
(*
** Frame printing
*)
let print_frame bf =
    let src_file src = src.s_file in
    let src_line src = src.s_line in
    sprintf "%s%d[%s: %d..%d]" 
            bf.bf_name
            bf.bf_id
            (src_file bf.bf_src_start)
            (src_line bf.bf_src_start)
            (src_line bf.bf_src_end) 

let rec print_frame_tree root =

  let print_bf bf =
    out (sprintf "%s#%d {" bf.bf_name bf.bf_id);
    ind_incr ();
    if bf.bf_src_start.s_line <> bf.bf_src_end.s_line then
      out (sprintf "SOURCE: %s lines %d..%d"
                 bf.bf_src_start.s_file
                 bf.bf_src_start.s_line
                 bf.bf_src_end.s_line)
    else
      out (sprintf "SOURCE: %s line %d"
                 bf.bf_src_start.s_file
                 bf.bf_src_start.s_line);
    (
      match bf.bf_parent with
      | Some bf' ->
        out (sprintf "PARENT: %s#%d" bf'.bf_name bf'.bf_id);
      | None ->
        out "PARENT: NONE";
    );
 
    let t1 = sprint_time bf.bf_time in
    let t2 = sprint_time (compact_time bf.bf_time) in
    if t1 = t2 then
        out (sprintf "TIME: %s TU" t1)
    else
    begin
        out (sprintf "TIME: %s " t1);
        out (sprintf "      = %s TU" t2); 
    end;
    (
        match bf.bf_type with
        | BF_data -> out "TYPE: BASIC BLOCK";
        | BF_branch -> out "TYPE: BRANCH";
        | BF_compound -> out "TYPE: COMPOUND";
        | BF_empty -> out "TYPE: EMPTY";
        | BF_loop -> out "TYPE: LOOP";
        | BF_conditional -> out "TYPE: CONDITIONAL";
    );
    let pl = ref  "" in
    List.iter (fun bp ->
        match bp with
        | BP_unroll -> pl := !pl ^ "<UNROLL>";
        | BP_locked -> pl := !pl ^ "<LOCKED>";
        | BP_bind -> pl := !pl ^ "<BIND>";
        | BP_expr et ->  pl := !pl ^ (sprintf "<EXPR=%s>" (str_of_expr_type et));
        | BP_temp expr ->  pl := !pl ^ (sprintf "<TEMP=%s>" expr);
        | BP_alu_min_width expr ->  pl := !pl ^ (sprintf "<ALUMIN=%d>" expr);
        | BP_alu_ops opl ->
          pl := !pl ^ "<ALUOPS=";
          List.iter (fun op ->
            pl := !pl ^ (op_name op);
            ) opl;
          pl := !pl ^ ">";
        | BP_alu_type dil ->
          pl := !pl ^ "<ALUTYP=";
          List.iter (fun di ->
            match di with
            | T_logic -> pl := !pl ^ "L";
            | T_int -> pl := !pl ^ "I";
            | T_char -> pl := !pl ^ "C";
            | _ -> ();
            ) dil;
          pl := !pl ^ ">";
        | BP_schedule sm ->
          pl := !pl ^ "<SCHED=";
          begin
            match sm with
            | Sched_auto -> pl := !pl ^ "AUTO";
            | Sched_def -> pl := !pl ^ "DEF";
            | Sched_custom sl -> 
              pl := !pl ^ "CUSTOM/";
              let first = ref true in
              List.iter (fun s ->
                pl := sprintf "%s%s%s" !pl  
                  (if !first then "" else "/")
                  (
                    match s with
                    | Sched_refstack -> "RS";
                    | Sched_expr -> "EX";
                    | Sched_basicblock -> "BB";
                  ) ;
                first := false;
                ) sl;
          end;
          pl := !pl ^ ">";
        | BP_inline -> pl := !pl ^ "<INLINE>";
        ) bf.bf_params;
    out (sprintf "PARAMS=%s." !pl);
    if bf.bf_type <> BF_data then
    begin
      out "CHILDS [";
      ind_incr ();
      List.iter (fun bf' ->
            out (sprintf "%s#%d" bf'.bf_name bf'.bf_id);
            ) bf.bf_childs;
      out "]";
    end;
    in

  let rec iter bfl =
    match bfl with
    | bf::tl ->
        print_bf bf;
        iter bf.bf_childs;
        ind_decr ();
        if bf.bf_type <> BF_data then
          ind_decr ();
        out "}";
        iter tl;
    | [] -> () in
  iter [root]  
  

let bf_add_time bf ft = 
  debug "bf_add_time" with (sprintf "%s%d: +%s" bf.bf_name bf.bf_id (sprint_time ft));
  bf.bf_time <- add_time bf.bf_time ft; 
  debug "bf_add_time" with (sprintf "%s%d: %s" bf.bf_name bf.bf_id (sprint_time bf.bf_time))
    
type minmax = Min | Max
  
(*
** Calculate blockframe time estimations
*)    
let frame_time_calc pro =
  Cp_common.out (sprintf "Calculating block frame time estimations for process <%s>..."
                          pro.pro_name);
  let uil =pro.pro_ucode in
  let add bf ft = bf_add_time bf ft in

  let rec ft_get_n ft =
    match ft with
    | FT_0 -> 0;
    | FT n -> n;
    | FT_min n -> n;
    | FT_max n -> n;
    | FT_minmax (_,ft') -> ft_get_n ft';
    | _ -> error 181919 "" in

    
  let bind = ref 0 in
  let decr0 i =
    if !i >= 0 then decr i in

  let rec update_childs bf =
    match bf.bf_parent with
    | Some bf' -> 
        if not (List.mem bf bf'.bf_childs) then
            bf'.bf_childs <- bf'.bf_childs @ [bf];
        update_childs bf';
        inherit_params bf;
    | None -> () in

  let name_sub name n =
    if String.length name > n 
      then String.sub name 0 n 
      else name in
      
  let eval ui =
    update_childs ui.ui_frame;
    match ui.ui_code with
    | Bind n -> bind := n;
    | Move (lhs,rhs) -> 
    begin
      decr0 bind;
      let gd_rd1,gd_wr1 = ud_guard lhs in
      let gd_rd2,gd_wr2 = ud_guard rhs in
      let ft = 
        if not gd_wr1 && not gd_rd2 then
            FT 1 
        else
            FT_min 2 in
      match ui.ui_frame.bf_name with
      | "COND_LOOP"
      | "COUNT_LOOP" ->
      begin
            if !bind <= 1 then
            ui.ui_frame.bf_time <- (
              match ui.ui_frame.bf_time with
              | FT_0 
              | FT _
              | FT_min _
              | FT_max _
                -> FT_list [ft;FT_n (FT_min 1,ui.ui_frame.bf_time)];
              | FT_list [i;FT_n (n,ft')] ->
                let ft'' = add_time ft' ft in
                FT_list [i;FT_n (n,ft'')]
              | _ -> error 733195 "";
              );
      end;
      | _  (* when !bind <= 1 *) ->
            add ui.ui_frame ft;
    end;
    | Expr (_,lhs,op1,op2) -> 
    begin
      decr0 bind;

      match lhs with
      | UC_immed _ -> ();
      | _ ->
      begin
          let gd_rd1,gd_wr1 = ud_guard lhs in
          let gd_rd2,gd_wr2 = ud_guard op1 in
          let gd_rd3,gd_wr3 = ud_guard op2 in
          let ft =
            if not gd_wr1 && not gd_rd2 && not gd_rd3 then
              FT 1
            else
              FT_min 2 in
          

          match ui.ui_frame.bf_name with 
          | "COND_LOOP"
          | "COUNT_LOOP" ->
          begin
            if !bind <= 1 then
            ui.ui_frame.bf_time <- (
              match ui.ui_frame.bf_time with
              | FT_0 
              | FT _
              | FT_min _
              | FT_max _
                -> FT_list [ft;FT_n (FT_min 1,ui.ui_frame.bf_time)];
              | FT_list [i;FT_n (n,ft')] ->
                let ft'' = add_time ft' ft in
                FT_list [i;FT_n (n,ft'')]
              | _ -> error 733195 "";
              );
          end;
          | _  (* when !bind <= 1 *) ->
            add ui.ui_frame ft;
      end;                                              
    end;
    | Falsejump _ ->
    begin
        decr0 bind;

        match ui.ui_frame.bf_type with
        | BF_loop ->
        begin
          match ui.ui_frame.bf_time with
          | FT_list [i;FT_n (n,ft')] ->
            let ft'' = add_time ft' (FT 1) in
            ui.ui_frame.bf_time <- FT_list [i;FT_n (n,ft'')];
          | FT_0 when (ui.ui_frame.bf_name = "COUNT_LOOP") ->
            ui.ui_frame.bf_time <- FT_n (FT_min 1,FT 1); 
          | FT_0 when (ui.ui_frame.bf_name = "COND_LOOP") ->
            ui.ui_frame.bf_time <- FT_n (FT_min 1,FT 1);
          | _ -> add ui.ui_frame (FT 1);
        end;
        | BF_branch -> add ui.ui_frame (FT 1);
        | _ -> ();
    end;
    | Jump _ ->
    begin
        decr0 bind;

        match ui.ui_frame.bf_type with
        | BF_branch when (ui.ui_frame.bf_time = FT_0) -> 
            add ui.ui_frame (FT 1);
        | BF_loop when ui.ui_frame.bf_name = "LOOP" ->
            ui.ui_frame.bf_time <- FT_n (FT_min 1,FT_0);
        | _ -> ();
    end;
    | Label _ ->
    begin
        decr0 bind;

        match ui.ui_frame.bf_type with
        | BF_loop when ui.ui_frame.bf_name = "LOOP" ->
            if ui.ui_frame.bf_time = FT_0 then
                ui.ui_frame.bf_time <- FT_n (FT_min 1,FT_0);
        | _ -> ();
    end;
    | Fun ((opl,ot),sel,_) ->
    begin
        decr0 bind;
        match ot with
        | OT_object ao ->
        begin
            let ft = (get_rules ot).rl_time ao.ao_module None 
                                                    (PI_fun (nilsrc (),
                                                             (opl,ot),
                                                             sel,[])) in
            add ui.ui_frame (compact_time ft);
        end;
        | _ -> add ui.ui_frame (FT_min 1);
    end;
    | _ -> 
        decr0 bind;
        in
  let uia = Array.of_list uil in
  let uin = Array.length uia in
  let labels = ref [] in
  for i = 0 to uin-1
  do
    let ui = uia.(i) in
    match ui.ui_code with
    | Label str -> labels := !labels @ [str,i];
    | _ -> ();
  done;
  let rec find_addr label ll =
     match ll with
     | (str,addr)::tl -> 
        if str = label then addr else find_addr label tl;
     | [] -> error 0
                     (sprintf "Unknown UC label <%s> found." label) in 
  let pc = ref 0 in
  let next () =
    incr pc;
    !pc = uin
    in
  let rec step () =
    let ui = uia.(!pc) in
    match ui.ui_code with
    | _ ->
      eval ui;
      let exit = next () in
      if not exit then step ();
    in
  step ();
  let root = 
    match pro.pro_instr with
    | (PI_block (_,bf))::tl -> bf;
    | _ -> error 0 "Can't find root frame" in


  let calc_block_time bf =
    let is_bind = List.mem BP_bind bf.bf_params in
    let is_branch = bf.bf_type = BF_branch in
    let is_loop = bf.bf_type = BF_loop in
    let tmp_bf = ref FT_0 in

    if is_loop then
    begin
      tmp_bf := bf.bf_time;
      bf.bf_time <- FT_0;
    end;
    
    if not is_bind && not is_branch then
    begin
      List.iter (fun bf' -> bf_add_time bf bf'.bf_time) bf.bf_childs;
    end
    else
    begin
      let onlyone = (List.length bf.bf_childs) = 1 in
      let init = bf.bf_time <> FT_0 in
      let branch_compl =
        (bf.bf_type = BF_branch) && 
        (bf.bf_name = "CASE_LIST" &&
          (
            let def = ref false in
            List.iter (fun bf' ->
              if bf'.bf_name = "SELECT_DEF" then def := true;
              ) bf.bf_childs;
            !def
          )  
        ) in
      let ftn = ref (if is_bind then FT_0 else 
                     if onlyone && init then  FT_minmax (FT 0,FT_0)
                     else FT_minmax (FT_0,FT_0)) in
      

      List.iter (fun bf' ->
        let is_branch' = 
          (*
          ** Select cases, special treatment!
          *)
          (bf'.bf_type = BF_branch) && is_branch && not onlyone in
        let eval f n n' =
          match f with
          | Min -> min n n';
          | Max -> max n n' in

        let rec cmp_ft f ft ft' =
          let ft,ft' = compact_time ft,
                       compact_time ft' in
          match ft' with
          | FT_0 -> ft;
          | FT n' -> 
            let rec cmp f ft =
              match ft with
              | FT_0 -> FT n';
              | FT n -> FT (eval f n n');
              | FT_min n -> 
                if f = Min then
                  (if n <= n' then FT_min n else FT n')
                else
                  FT_min (max n n');
              | FT_max n -> 
                if f = Max then
                  (if n > n' then FT_max n else FT n')
                else
                  FT_max (min n n');
              | FT_minmax (ft1,ft2) -> 
                FT_minmax (cmp Min ft1,cmp Max ft2)
              | _ -> error 733195 "" in
            cmp f ft;
          | FT_min n' -> 
            let rec cmp f ft =
              match ft with
              | FT_0 -> FT_min n';
              | FT n -> 
                if f = Min then
                  (if n' <= n then FT_min n' else FT n)
                else
                  FT_min (max n n');
              | FT_min n -> FT_min (eval f n n');
              | FT_max n ->
                if f = Min then
                  FT_max (min n n')
                else
                  FT_min (max n n');
              | FT_minmax (ft1,ft2) -> 
                FT_minmax (cmp Min ft1,cmp Max ft2)
              | _ -> error 733196 "" in
            cmp f ft;
          | FT_max n' -> 
            let rec cmp f ft =
              match ft with
              | FT_0 -> FT_max n';
              | FT n -> 
                if f = Max then
                  (if n' > n then FT_max n' else FT n)
                else
                  FT_max (min n n');
              | FT_min n -> 
                if f = Min then
                  FT_max (min n n')
                else
                  FT_min (max n n');
              | FT_max n -> FT_max (eval f n n');
              | FT_minmax (ft1,ft2) -> 
                FT_minmax (cmp Min ft1,cmp Max ft2)
              | _ -> error 733197 "" in
            cmp f ft;
          | FT_minmax (ft1',ft2') -> 
            let rec cmp f ft =
              match ft with
              | FT_0 -> FT_minmax (ft1',ft2');
              | FT _ 
              | FT_min _ 
              | FT_max _ ->
                cmp_ft Max ft ft2';
              | FT_minmax (ft1,ft2) ->
                if not branch_compl then
                  FT_minmax (cmp_ft Min ft1 ft1',
                             cmp_ft Max ft2 ft2')
                else
                  FT_minmax (cmp_ft Min ft1 ft2',
                             cmp_ft Max ft2 ft2');
              | _ -> error 733198 "" in
            cmp f ft;
          | _ -> print_frame_tree bf; error 716609 "" in
          
        ftn := cmp_ft Max !ftn bf'.bf_time;

        ) bf.bf_childs;
      bf_add_time bf !ftn;
    end;
    if is_loop then
    begin
      bf.bf_time <- (
          match !tmp_bf with
          | FT_list [i;FT_n (n,ft')] ->
            let count =
                match bf.bf_loop with
                | (FT a,FT b) -> FT (b-a+1);
                | (FT_0,FT b) -> FT_max b;
                | _ -> FT_min 1;
                in
            let ft'' = add_time ft' bf.bf_time in
            FT_list [i;FT_n (count,ft'')];
          | FT_n (n,ft') -> 
            let ft'' = add_time ft' bf.bf_time in
            let count =
                match bf.bf_loop with
                | (FT a,FT b) -> FT (b-a+1);
                | (FT_0,FT b) -> FT_max b;
                | _ -> FT_min 1;
                in
            FT_n (count,ft'');
          | _ -> !tmp_bf); 
      tmp_bf := FT_0;
    end; 
    in

  let rec iter bfl =
    match bfl with
    | bf::tl ->
        iter bf.bf_childs;
        calc_block_time bf;
        iter tl;
    | [] -> () in
  iter [root]



let print_frame_trees modu =
  let fname = of_mod (modu.mod_name^".ft") in
  info (sprintf "Emitting block frame informations in file <%s>..." fname);
  let ok = protects(vhdl_oc := open_out (sprintf "%s%s" compiler.t_output fname)) in
  if not ok then error 0 (sprintf "Can't open frame time file <%s>!" fname);
  out_stdout := false;
  
  out "Block frame trees";
  out "-----------------";
  List.iter (fun pro ->

    if !out_stdout then info (sprintf "  For process <%s>..." pro.pro_name);
    out (sprintf "For process <%s>:" pro.pro_name);
    ind_incr ();
    let root = 
      match pro.pro_instr with
        | (PI_block (_,bf))::tl -> bf;
        | _ -> error 0 "Can't find root frame" in

    print_frame_tree root;
    ind_decr ();
    ) modu.mod_procs;
  close_out !vhdl_oc;
  out_stdout := true
    
