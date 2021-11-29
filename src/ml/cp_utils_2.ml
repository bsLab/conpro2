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
**    $CREATED:     16.4.2006
**    $VERSION:     2.06
**
**    $INFO:
**
**  Utils
**
**    $ENDOFINFO
**
*)

open Unix
open Printf

open Cp_syntax
open Cp_types
open Cp_symbol
open Cp_common
open Cp_utils_1
open Cp_data_core
open Cp_vhdl



(*
** Calculate array index from multidimensional array selector.
*)
let at_index at sel =
    let ind = ref sel.(0) in
    let n = Array.length sel in
    for i = 1 to n-1
    do
        ind := !ind + sel.(i)*at.at_dim.(i-1);
    done;
    !ind

let at_dimi at num dim =
    let sel_size = proda at.at_dim in
    let sel_width = sel_width sel_size in
    let mul = ref 1 in
    for i = 1 to dim-1
    do
        mul := !mul * at.at_dim.(i-1);
    done;
    let id = num * !mul in
    sprintf "%s" (val_str (DT_logic sel_width) (V_int (Int64.of_int id)))

let at_dims at reg dim =
    if dim = 1 then sprintf "(%s)" reg else
    begin
        let sel_size = proda at.at_dim in
        let sel_width = 
                    let w = const_width (V_int (Int64.of_int sel_size)) in
                    let s' = 1 lsl (w-1) in
                    if s' = sel_size then (w-1) else w in
        let rec w dim =           
          if dim = 0 then 1 else
          (const_width (V_int (Int64.of_int at.at_dim.(dim-1))))*
          (w (dim-1)) in
        let s = (w (dim-1))-1 in
        (*
        ** at_dim must be a power of 2!
        *)
        sprintf "(%s & %s)" 
                reg 
                (zeros (sel_width - s))
    end

let at_dimsv at reg dim =
    if dim = 1 then sprintf "(%s)" reg else
    begin
        let sel_size = proda at.at_dim in
        let sel_width = 
                    let w = const_width (V_int (Int64.of_int sel_size)) in
                    let s' = 1 lsl (w-1) in
                    if s' = sel_size then (w-1) else w in
        let rec w dim =           
          if dim = 0 then 1 else
          (const_width (V_int (Int64.of_int at.at_dim.(dim-1))))*
          (w (dim-1)) in
        let s = (w (dim-1))-1 in
        (*
        ** at_dim must be a power of 2!
        *)
        sprintf "(%s & %s)" 
                reg 
                (zeros s)
    end
    


let rec ao_of_ot opl ot =
    match ot with
    | OT_object ao -> ao;
    | OT_queue qu -> qu.qu_ao;
    | OT_channel ch -> ch.ch_ao;
    | OT_array at ->
      if is_sel opl then
        ao_of_ot [] at.at_objs.(at_index at (obj_sel opl))
      else
        ao_of_ot [] at.at_objs.(0);
    | _ -> error 876173 "" 

let ao_of_sym modu sym =
  let ao name rules ta_name =
              {
                ao_name = name;
                ao_module = modu;
                ao_type = {
                    ta_name = ta_name;
                    ta_rules = rules;
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
              } in
  OT_object (match sym with
  | Sym_obj ot ->
  begin
    match ot with
    | OT_reg co -> ao co.co_name (get_some co.co_rules) "Register";
    | _ -> error 0 (sprintf "ao_of_sym: Can't derive abstract object from symbol.")
  end;
  | Sym_block db ->  ao db.db_name (get_some db.db_rules) "Ram";
  | _ -> error 0 (sprintf "ao_of_sym: Can't derive abstract object from symbol."))
              
let rec co_of_ud ud =
  match ud with
  | UC_reg uo 
  | UC_var uo 
  | UC_chan uo
  | UC_queue uo
  | UC_sig uo -> uo.uo_obj;
  | UC_sel us -> co_of_ud us.us_obj; 
  | UC_temp ut -> ut.ut_obj;
  | _ -> None

let rec uo_of_ud ud =
  match ud with
  | UC_reg uo 
  | UC_var uo
  | UC_chan uo
  | UC_queue uo
  | UC_sig uo -> Some uo;
  | UC_sel us -> uo_of_ud us.us_obj;
  | _ -> None

let dt_of_ud ud =
  match co_of_ud ud with
  | Some co -> co.co_type;
  | None -> error 353575 ""

(*
** Find maximal sized DT from uc_data list (part of expression for example)
** Returns MAX(uo_expr_type) DT -> what's required!
*)
let dt_of_udl udl =
    let dt = ref None in
    let add_dt d2 = 
      match !dt with
      | Some d1 ->
      begin
        match d2 with
        | DT_logic n -> dt := Some (DT_logic (max n (size_of_dt d1))); 
        | DT_int n -> dt := Some (DT_int (max n (size_of_dt d1))); 
        | DT_bool -> ();
        | DT_char -> dt := Some (DT_logic (max 8 (size_of_dt d1)));
        | _ -> (); 
      end;
      | None -> 
            dt := Some d2;
      in
    List.iter (fun ud ->
        match ud with
        | UC_temp ut -> add_dt ut.ut_type.uo_expr_type;
        | UC_alu ua -> if ua.ua_type <> DT_bool then
                            add_dt ua.ua_type;
        | _ ->
        begin
          match uo_of_ud ud with
          | Some uo -> add_dt uo.uo_type.uo_expr_type;
          | None -> ();
        end;
        ) udl;
    match !dt with
    | Some dt -> dt;
    | None -> DT_logic 0

(*
** Apply (new) expression type conversion to UD
*)
let ud_conv ud et =
  let rec get_last l =
    match l with
    | hd::tl -> if tl = [] then hd else get_last tl;
    | [] -> error 492577 "" 
    in
                      
  let fix_uo uo = uo.uo_type.uo_expr_type <- et in
  let fix_ut ut = ut.ut_type.uo_expr_type <- et in
  let fix_val uv = uv.uv_type.uo_expr_type <- et in

  match uo_of_ud ud with    
  | Some uo -> fix_uo uo;
  | None ->
  begin
    match ud with
    | UC_temp ut -> fix_ut ut;
    | UC_val uv -> fix_val uv;
    | _ -> ()
  end
  

(*
** Returns: (rd,wr) guard check
*)
let array_guard at =
    let is_block = List.mem AT_block at.at_flags in
    let is_dyn = List.mem AT_dyn at.at_flags in

    if is_block then true,true 
    else if is_dyn then
    begin
        let gd_rd,gd_wr = ref false, ref false in
        Array.iter (fun ot ->
            match ot with
            | OT_reg co ->                          
            begin
              match co.co_guard with    
              | Some gd ->
                    gd_wr := !gd_wr or (List.mem GD_wr gd.gd_req);
              | None -> ();                                       
            end;           
            | OT_channel ch ->
            begin
              match ch.ch_obj.co_guard with    
              | Some gd ->
                    gd_wr := !gd_wr or (List.mem GD_wr gd.gd_req);
                    gd_rd := !gd_rd or (List.mem GD_rd gd.gd_req);
              | None -> ();                                       
            end;                          
            | OT_queue qu ->
            begin
              match qu.qu_obj.co_guard with    
              | Some gd ->
                    gd_wr := !gd_wr or (List.mem GD_wr gd.gd_req);
                    gd_rd := !gd_rd or (List.mem GD_rd gd.gd_req);
              | None -> ();                                       
            end;                          
            | _ -> error 503742 "";
            ) at.at_objs;          
        !gd_rd, !gd_wr
    end
    else false,false


let rec ud_guard ud = 
  match ud with
  | UC_chan _
  | UC_queue _
  | UC_var _ -> true,true;
  | UC_reg uo ->
  begin
    match uo.uo_obj with
    | Some co ->
    begin
      match co.co_guard with    
      | Some gd ->
        (List.mem GD_rd gd.gd_req),
        (List.mem GD_wr gd.gd_req)
      | None -> false,false;
    end;
    | None -> false,false;
  end;
  | UC_sel us ->
      array_guard us.us_array
  | UC_array uat ->
      array_guard uat.uat_obj
  | _ -> false,false 

(*
** Returns true if object is fragmented (variables only)
*)
let ud_frag ud = 
  match ud with
  | UC_var uo -> 
  begin
    match co_of_ud ud with
    | Some co -> co.co_subsize > 1;
    | None -> false;
  end;
  | _ -> false

let ud_temp ud =
  match ud with
  | UC_temp _ -> true;
  | _ -> false

let rec ud_local ud =
  let is_local uo = List.mem UO_loc uo.uo_flags in
  match ud with
  | UC_temp _ -> true;
  | UC_reg uo -> is_local uo;
  | UC_var uo -> is_local uo;
  | UC_sel us -> ud_local us.us_obj;
  | _ -> false

let ud_name ud =
  match ud with
  | UC_temp ut -> ut.ut_name;
  | UC_sel us -> us.us_name;
  | _  ->
  begin
    match uo_of_ud ud with
    | Some uo -> uo.uo_name;
    | None -> "" 
  end

let ud_val ud =
  match ud with
  | UC_val uv -> uv.uv_val;
  | _ -> error 141140 ""

let ud_fix_rhs ud =
    match ud with
    | UC_reg uo -> 
        let uo_flags = (List.filter (fun f -> f <> UO_lhs) uo.uo_flags)
                       @ (if List.mem UO_rhs uo.uo_flags  
                            then [] else [UO_rhs]) in
        UC_reg {uo with uo_flags=uo_flags};
    | UC_sig uo -> 
        let uo_flags = (List.filter (fun f -> f <> UO_lhs) uo.uo_flags)
                       @ (if List.mem UO_rhs uo.uo_flags  
                            then [] else [UO_rhs]) in
        UC_sig {uo with uo_flags=uo_flags};
    | UC_var uo -> 
        let uo_flags = (List.filter (fun f -> f <> UO_lhs) uo.uo_flags)
                       @ (if List.mem UO_rhs uo.uo_flags  
                            then [] else [UO_rhs]) in
        UC_var {uo with uo_flags=uo_flags};
    | UC_alu ua ->
        let ua_flags = (List.filter (fun f -> f <> UO_lhs) ua.ua_flags)
                       @ (if List.mem UO_rhs ua.ua_flags  
                            then [] else [UO_rhs]) in
        UC_alu {ua with ua_flags=ua_flags};
    | UC_temp ut ->
        let ut_flags = (List.filter (fun f -> f <> UO_lhs) ut.ut_flags)
                       @ (if List.mem UO_rhs ut.ut_flags  
                            then [] else [UO_rhs]) in
        UC_temp {ut with ut_flags=ut_flags};
    | _ -> ud

let ud_fix_lhs ud =
    match ud with
    | UC_reg uo -> 
        let uo_flags = (List.filter (fun f -> f <> UO_rhs) uo.uo_flags)
                       @ (if List.mem UO_lhs uo.uo_flags  
                            then [] else [UO_lhs]) in
        UC_reg {uo with uo_flags=uo_flags};
    | UC_sig uo -> 
        let uo_flags = (List.filter (fun f -> f <> UO_rhs) uo.uo_flags)
                       @ (if List.mem UO_lhs uo.uo_flags  
                            then [] else [UO_lhs]) in
        UC_sig {uo with uo_flags=uo_flags};
    | UC_var uo -> 
        let uo_flags = (List.filter (fun f -> f <> UO_rhs) uo.uo_flags)
                       @ (if List.mem UO_lhs uo.uo_flags  
                            then [] else [UO_lhs]) in
        UC_var {uo with uo_flags=uo_flags};
    | UC_alu ua ->
        let ua_flags = (List.filter (fun f -> f <> UO_rhs) ua.ua_flags)
                       @ (if List.mem UO_lhs ua.ua_flags  
                            then [] else [UO_lhs]) in
        UC_alu {ua with ua_flags=ua_flags};
    | UC_temp ut ->
        let ut_flags = (List.filter (fun f -> f <> UO_rhs) ut.ut_flags)
                       @ (if List.mem UO_lhs ut.ut_flags  
                            then [] else [UO_lhs]) in
        UC_temp {ut with ut_flags=ut_flags};
    | _ -> ud

let ud_default ud =
  match co_of_ud ud with
  | Some co -> co.co_default;
  | None -> V_int Int64.zero
  
let rec ud_change_sign ud uo_sign =
    let fix uo_type =
      let s = uo_type.uo_sign in
      {uo_type with uo_sign=uo_sign},s in
    match ud with
    | UC_reg uo -> 
        let t,s = fix uo.uo_type in
        UC_reg {uo with uo_type=t},s
    | UC_sig uo -> 
        let t,s = fix uo.uo_type in
        UC_sig {uo with uo_type=t},s
    | UC_var uo -> 
        let t,s = fix uo.uo_type in
        UC_var {uo with uo_type=t},s
    | UC_temp ut ->
        let t,s = fix ut.ut_type in
        UC_temp {ut with ut_type=t},s
    | UC_sel us ->
        let ud',s = ud_change_sign us.us_obj uo_sign in
        UC_sel {us with us_obj=ud'},s
    | _ -> ud,uo_sign
  

let pi_frag pi = 
  match ot_of_pi pi with
  | OT_var co -> co.co_subsize > 1;
  | _ -> false


    
let is_value ot =
    match ot with
    | OT_named_value _
    | OT_value _ -> true;
    | _ -> false
let get_value ot =
    match ot with
    | OT_named_value (_,v)
    | OT_value v -> v;
    | _ -> error 291877 ""
let is_pi_value pi =
    match pi with
    | PI_obj (_,ot) -> is_value ot;
    | _ -> false
let is_const ot =
    match ot with
    | OT_const _ -> true;
    | _ -> false


let is_core ot =
    not ((dt_of_ot ot) = None)

   
let print_timeunit u =
    match u with
    | Nsec -> "ns";
    | Usec -> "us";
    | Msec -> "ms";
    | Sec -> "s";
    | Cycles -> "" 

let print_frequnit u =
    match u with
    | Ghz -> "GHz";
    | Mhz -> "MHz";
    | Khz -> "KHz";
    | Hz -> "Hz"

let is_mod_pro name =
  let n = String.length name in
  if n < 5 then false else
    (String.sub name 0 4)="MOD_" 
    


(*
** Create address selector function (UC_var) from OT (OT_reg,OT_val)
*)
let set_addr_sel at ud ot_sell =
  match ud with
  | UC_var uo ->
  begin
    let db = 
      match uo.uo_block with
      | Some db -> db;
      | None -> error 341559 "";
      in
    let n = const_width (V_int (i64 (max 2 (db.db_size-1)))) in
    let sel_str = ref "" in
    let val_addr_fun v =
          val_str (DT_logic n) v;
        in 
    let addr_add v n =
        match v with
        | V_int n' -> V_int (Int64.add n n');
        | _ -> error 388015 "";
        in
    let co = 
        match uo.uo_obj with
        | Some co -> co;
        | None -> error 792157 "";
        in
    let add_off off =
        if off = 0 then ""
        else sprintf " + %d" off in

    let first = ref true in
    let add_sel f =
        if !first then
        begin
            first := false;
            uo.uo_addr <- f;
        end
        else 
        begin
          let f' = uo.uo_addr in
          uo.uo_addr <- (fun off -> sprintf "(%s + %s)" 
                                               (f' off)
                                               (f off))
        end in
    let addr0 = i64 co.co_index in
    let dim = ref 0 in

    List.iter (fun ot_sel ->
     incr dim;
     let d = !dim in
     match ot_sel with
     | OT_reg co_sel ->
     begin
      let uo_type = uo_type co_sel.co_type co_sel.co_type in
      let nulladdr = (fun _ -> "") in
      uo.uo_sel <- uo.uo_sel @ [UC_reg {
                uo_name = co_sel.co_name;
                uo_obj = Some co_sel;
                uo_type = uo_type;
                uo_range = None;
                uo_index = None;
                uo_addr = nulladdr;
                uo_sel = [];
                uo_block = None;
                uo_flags = [UO_rhs];
            }];
    
      let is_local = (co_sel.co_process <> None) or (List.mem Obj_local co_sel.co_flags) in
      let sel_name =
        if is_local then co_sel.co_name else
            sprintf "REG_%s_rd" co_sel.co_name in

      (*
      ** We must provide VHDL output here!!!
      *)
      add_sel (fun off ->
        match co_sel.co_type with
        | DT_logic n' -> 
          if addr0 = Int64.zero or d > 1 then
          begin
            let rec w dim =           
              if dim = 0 then 1 else
              (const_width (V_int (Int64.of_int at.at_dim.(dim-1))))*
              (w (dim-1)) in
            let s = (w (d-1))-1 in
            let n'' = n - s in
            let r_sig =
              if n'' = n' then
                sprintf "%s%s" sel_name   
                               (add_off off)
              else if n'' > n' then
                sprintf "(%s & %s)%s" (zeros (n''-n')) 
                                      sel_name
                                      (add_off off)
              else
                sprintf "%s(%d downto 0)%s" sel_name 
                                            (n''-1)
                                            (add_off off);
              in
            at_dimsv at r_sig d 
          end
          else
          begin
            let add = val_str (DT_logic n) (V_int 
                                            (Int64.add addr0 (i64 off))) in
            let rec w dim =           
              if dim = 0 then 1 else
              (const_width (V_int (Int64.of_int at.at_dim.(dim-1))))*
              (w (dim-1)) in
            let s = (w (d-1))-1 in
            let n'' = n - s in
            let r_sig =
              if n'' = n' then
                sprintf "%s + %s" 
                        sel_name add
              else if n'' > n' then
                sprintf "(%s & %s) + %s" (zeros (n''-n')) 
                        sel_name add
              else
                sprintf "%s(%d downto 0) + %s" 
                        sel_name (n''-1) add;
              in
            at_dimsv at r_sig d 
          end;
        | DT_int n' -> 
          if addr0 = Int64.zero or d > 1 then
          begin
            let rec w dim =           
              if dim = 0 then 1 else
              (const_width (V_int (Int64.of_int at.at_dim.(dim-1))))*
              (w (dim-1)) in
            let s = (w (d-1))-1 in
            let n'' = n - s in
            let r_sig =
              if n'' = n' then
                sprintf "std_logic_vector(%s)%s" sel_name
                        (add_off off)
              else if n'' > n' then
                sprintf "(%s & std_logic_vector(%s))%s" 
                        (zeros (n''-n')) 
                        sel_name
                        (add_off off)
              else
                sprintf "std_logic_vector(%s(unsigned(%s),%d))%s" 
                        (vhdl_map "resize")
                        sel_name n''
                        (add_off off);
              in
            at_dimsv at r_sig d 
          end
          else
          begin
            let add = val_str (DT_logic n) (V_int 
                              (Int64.add addr0 (i64 off))) in
            let rec w dim =           
              if dim = 0 then 1 else
              (const_width (V_int (Int64.of_int at.at_dim.(dim-1))))*
              (w (dim-1)) in
            let s = (w (d-1))-1 in
            let n'' = n - s in
            let r_sig =
              if n'' = n' then
                sprintf "std_logic_vector(%s) + %s" 
                        sel_name add
              else if n'' > n' then
                sprintf "(%s & std_logic_vector(%s)) + %s" 
                        (zeros (n''-n')) sel_name add
              else
                sprintf "std_logic_vector(%s(unsigned(%s),%d)) + %s" 
                        (vhdl_map "resize")
                        sel_name n'' add;
              in
            at_dimsv at r_sig d 
          end
        | _ -> error 202061 "";
        );
     end;  
     | OT_const co ->
      uo.uo_sel <- uo.uo_sel @ [UC_val {
                uv_type = uo_type co.co_type co.co_type;
                uv_val = co.co_init;
            }];
      add_sel (fun off ->
                (val_addr_fun (addr_add co.co_init 
                        (Int64.add addr0 (i64 off))))
                );            
     | OT_named_value (_,v)
     | OT_value v ->
	  let dt = dt_of_val v in
      uo.uo_sel <- uo.uo_sel @ [UC_val {
                uv_type = uo_type dt dt;
                uv_val = v;
            }];
      add_sel (fun off ->
                (val_addr_fun (addr_add v 
                        (Int64.add addr0 (i64 off))))
                );    
     | _ -> error 811754 "";
    ) ot_sell;
  end;
  | _ -> error 253615 ""
