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
**    $INITIAL:     (C) 2006-2011 BSSLAB
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
open Cp_utils_2
open Cp_utils_3
open Cp_data_core

(*       
** Convert arithmetic, logical and boolean operators to state type
*)       
let op_type op =
    match op with
    | "+" -> OP_add;
    | "-" -> OP_sub;
    | "*" -> OP_mul;
    | "/" -> OP_div;
    | "asl" -> OP_asl;
    | "asr" -> OP_asr;
    | "and" -> OP_band;
    | "or" -> OP_bor;
    | "xor" -> OP_bxor;
    | "not" -> OP_bnot;
    | "land" -> OP_land;
    | "lor" -> OP_lor;
    | "lxor" -> OP_lxor;
    | "lnot" -> OP_lnot;
    | "lsl" -> OP_lsl;
    | "lsr" -> OP_lsr;
    | "max" -> OP_max;
    | "min" -> OP_min;
    | "=" -> OP_eq;
    | "<>" -> OP_neq;
    | "<" -> OP_lt;
    | ">" -> OP_gt;
    | ">=" -> OP_ge;
    | "<=" -> OP_le;
    | _ -> error 0 (sprintf "Unexpected operator <%s> found." op)


let op_name op =
    match op with
    | OP_add -> "+";
    | OP_sub -> "-";
    | OP_mul -> "*";
    | OP_div -> "/";
    | OP_asl -> "asl";
    | OP_asr -> "asr";
    | OP_band -> "and";
    | OP_bor -> "or";
    | OP_bxor -> "xor";
    | OP_bnot -> "not";
    | OP_land -> "land";
    | OP_lor -> "lor";
    | OP_lxor -> "lxor";
    | OP_lnot -> "lnot";
    | OP_lsl -> "lsl";
    | OP_lsr -> "lsr";
    | OP_max -> "max";
    | OP_min -> "min";
    | OP_eq -> "=";
    | OP_neq -> "<>";
    | OP_lt -> "<"; 
    | OP_gt -> ">"; 
    | OP_ge -> ">=";
    | OP_le -> "<=";
    | OP_alu -> "@ALU";
    | OP_nop -> "@NOP"




(*
** Convert arithmetic, logical and boolean operators to ALU state name
** specifier.
*)
let alu_state_str op =
    match op with
    | OP_add -> "Alu_add";
    | OP_sub -> "Alu_sub";
    | OP_mul -> "Alu_mul";
    | OP_div -> "Alu_div";
    | OP_asr -> "Alu_asr";
    | OP_asl -> "Alu_asl";
    | OP_land -> "Alu_land";
    | OP_lor -> "Alu_lor";
    | OP_lxor -> "Alu_lxor";
    | OP_lnot -> "Alu_lnot";
    | OP_band -> "Alu_band";
    | OP_bor -> "Alu_bor";
    | OP_bxor -> "Alu_bxor";
    | OP_bnot -> "Alu_bnot";
    | OP_lsl -> "Alu_lsl";
    | OP_lsr -> "Alu_lsr";
    | OP_max -> "Alu_max";
    | OP_min -> "Alu_min";
    | OP_eq -> "Alu_eq";
    | OP_neq -> "Alu_neq";
    | OP_lt -> "Alu_lt";
    | OP_gt -> "Alu_gt";
    | OP_ge -> "Alu_ge";
    | OP_le -> "Alu_le";
    | OP_nop -> "Alu_nop";
    | _ -> error 97781 ""

(*
** Convert arithmetic, logical and boolean operators to VHDL function.
*)

let op_vhdl op =
    match op with
    | OP_add -> "+";
    | OP_sub -> "-";
    | OP_mul -> "*";
    | OP_div -> "/";
    | OP_asr -> "sra";
    | OP_asl -> "sla";
    | OP_band -> "and";
    | OP_bor -> "or";
    | OP_bxor -> "xor";
    | OP_bnot -> "not";
    | OP_land -> "and";
    | OP_lor -> "or";
    | OP_lxor -> "xor";
    | OP_lnot -> "not";
    | OP_lsl -> "sll";
    | OP_lsr -> "srl";
    | OP_max -> "max";
    | OP_min -> "min";
    | OP_eq -> "=";
    | OP_neq -> "/=";
    | OP_lt -> "<";
    | OP_gt -> ">";
    | OP_ge -> ">=";
    | OP_le -> "<=";
    | OP_nop -> "null";
    | _ -> error 907994 ""
    

(*
** Return kind of operation
*)
let op_mode op =
    match op with
    | OP_add 
    | OP_sub 
    | OP_mul 
    | OP_div 
    | OP_max
    | OP_min
    | OP_asr 
    | OP_asl -> OP_arith;
    | OP_land 
    | OP_lor 
    | OP_lxor 
    | OP_lnot -> OP_logic;
    | OP_lsl 
    | OP_lsr -> OP_logic;   (* but special treatment required *)
    | OP_eq 
    | OP_neq 
    | OP_lt 
    | OP_gt 
    | OP_ge 
    | OP_le -> OP_relat;
    | OP_band 
    | OP_bor 
    | OP_bxor 
    | OP_bnot -> OP_bool
    | _ -> error 674340 ""

(*
** Level of operator binding
*)
let op_level op =
    match op with
    | OP_add 
    | OP_sub -> 1; 
    | OP_mul -> 2;
    | OP_asr 
    | OP_asl -> 2;
    | OP_land -> 2;
    | OP_lor 
    | OP_lxor -> 1; 
    | OP_lnot -> 3;
    | OP_lsl 
    | OP_lsr -> 2;
    | OP_eq 
    | OP_neq 
    | OP_lt 
    | OP_gt 
    | OP_ge 
    | OP_le -> 1;
    | OP_band -> 2;
    | OP_bor 
    | OP_bxor -> 1
    | OP_bnot -> 3
    | _ -> 0


(*
** Some operators require special treatment
*)
let op_special op =
    match op with
    | OP_lsl 
    | OP_lsr -> true;
    | _ -> false

 

(*
** Find next unused and best matching process temporary register with
** respect to data type of obj (dt).
*)
let new_tmp' temps dt =
    debug "new_tmp'" with (sprintf "new_tmp': dt=<%s>" (sprint_dt dt));
    let best_fit = ref None in
    let best_size = ref 0 in
    let tid = td_of_dt dt in
    let size = size_of_dt dt in
    List.iter (fun co' ->
        let tid' = td_of_dt co'.co_type in
        let size' = size_of_dt co'.co_type in

        if not (List.mem Obj_inuse co'.co_flags) &&
           tid = tid' then
        begin
            if  abs (size' - size) <=
                abs (!best_size - size) ||
                !best_size = 0
            then
            begin
                best_fit := Some co';
                best_size := size';
            end;
        end
        else debug "new_tmp'" with (sprintf "new_tmp: skipped <%s>" co'.co_name);
      ) !temps;
    if !best_fit = None then
    begin
        let tmpnum=List.length !temps in
        let co' = {
                    co_name = sprintf  "TEMP_%d" tmpnum;
                    co_process = an.a_pro;
                    co_module = an.a_modu;
                    co_rules = None;
                    co_type = dt;
                    co_type_name = "";
                    co_level = 0;
                    co_init = V_null;
                    co_default = V_int Int64.zero;
                    co_guard = None;
                    co_reader = [];
                    co_writer = [];
                    co_array = [];  
                    co_struct = [];                             
                    co_index = 0;
                    co_size = 1;
                    co_subsize=1;
                    co_block = None;
                    co_flags = [Obj_temp;Obj_inuse;Obj_local];
                    co_bittype = None;
                    co_domain = an.a_modu.mod_name;
                  } in
        debug "new_tmp'" with (sprintf "new_tmp: created <%s> for size %d"  co'.co_name size );
(*
        sym_add pro.pro_objs (Sym_obj (OT_reg co'));
*)
        temps := !temps @ [co'];
        co'
    end
    else
    begin
        let co' = get_some !best_fit in

        debug "new_tmp'" with (sprintf "new_tmp: found <%s> for size %d"  co'.co_name size);

        (*
        ** Adjust size if necessary.
        *)
        let size' = size_of_dt co'.co_type  in
        if size' < size then
            co'.co_type <- dt_of_td (td_of_dt co'.co_type) size;
        co'.co_flags <- co'.co_flags @ [Obj_inuse];
        co'
    end

let new_tmp pro dt =
    debug "new_tmp" with (sprintf "new_tmp: pro=<%s> dt=<%s>" pro.pro_name (sprint_dt dt));
    let temps = ref pro.pro_temps in
    let tmp = new_tmp' temps dt in
    pro.pro_temps <- !temps;
    tmp.co_process <- Some pro;
    tmp.co_module <- pro.pro_module;
    tmp.co_reader <- [pro];
    tmp.co_writer <- [pro];
    tmp.co_domain <- pro.pro_domain;
    tmp

(*
** Release temporary register if it can be shared and reused.
*)
let release_tmp pro co =
    debug "release_tmp" with (sprintf "release_tmp: pro=<%s> co=<%s> dt=<%s>" pro.pro_name co.co_name (sprint_dt co.co_type));
    let shared =
        let m = ref "shared" in
        List.iter (fun p ->
        match p with
        | BP_temp s -> m := s;
        | _ -> ();
        ) (List.hd pro.pro_constr).bc_params;
        !m = "shared" in
    if shared then        
        co.co_flags <- List.filter (fun f ->
                                f <> Obj_inuse
                            ) co.co_flags

(*
** Same as above but there is a best_fit one already!
*)
let get_tmp pro dt =
    debug "get_tmp" with (sprintf "get_tmp: pro=<%s> dt=<%s>" pro.pro_name (sprint_dt dt));
    let best_fit = ref None in
    let best_size = ref 0 in
    let tid = td_of_dt dt in
    let size = size_of_dt dt in
    debug "get_tmp"  with (sprintf "get_tmp for size %d..." size);
    List.iter (fun co' ->
        let tid' = td_of_dt co'.co_type in
        let size' = size_of_dt co'.co_type in

        if not (List.mem Obj_inuse co'.co_flags) &&
           tid = tid' then
        begin
            if  (abs (size' - size) <=
                 abs (!best_size - size) &&
                 size' >= size) ||
                (!best_size = 0 &&
                 size' >= size)
            then
            begin
                best_fit := Some co';
                best_size := size';
            end;
        end;
      ) pro.pro_temps;
    if !best_fit <> None then
    begin
        let co' = get_some !best_fit in
        let tid' = td_of_dt co'.co_type in
        let size' = size_of_dt co'.co_type in
        debug "get_tmp" with (sprintf "get_tmp: found <%s.%s[%d]> for size %d"  
                             pro.pro_name co'.co_name
                             size' size);
        (*
        ** Adjust size if necessary.
        *)
(*
        let size' = size_of_dt co'.co_type  in
        if size' < size then
            co'.co_type <- dt_of_td (td_of_dt co'.co_type) size;
*)
        co'.co_flags <- co'.co_flags @ [Obj_inuse];
        co'
    end
    else
        error 744663 ""

(*
** Find next unused and best matching register (regular process object with
** name prefix "TEMPS_") with respect to data type of obj (dt).
*)
let tmp_reg pro dt =
    debug "tmp_reg" with (sprintf "tmp_reg: pro=<%s> dt=<%s>" pro.pro_name (sprint_dt dt));
    let temps = ref [] in

    List.iter (fun sym ->
        match sym with
        | Sym_obj (OT_reg co) ->
          let prefix = "TEMPS_" in
          let l = String.length prefix in
          let name = co.co_name in
          let lr = String.length name in
          if lr > l && (String.sub name 0 l) = prefix then
            temps := !temps @ [co];
        | _ -> ();
        ) (list_of_sym pro.pro_objs);

    let best_fit = ref None in
    let best_size = ref 0 in
    let tid = td_of_dt dt in
    let size = size_of_dt dt in

    List.iter (fun co' ->
        let tid' = td_of_dt co'.co_type in
        let size' = size_of_dt co'.co_type in

        if not (List.mem Obj_inuse co'.co_flags) &&
           tid = tid' then
        begin
            if  abs (size' - size) <=
                abs (!best_size - size) ||
                !best_size = 0
            then
            begin
                best_fit := Some co';
                best_size := size';
            end;
        end
        else debug "tmp_reg" with (sprintf "new_tmp: skipped <%s.%s>" pro.pro_name co'.co_name);
      ) !temps;
    if !best_fit = None then
    begin
        let tempnum = (List.length !temps) in
        let co' = {
                    co_name = sprintf  "TEMPS_%d" tempnum;
                    co_process = Some pro;
                    co_module = pro.pro_module;
                    co_rules = None;
                    co_type = dt;
                    co_type_name = "";
                    co_level = 0;
                    co_init = V_null;
                    co_default = V_int Int64.zero;
                    co_guard = None;
                    co_reader = [pro];
                    co_writer = [pro];
                    co_array = []; 
                    co_struct = [];                              
                    co_index = 0;
                    co_size = 1;
                    co_subsize=1;
                    co_block = None;
                    co_flags = [Obj_temp;Obj_inuse;Obj_local];
                    co_bittype = None;
                    co_domain = pro.pro_domain;
                  } in
        debug "tmp_reg" with (sprintf "new_tmp: created <%s.%s> for size %d"  
                             pro.pro_name co'.co_name size );
        sym_add pro.pro_objs (Sym_obj (OT_reg co'));
        co'
    end
    else
    begin
        let co' = get_some !best_fit in

        debug "tmp_reg" with (sprintf "new_tmp: found <%s.%s> for size %d" 
                              pro.pro_name co'.co_name size);

        (*
        ** Adjust size if necessary.
        *)
        let size' = size_of_dt co'.co_type  in
        if size' < size then
            co'.co_type <- dt_of_td (td_of_dt co'.co_type) size;
        co'.co_flags <- co'.co_flags @ [Obj_inuse];
        co'
    end



(*
** Create a new (auxilliary) signal of type dt
*)
let new_sig modu pro signame dt =
    let co = {
                    co_name = signame;
                    co_process = pro;
                    co_module = modu;
                    co_rules = None;
                    co_type = dt;
                    co_type_name = "";
                    co_level = 0;
                    co_init = V_null;
                    co_default = V_int Int64.zero;
                    co_guard = None;
                    co_reader = (
                        match pro with
                        | Some pro -> [pro];
                        | None -> []);
                    co_writer = (
                        match pro with
                        | Some pro -> [pro];
                        | None -> []);
                    co_array = [];  
                    co_struct = [];                             
                    co_index = 0;
                    co_size = 1;
                    co_subsize=1;
                    co_block = None;
                    co_flags = [Obj_temp;];
                    co_bittype = None;
                    co_domain = (if pro = None then modu.mod_name else
                                    (get_some pro).pro_domain);
                  } in
    match pro with
    | Some pro -> sym_add pro.pro_objs (Sym_obj (OT_signal co)); co;
    | None -> sym_add modu.mod_objs (Sym_obj (OT_signal co)); co

let new_reg modu pro name dt = 
   let co =
      {
        co_name = name;
        co_module = modu;
        co_rules = None;
        co_process = pro;
        co_type = dt;
        co_type_name = "";
        co_level = 0;
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
        co_domain = (if pro = None then modu.mod_name else
                        (get_some pro).pro_domain);
      } in
    match pro with
    | Some pro -> sym_add pro.pro_objs (Sym_obj (OT_reg co)); co;
    | None -> sym_add modu.mod_objs (Sym_obj (OT_reg co)); co


(*
** Return actual alu width threshold value.
*)
let alu_thres pro =
    let width = ref 0 in
    List.iter (fun p ->
        match p with
        | BP_inline 
        | BP_expr EXPR_binary
        | BP_expr EXPR_flat -> width := -1;
        | BP_expr EXPR_top -> width := -1;
        | BP_alu_min_width n -> if !width = 0 then
                                    width := n;
        | _ -> ();
        ) (List.hd pro.pro_constr).bc_params;
    if !width > 0 then !width else 1000000

let alu_use pro = 
    (List.mem (BP_expr EXPR_alu) (List.hd pro.pro_constr).bc_params) &&
    not (List.mem (BP_inline) (List.hd pro.pro_constr).bc_params)


(*
** Adjust (width, operator list) of existing ALU or create new ALU 
**
**  data_ops: ["+";...]
**  dt: 
**      data_size: bits required
**      data_type: type id
**  return: use_alu bool
*)

let check_alu pro data_ops dt =
  let data_type = td_of_dt dt in
  let data_size = size_of_dt dt in

  let alu_thres = alu_thres pro in
  let is_special = 
    let s = ref false in
    List.iter (fun op -> if op_special op then s:= true; ) data_ops;
    !s in
  let use_alu = (data_size >= alu_thres) && not is_special in
  debug "check_alu" with (sprintf "check_alu: size=%d use_alu=%b" data_size use_alu);

  if use_alu then
  begin
    let rec find_alu alul =
        match alul with
        | alu::tl ->
        begin
            (*
            ** Check type..
            *)
            let alu_td = td_of_dt alu.alu_type in
            let td = td_of_dt dt in
            if alu_td = td then
                Some alu
            else
                find_alu tl;
        end;
        | [] -> None;
        in
    match find_alu pro.pro_alu with
    | Some alu -> 
    begin
        (*
        ** Update operator list
        *)
        alu.alu_ops <-
            alu.alu_ops @ 
                (List.filter (fun o ->
                       o <> OP_alu && 
                       o <> OP_nop && 
                       not (List.mem o alu.alu_ops)
                    ) data_ops);
        (*
        ** Update data type
        *)
        match alu.alu_type with
        | DT_logic n -> 
            let n' = size_of_dt dt in
            alu.alu_type <- DT_logic (max n n');
        | DT_int n -> 
            let n' = size_of_dt dt in
            alu.alu_type <- DT_int (max n n');
        | _ -> error 964395 "";
    end;
    | None ->    
        let alu = {
                        alu_name = sprintf "%d" 
                                   ((List.length pro.pro_alu)+1);
                        alu_ops = (List.filter (fun o ->
                                       o <> OP_alu && 
                                       o <> OP_nop 
                                    ) data_ops);

                        alu_type = (if data_type = T_logic then 
                                        dt_of_td data_type data_size
                                    else
                                        DT_int data_size);
                    } in
        pro.pro_alu <- pro.pro_alu @ [alu];
  end;
  use_alu

let add_alu pro ops dt = __(check_alu pro ops dt)

(*
** Return best matching ALU (Type,Size)
*)
let find_alu pro dt =
    let rec search alul =
        match alul with
        | alu::tl ->
            let td = td_of_dt dt in
            let alu_td = td_of_dt alu.alu_type in
            if td = alu_td then
                alu
            else 
                search tl;
        | [] -> 
            error 686753 (sprintf "find_alu: no matching ALU found (%d)"
                    (id_of_dt dt));
        in
    search pro.pro_alu


