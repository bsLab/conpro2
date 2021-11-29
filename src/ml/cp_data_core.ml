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
**    $CREATED:     11.10.2007
**    $VERSION:     2.05
**
**    $INFO:
**
** Data management (microcode,vhdl): Basic operations
**
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

let uo_type dt et = {
  uo_data_type = dt;
  uo_expr_type = et; 
  uo_phys_type = None;
  uo_sub_type = None;     
  uo_log_type = None;
  uo_conv = None;
  uo_sign = None;
} 

(*
** Get copy of type descriptor for a 
** temporary register inference.
** Blank out physical type!
*)
let copy_uo_type src = {src with
  uo_data_type = src.uo_data_type;
  uo_expr_type = src.uo_expr_type; 
  uo_phys_type = None; (* src.uo_phys_type; *)
  uo_sub_type = src.uo_sub_type;     
  uo_log_type = src.uo_log_type;
  uo_conv = src.uo_conv;
  uo_sign = src.uo_sign;
} 

(*
** Return VHDL data type string
*)
let obj_decl_type dt =
    match dt with
    | DT_logic n -> 
        if n > 1 
            then sprintf "std_logic_vector(%d downto 0)" (n-1)
            else "std_logic";
    | DT_int n -> 
        if n < 2 then error 0 "obj_decl_type: width of type int <2";
        sprintf "signed(%d downto 0)" (n-1);
    | DT_bool -> "std_logic";
    | DT_char -> "std_logic_vector(7 downto 0)";
    | DT_natural n -> "integer";
    | _ -> error 411527 ""

let obj_size dt =
    match dt with
    | DT_logic n -> n;
    | DT_int n -> n;  
    | DT_char -> 8;   
    | _ -> error 456016 ""


(*
** Some utils for data and object types
*)
let dt_of_td td n =
    match td with
    | T_logic   -> DT_logic n;
    | T_int     -> DT_int n;
    | T_str  -> DT_string n;
    | T_char    -> DT_char;
    | T_bool    -> DT_bool;
    | _     -> failwith "dt_of_td"

let td_of_dt dt =
    match dt with
    | DT_logic _    -> T_logic;
    | DT_int _      -> T_int;
    | DT_string _   -> T_str;
    | DT_char       -> T_char;
    | DT_bool       -> T_bool;
    | DT_object _   -> T_object
    | DT_lneg
    | DT_aneg -> T_object
    | DT_natural _ -> T_natural

let id_of_td id =
    match id with
    | T_logic     -> 0x1;
    | T_int       -> 0x2;
    | T_str       -> 0x4;
    | T_char      -> 0x8;
    | T_bool      -> 0x10;
    | T_object    -> 0x20;
    | T_natural   -> 0x40

let id_of_dt dt = id_of_td (td_of_dt dt)

let rec to_of_ot ot =
    match ot with
    | OT_const  _   -> O_const;
    | OT_named_value _ -> O_named_value;
    | OT_signal _   -> O_signal;
    | OT_reg _      -> O_reg;
    | OT_var _      -> O_var;
    | OT_channel _  -> O_channel;
    | OT_queue _  -> O_queue;
    | OT_component _  -> O_component;
    | OT_struct _   -> O_struct;
    | OT_array  at  -> to_of_ot at.at_objs.(0);
    | OT_array_sel (at,sel) -> to_of_ot at.at_objs.(0); 
    | OT_object _   -> O_object;
    | OT_value _    -> O_value
    | OT_reference _ -> O_reference

let rec name_of_ot ot =
    match ot with
    | OT_const co   -> co.co_name;
    | OT_signal co  -> co.co_name;
    | OT_reg co     -> co.co_name;
    | OT_var co     -> co.co_name;
    | OT_channel ch -> ch.ch_obj.co_name;
    | OT_queue qu -> qu.qu_obj.co_name;
    | OT_component st  -> st.st_name;
    | OT_struct st  -> st.st_name;
    | OT_array  at  -> at.at_name;
    | OT_array_sel (at,sel) -> 
          sprintf "%s_%d" at.at_name sel;
    | OT_object ao  -> ao.ao_name;
    | OT_reference (r,_) -> r;
    | OT_named_value _
    | OT_value _    -> "%V"


let rec dt_of_ot ot =
    match ot with
    | OT_const  co  -> Some co.co_type 
    | OT_signal co  -> Some co.co_type;
    | OT_reg co     -> Some co.co_type;
    | OT_var co  -> Some co.co_type;
    | OT_channel ch -> 
    begin
        match ch.ch_ot with
        | Some ot' -> dt_of_ot ot';
        | None -> Some ch.ch_obj.co_type;
    end;
    | OT_queue qu -> 
    begin
        match qu.qu_ot with
        | Some ot' -> dt_of_ot ot';
        | None -> Some qu.qu_obj.co_type;
    end;
    | OT_component _  -> None;
    | OT_struct st   -> Some (DT_object st.st_type.ts_name);
    | OT_array  at  -> dt_of_ot at.at_objs.(0);
    | OT_array_sel  (at,sel)  -> dt_of_ot at.at_objs.(sel);
    | OT_object ao   -> Some (DT_object ao.ao_name);
    | OT_named_value (_,V_int i64)    -> Some (DT_int (const_width (V_int i64)));
    | OT_value (V_int i64)    -> Some (DT_int (const_width (V_int i64)));
    | OT_named_value _
    | OT_value _    -> Some (DT_object "%V");
    | OT_reference _ -> None


let rec co_of_ot ot =
    match ot with
    | OT_const  co  -> Some co;
    | OT_named_value _ -> None;
    | OT_signal co  -> Some co;
    | OT_reg co     -> Some co;
    | OT_var co  -> Some co;
    | OT_channel ch -> Some ch.ch_obj;
    | OT_queue qu -> Some qu.qu_obj;
    | OT_component _  -> None;
    | OT_struct _   -> None; 
    | OT_array  at  -> co_of_ot at.at_objs.(0);
    | OT_array_sel  (at,sel)  -> co_of_ot at.at_objs.(sel);
    | OT_object _   -> None;
    | OT_value _    -> None;
    | OT_reference _ -> None


let size_of_dt dt =
    match dt with
    | DT_logic n -> n;
    | DT_int n -> n;
    | DT_string _ -> 8;
    | DT_char -> 8;
    | DT_bool -> 1;
    | _ -> (-1)

let sprint_dt dt =
    match dt with
    | DT_logic n -> sprintf "DT_logic %d" n;
    | DT_int n -> sprintf "DT_int %d" n;
    | DT_string n -> sprintf "DT_string %d" n;
    | DT_char -> "DT_char";
    | DT_bool -> "DT_bool";
    | DT_object str -> sprintf "DT_object %s" str;
    | DT_lneg -> "DT_lneg";
    | DT_aneg -> "DT_aneg";
    | DT_natural n -> sprintf "DT_natural %d" n

let sprint_td td =
    match td with
    | T_logic -> "T_logic";
    | T_int -> "T_int";
    | T_str -> "T_string";
    | T_char -> "T_char";
    | T_bool -> "T_bool";
    | T_object -> "T_object";
    | T_natural -> "T_natural"

let print_dt dt = out (sprint_dt dt)

let sprint_ot ot =
  sprintf "%s(%s)" (name_of_ot ot) (
    match dt_of_ot ot with
    | Some dt -> sprint_dt dt;
    | None -> "%"
  )

(*
** Print object_params list
*)
let sprint_opl opl =
  let str = ref "" in
  List.iter (fun op ->
        match op with
        | OD_sub (a,b) -> str := !str ^ (sprintf "<SUB(%d,%d)>" a b);
        | OD_index _   -> str := !str ^ (sprintf "<INDEX>");
        | OD_conv dt   -> str := !str ^ (sprintf "<CONV(%s)>"
                                                 (sprint_dt dt));
        | OD_sel il    -> str := !str ^ (sprintf "<SEL[%s]>"
                                (let s = ref "" in
                                 Array.iter (fun i -> 
                                    s := !s ^ (sprintf "<%d>" i)
                                    ) il;
                                 !s));
        | OD_sel_obj _ -> str := !str ^ (sprintf "<SELOBJ>");
        | OD_lneg      -> str := !str ^ (sprintf "<LNEG>");
        | OD_aneg      -> str := !str ^ (sprintf "<LNEG>");
        | OD_bneg      -> str := !str ^ (sprintf "<BNEG>");
    ) opl;
  !str

(*
** PI object parameter utils
*)

let is_value ot =
    match ot with
    | OT_named_value _
    | OT_value _ -> true;
    | _ -> false
let get_value ot =
    match ot with
    | OT_named_value (_,v)
    | OT_value v -> v;
    | OT_const co -> co.co_init;
    | _ -> error 291877 ""
let is_const ot =
    match ot with
    | OT_const _ -> true;
    | _ -> false
                                
let is_sub opl =
    let rec search opl =
        match opl with
        | pa::tl -> 
        begin
            match pa with
            | OD_sub _ -> true;
            | _ -> search tl;
        end;
        | [] -> false in
    search opl

let is_index opl =
    let rec search opl =
        match opl with
        | pa::tl -> 
        begin
            match pa with
            | OD_index (PI_obj (_,ot)) -> is_value ot;
            | _ -> search tl;
        end;
        | [] -> false in
    search opl

let is_index_obj opl =
    let rec search opl =
        match opl with
        | pa::tl -> 
        begin
            match pa with
            | OD_index (PI_obj (_,ot)) -> not (is_value ot);
            | OD_index _ -> true;
            | _ -> search tl;
        end;
        | [] -> false in
    search opl

let is_conv opl =
    let rec search opl =
        match opl with
        | pa::tl -> 
        begin
            match pa with
            | OD_conv dt -> 
            begin
                match dt with
                | DT_lneg
                | DT_aneg -> search tl;
                | _ -> true;
            end;
            | _ -> search tl;
        end;
        | [] -> false in
    search opl

let is_neg opl =
    let rec search opl =
        match opl with
        | pa::tl -> 
        begin
            match pa with
            | OD_conv dt -> 
            begin
                match dt with
                | DT_lneg
                | DT_aneg -> true;
                | _ -> search tl;
            end;
            | _ -> search tl;
        end;
        | [] -> false in
    search opl


let is_sel opl =
    let rec search opl =
        match opl with
        | pa::tl -> 
        begin
            match pa with
            | OD_sel _ -> true;
            | _ -> search tl;
        end;
        | [] -> false in
    search opl

let is_sel_obj opl =
    let rec search opl =
        match opl with
        | pa::tl -> 
        begin
            match pa with
            | OD_sel_obj _ -> true;
            | _ -> search tl;
        end;
        | [] -> false in
    search opl

let is_array ot =
    match ot with
    | OT_array _ -> true;
    | _ -> false

let obj_sub opl =
    let rec search opl =
        match opl with
        | pa::tl -> 
        begin
            match pa with
            | OD_sub sub -> sub;
            | _ -> search tl;
        end;
        | [] -> error 269519 "" in
    search opl

let obj_index opl =
    let rec search opl =
        match opl with
        | pa::tl -> 
        begin
            match pa with
            | OD_index (PI_obj(_,ot)) -> 
            begin
              match get_value ot with
              | V_int i64 -> Int64.to_int i64;
              | _ -> error 455870 "";
            end;
            | _ -> search tl;
        end;
        | [] -> error 219519 "" in
    search opl

let obj_index_obj opl =
    let rec search opl =
        match opl with
        | pa::tl -> 
        begin
            match pa with
            | OD_index obj -> obj;
            | _ -> search tl;
        end;
        | [] -> error 229519 "" in
    search opl

let obj_conv opl =
    let sub = ref None in
    let rec search opl =
        match opl with
        | pa::tl -> 
        begin
            match pa with
            | OD_conv dt -> 
            begin
                match dt with
                | DT_lneg
                | DT_aneg -> search tl;
                | _ -> dt;
            end;
            | OD_sub r -> 
                sub := Some r;
                search tl;
            | _ -> search tl;
        end;
        | [] -> error 269520 "" in
    let dt = search opl in
    if !sub <> None then
    begin
      (*
      ** Bit subrange with higher priority than conversion...
      *)
      let (a,b) = obj_sub opl in
      dt_of_td (td_of_dt dt) (b-a+1);
    end 
    else if is_index opl then
    begin
      (*
      ** Bit selector
      *)
      dt_of_td (td_of_dt dt) 1;
    end 
    else if is_index_obj opl then
    begin
      (*
      ** Bit selector
      *)
      dt_of_td (td_of_dt dt) 1;
    end 
    else         
        dt
    
let obj_neg opl =
    let rec search opl =
        match opl with
        | pa::tl -> 
        begin
            match pa with
            | OD_conv dt -> 
            begin
                match dt with
                | DT_lneg
                | DT_aneg -> dt;
                | _ -> search tl;
            end;
            | _ -> search tl;
        end;
        | [] -> error 269521 "" in
    search opl

let obj_sel opl =
    let rec search opl =
        match opl with
        | pa::tl -> 
        begin
            match pa with
            | OD_sel sub -> sub;
            | _ -> search tl;
        end;
        | [] -> error 269521 "" in
    search opl
let obj_sel_obj opl =
    let rec search opl =
        match opl with
        | pa::tl -> 
        begin
            match pa with
            | OD_sel_obj sub -> sub;
            | _ -> search tl;
        end;
        | [] -> error 269522 "" in
    search opl

(*
** Return DT of PI obj
*)
let obj_dt obj =
    let get_some_dt dts =
        match dts with
        | Some dt -> dt;
        | None -> error 826196 "";
        in

    match obj with
    | PI_obj (opl,ot) ->
        debug "obj_dt" with (sprintf "obj_dt: <%s>" (name_of_ot ot));
        if is_conv opl then
        begin
            let dt = 
                match dt_of_ot ot with
                | Some dt -> dt;
                | None -> error 687022 "" in
            let n = size_of_dt dt in
            let dt' = obj_conv opl in
            let n' = size_of_dt dt' in
            if n' > 0 then dt' else
                dt_of_td (td_of_dt dt') n;
        end
        else if (is_sub opl) && not (is_array ot) then
        begin
            let (a,b) = obj_sub opl in
            dt_of_td (td_of_dt (get_some_dt (dt_of_ot ot))) (b-a+1);
        end 
        else if (is_index opl) && not (is_array ot) then
        begin
            dt_of_td (td_of_dt (get_some_dt (dt_of_ot ot))) 1;
        end 
        else if (is_index_obj opl) && not (is_array ot) then
        begin
            dt_of_td (td_of_dt (get_some_dt (dt_of_ot ot))) 1;
        end 
        else         
            get_some_dt (dt_of_ot ot);
    | _ -> error 545768 ""

let rec ot_of_pi pi =
  match pi with
  | PI_obj (op,ot) -> ot;    
  | _ -> error 366821 ""

let rec rot_of_pi pi =
  match pi with
  | PI_obj (opl,ot) -> 
    if is_sub opl then Some (obj_sub opl),ot
    else if is_index opl then
      Some (0,0),ot
    else if is_index_obj opl then
      Some (0,0),ot
    else
      None,ot;
  | _ -> error 366821 ""

let dt_of_pi pi =
  let range,ot = rot_of_pi pi in
  match dt_of_ot ot with
  | Some dt -> 
  begin
    match range with
    | Some (a,b) when (not (is_array ot))->
      dt_of_td (td_of_dt dt) (b-a+1);
    | _ -> dt;
  end;
  | None -> error 974913 ""

let conv_dt_of_pi pi =
  match pi with
  | PI_obj (opl,ot) -> 
    let rec search opl =
        match opl with
        | pa::tl -> 
        begin
            match pa with
            | OD_conv dt -> 
            begin
                match dt with
                | DT_lneg
                | DT_aneg -> search tl;
                | _ -> Some dt;
            end;
            | OD_sub r -> search tl;
            | _ -> search tl;
        end;
        | [] -> None in
    search opl;
  | _ -> None

let rec name_of_pi pi = 
  match pi with
  | PI_obj (op,ot) -> name_of_ot ot;
  | PI_concat (op1,op2) ->
    sprintf "%s_%s" (name_of_pi op1) (name_of_pi op2)
  | _ -> error 366822 ""

let rec type_of_ud ud =
  match ud with
  | UC_reg uo -> uo.uo_type;
  | UC_var uo -> uo.uo_type;
  | UC_sig uo -> uo.uo_type;
  | UC_chan uo -> uo.uo_type;
  | UC_queue uo -> uo.uo_type;
  | UC_val uv -> uv.uv_type;
  | UC_list ul -> ul.ul_type;
  | UC_sel us -> type_of_ud us.us_obj;
  | UC_alu ua -> uo_type ua.ua_type ua.ua_type;
  | UC_immed n -> uo_type (DT_object (sprintf "IMMED%d" n)) (DT_object (sprintf "IMMED%d" n))
  | UC_temp ut -> ut.ut_type;
  | UC_bool n -> uo_type DT_bool DT_bool;
  | UC_array uca -> uca.uat_type
  
let rec to_of_ud ud =
  match ud with
  | UC_reg _ -> O_reg
  | UC_var _ -> O_var
  | UC_sig _ ->  O_signal
  | UC_chan _ -> O_channel
  | UC_queue _ -> O_queue
  | UC_val _ -> O_value
  | UC_list _ -> O_object
  | UC_sel _ -> O_object
  | UC_alu _ -> O_object
  | UC_immed _ -> O_object
  | UC_temp _ -> O_reg
  | UC_bool _ -> O_object
  | UC_array _ -> O_array

let name_of_ud ud =
  match ud with
  | UC_reg uo -> uo.uo_name
  | UC_var uo -> uo.uo_name
  | UC_sig uo ->  uo.uo_name
  | UC_chan uo -> uo.uo_name
  | UC_queue uo -> uo.uo_name
  | UC_val uv -> "%V"
  | UC_list ul -> "%L"
  | UC_sel us -> "%S"
  | UC_alu _ -> "%A"
  | UC_immed _ -> "%I"
  | UC_temp ut -> ut.ut_name
  | UC_bool _ -> "%B"
  | UC_array uat -> uat.uat_name
 
(*
** Return a list of all found objects
*)
let rec pi_get_objs pi =
  let rec l il =
    match il with
    | hd::tl -> (pi_get_objs hd)@(l tl);
    | [] -> [] in 
  match pi with
  | PI_obj _ -> [pi];
  | PI_assign (_,lhs,rhs) ->
    [lhs] @ (pi_get_objs rhs);
  | PI_block (il,_) 
  | PI_list il -> 
     l il
  | PI_arithm (_,op1,op2) 
  | PI_bool (_,_,op1,op2) -> 
    (pi_get_objs op1) @ (pi_get_objs  op2)
  | PI_concat (i1,i2) ->
    (pi_get_objs i1) @ (pi_get_objs i2)
  | PI_waitfor (_,expr,_,_,b1,b2) ->
    (pi_get_objs expr) @ (pi_get_objs b1) @ (pi_get_objs b2)
  | PI_branch (_,expr,b1,b2) ->
    (pi_get_objs expr) @ (pi_get_objs b1) @ (pi_get_objs b2)
  | PI_forloop (_,expr,_,_,_,b) ->
    (pi_get_objs expr) @ (pi_get_objs b)
  | PI_loop (_,_,expr,b) ->
    (pi_get_objs expr) @ (pi_get_objs b)
  | PI_select (_,expr,cl) ->
    (pi_get_objs expr) @ (pi_get_objs cl)
  | PI_case (_,il,b) ->
    (l il) @ (pi_get_objs b)
  | PI_map (_,i1,i2) ->
    (pi_get_objs i1) @ (pi_get_objs i2)
  | PI_fun (_,obj,_,args) ->
    [PI_obj obj] @ (l args)
  | PI_monitor (_,_,_) -> [];
  | PI_raise _ -> [];
  | PI_try (block,cb) -> (pi_get_objs block)@(pi_get_objs cb);
  | PI_nop -> []
  
let sprint_vhdl_data vhd =
  (
    sprintf "{dp_sig=%s;\ndp_def=%s;\ndp_sen=%s;\ndp_aux=%s;\ndp_aux_def=%s\n"
            vhd.dp_sig
            (
              let first = ref true in
              let str = ref "" in
              List.iter (fun (l1,l2) ->
                if not !first then str := sprintf "%s\n    (\"%s\",\"%s\")" !str l1 l2
                else str := sprintf "(\"%s\",\"%s\")" l1 l2;
                first := false;
                ) vhd.dp_def;
              !str
            )
            (
              let first = ref true in
              let str = ref "" in
              List.iter (fun (l) ->
                if not !first then str := sprintf "%s\n    \"%s\"" !str l
                else str := sprintf "\"%s\"" l;
                first := false;
                ) vhd.dp_sen;
              !str
            )
            (
              let first = ref true in
              let str = ref "" in
              List.iter (fun (l) ->
                if not !first then str := sprintf "%s\n    \"%s\"" !str l
                else str := sprintf "\"%s\"" l;
                first := false;
                ) vhd.dp_aux;
              !str
            )  
            (
              let first = ref true in
              let str = ref "" in
              List.iter (fun (l1,l2) ->
                if not !first then str := sprintf "%s\n    (\"%s\",\"%s\")" !str l1 l2
                else str := sprintf "(\"%s\",\"%s\")" l1 l2;
                first := false;
                ) vhd.dp_aux_def;
              !str
            )
  )^
  (
    sprintf "dp_aux_sen=%s;\ncp_sig=%s;\ncp_sen=%s;\ndp_conv=%s;\ntop_def=%s\ntop_expr=%s}\n"
            (
              let first = ref true in
              let str = ref "" in
              List.iter (fun (l) ->
                if not !first then str := sprintf "%s\n    \"%s\"" !str l
                else str := sprintf "\"%s\"" l;
                first := false;
                ) vhd.dp_aux_sen;
              !str
            )
            vhd.cp_sig
            (
              let first = ref true in
              let str = ref "" in
              List.iter (fun (l) ->
                if not !first then str := sprintf "%s\n    \"%s\"" !str l
                else str := sprintf "\"%s\"" l;
                first := false;
                ) vhd.cp_sen;
              !str
            )
            vhd.dp_conv
            (
              let first = ref true in
              let str = ref "" in
              List.iter (fun (l) ->
                if not !first then str := sprintf "%s\n    \"%s\"" !str l
                else str := sprintf "\"%s\"" l;
                first := false;
                ) vhd.top_def;
              !str
            )
            (
              let first = ref true in
              let str = ref "" in
              List.iter (fun (l) ->
                if not !first then str := sprintf "%s\n    \"%s\"" !str l
                else str := sprintf "\"%s\"" l;
                first := false;
                ) vhd.top_expr;
              !str
            )  
  )
  
let print_vhdl_data vhd =
  out (sprint_vhdl_data vhd)
  
