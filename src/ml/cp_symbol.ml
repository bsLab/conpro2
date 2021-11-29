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
**    $CREATED:     8.3.2006
**    $VERSION:     2.07
**
**    $INFO:
**
**  Symbol table module. Symbol table hierarchy:
**
**  1. modu.mod_procs.pro_objs
**  2. modu.mod_objs
**  3. for all modu.mod_deps.mod_export
**  
**
**    $ENDOFINFO
**
*)

open Cp_common
open Cp_syntax
open Cp_types
open Hashtbl
open Printf


(*
** Main toplevel symbol table (all modules).
*)
let (top_syms:(string,sym_type) t) = create 100

(*
** All known symbols are stored in hash tables:
**
**  Objects, Processes, Components, Modules
*)



open Printf
let rec print_value va =
  let ast_sprint_timeunit vunit =
      match vunit with
      | Nsec -> "Nsec";
      | Usec -> "Usec";
      | Msec -> "Msec";
      | Sec -> "Sec";
      | Cycles -> "Cycles" in

  let ast_sprint_frequnit vunit =
      match vunit with
      | Ghz -> "Ghz";
      | Mhz -> "Mhz";
      | Khz -> "Khz";
      | Hz -> "Hz" in
  match va with
  | V_int i64 -> sprintf "%s" (Int64.to_string i64)
  | V_float flt -> sprintf "%f" flt
  | V_string str -> sprintf "\"%s\"" str
  | V_char ch -> sprintf "'%c'" ch
  | V_logic lstr -> sprintf "%s" lstr
  | V_bool bo -> sprintf "%b" bo
  | V_list vl -> sprintf "[%s]" (Cp_print.hlist print_value vl);
  | V_z -> "V_z"
  | V_time (i64,timeunit) -> sprintf "%s %s" (Int64.to_string i64) (ast_sprint_timeunit timeunit)  
  | V_freq (i64,frequnit) -> sprintf "%s %s" (Int64.to_string i64) (ast_sprint_frequnit frequnit)
  | V_null -> "NULL"

(*
** Return (first) matching symbol.
*)
let sym_lookup symtab name = 
    if mem symtab name then  
        find symtab name 
    else
    begin
      out_ (sprintf "Symbol <%s> not found." name);
      raise Not_found
    end

(*
** Get only object symbol
*)
let sym_get_obj symtab name =
    if mem symtab name then  
    begin
        let sym = find symtab name in
        match sym with
        | Sym_obj obj -> obj;
        | _ -> raise Not_found;
    end
    else
    begin
      out_ (sprintf "Symbol <%s> not found." name);
      raise Not_found
    end

(*
** Get only module symbol
*)
let sym_get_mod symtab name =
    if mem symtab name then  
    begin
        let sym = find symtab name in
        match sym with
        | Sym_mod md -> md;
        | _ -> raise Not_found;
    end
    else
        raise Not_found
(*
** Get only rule symbol
*)
let sym_get_rule symtab name =
    if mem symtab name then  
    begin
        let sym = find symtab name in
        match sym with
        | Sym_rule md -> md;
        | _ -> raise Not_found;
    end
    else
        raise Not_found

(*
** Get only type symbol
*)
let sym_get_type symtab name =
    if mem symtab name then  
    begin
        let sym = find symtab name in
        match sym with
        | Sym_type tp -> tp;
        | _ -> raise Not_found;
    end
    else
        raise Not_found


(*
** Get only process symbol
*)
let sym_get_pro symtab name =
    let name = "PRO_"^name in
    if mem symtab name then  
        let sym = find symtab name in
        match sym with
        | Sym_pro pro -> pro;
        | _ -> raise Not_found;
    else
        raise Not_found

(*
** Get only function symbol
*)
let sym_get_fun symtab name =
    if mem symtab name then  
        let sym = find symtab name in
        match sym with
        | Sym_fun f -> f;
        | _ -> raise Not_found;
    else
        raise Not_found

(*
** Search for a symbol
*)
let sym_check symtab name = 
    mem symtab name

let sym_check_obj symtab name =
    if mem symtab name then  
        let sym = find symtab name in
        match sym with
        | Sym_obj _ -> true;
        | _ -> false;
    else
        false
let sym_check_mod symtab name =
    if mem symtab name then  
        let sym = find symtab name in
        match sym with
        | Sym_mod _ -> true;
        | _ -> false;
    else
        false
let sym_check_rule symtab name =
    if mem symtab name then  
        let sym = find symtab name in
        match sym with
        | Sym_rule _ -> true;
        | _ -> false;
    else
        false
let sym_check_pro symtab name =
    if mem symtab ("PRO_"^name) then  
        let sym = find symtab ("PRO_"^name) in
        match sym with
        | Sym_pro _ -> true;
        | _ -> false;
    else
        false
let sym_check_type symtab name =
    if mem symtab name then  
        let sym = find symtab name in
        match sym with
        | Sym_type  _ -> true;
        | _ -> false;
    else
        false
let sym_check_fun symtab name =
    if mem symtab name then  
        let sym = find symtab name in
        match sym with
        | Sym_fun  _ -> true;
        | _ -> false;
    else
        false
let sym_check_block symtab name =
    if mem symtab name then  
        let sym = find symtab name in
        match sym with
        | Sym_block  _ -> true;
        | _ -> false;
    else
        false

(*
** Search and get symbols from external module symbol list tree
*)
let sym_check_ext modu name =
    let rec iter ml =
        match ml with
        | md::tl ->
            let found = sym_check md.mod_export name in
            if not found then iter tl else true
        | [] -> false
        in
    iter modu.mod_external
let sym_lookup_ext modu name =
    let rec iter ml =
        match ml with
        | md::tl ->
            let found = sym_check md.mod_export name in
            if not found then iter tl else 
                sym_lookup md.mod_export name
        | [] -> raise Not_found
        in
    iter modu.mod_external

let sym_check_obj_ext modu name =
    let rec iter ml =
        match ml with
        | md::tl ->
            let found = sym_check_obj md.mod_export name in
            if not found then iter tl else true
        | [] -> false
        in
    iter modu.mod_external
let sym_get_obj_ext modu name =
    let rec iter ml =
        match ml with
        | md::tl ->
            let found = sym_check_obj md.mod_export name in
            if not found then iter tl else 
                sym_get_obj md.mod_export name
        | [] -> raise Not_found
        in
    iter modu.mod_external

let sym_check_mod_ext modu name =
    let rec iter ml =
        match ml with
        | md::tl ->
            let found = sym_check_mod md.mod_export name in
            if not found then iter tl else true
        | [] -> false
        in
    iter modu.mod_external
let sym_get_mod_ext modu name =
    let rec iter ml =
        match ml with
        | md::tl ->
            let found = sym_check_mod md.mod_export name in
            if not found then iter tl else 
                sym_get_mod md.mod_export name
        | [] -> raise Not_found
        in
    iter modu.mod_external

let sym_check_pro_ext modu name =
    let rec iter ml =
        match ml with
        | md::tl ->
            let found = sym_check_pro md.mod_export name in
            if not found then iter tl else true
        | [] -> false
        in
    iter modu.mod_external
let sym_get_pro_ext modu name =
    let rec iter ml =
        match ml with
        | md::tl ->
            let found = sym_check_pro md.mod_export name in
            if not found then iter tl else 
                sym_get_pro md.mod_export name
        | [] -> raise Not_found
        in
    iter modu.mod_external

let sym_check_type_ext modu name =
    let rec iter ml =
        match ml with
        | md::tl ->
            let found = sym_check_type md.mod_export name in
            if not found then iter tl else true
        | [] -> false
        in
    iter modu.mod_external
let sym_get_type_ext modu name =
    let rec iter ml =
        match ml with
        | md::tl ->
            let found = sym_check_type md.mod_export name in
            if not found then iter tl else 
                sym_get_type md.mod_export name
        | [] -> raise Not_found
        in
    iter modu.mod_external

let sym_check_fun_ext modu name =
    let rec iter ml =
        match ml with
        | md::tl ->
            let found = sym_check_fun md.mod_export name in
            if not found then iter tl else true
        | [] -> false
        in
    iter modu.mod_external
let sym_get_fun_ext modu name =
    let rec iter ml =
        match ml with
        | md::tl ->
            let found = sym_check_fun md.mod_export name in
            if not found then iter tl else 
                sym_get_fun md.mod_export name
        | [] -> raise Not_found
        in
    iter modu.mod_external

let rec sym_name sym = 
    match sym with
    | Sym_mod md -> md.mod_name;
    | Sym_rule md -> md.rl_name;
    | Sym_pro pr -> "PRO_"^pr.pro_name;
    | Sym_block bl -> bl.db_name;
    | Sym_type tp ->
        let tp_name =
            match tp with
            | Type_const tc -> tc.tc_name;
            | Type_struct ts -> ts.ts_name;
            | Type_bit tb -> tb.tb_name;
            | Type_abstract ta -> ta.ta_name;
            | Type_exc tx -> tx.tx_name;
            | Type_module tm -> sprintf "%s.%s" tm.tm_module tm.tm_type;
            in
        tp_name
    | Sym_fun f -> f.fun_name;
    | Sym_obj ob ->
      let rec get_obj_name ob =
        match ob with
        | OT_const co -> co.co_name;
        | OT_named_value (name,_) -> name;
        | OT_signal co -> co.co_name;
        | OT_reg co -> co.co_name;
        | OT_var co -> co.co_name;
        | OT_channel ch -> ch.ch_obj.co_name;
        | OT_queue qu -> qu.qu_obj.co_name;
        | OT_array at -> at.at_name;
        | OT_array_sel (at,sel) -> 
            sprintf "%s_%d" at.at_name sel;
        | OT_object ao -> ao.ao_name;
        | OT_struct st -> st.st_name;
        | OT_component st -> st.st_name;
        | _ -> error 615795 "";
        in
      let name = get_obj_name ob in
      name;
    | Sym_mon (dbg,mon) -> sprintf "MON_%s" (sym_name mon)

(*
** Add symbol to table
*)
let sym_add symtab sym = 
  let to_mod name = 
        let name' = String.copy name in
        name'.[0] <- char_of_int ((int_of_char name'.[0])-32);
        name' in
  let of_mod name =    
        let name' = String.copy name in
        name'.[0] <- char_of_int ((int_of_char name'.[0])+32);
        name' in

  let check name = mem symtab name in
 

  let name = sym_name sym in
  debug "sym_add" with (sprintf "adding symbol <%s>..." name);
  let found = check name in
  if found then
  begin
    (*
    ** named values can be overriden by previous
    ** definition
    *)
    match find symtab name with
    | Sym_obj (OT_named_value (name,valu)) -> 
        warning (sprintf "Parameter value <%s> already defined, using value %s" 
                         name (print_value valu))
    | _ -> error 0 (sprintf "sym_add: Symbol <%s> exists already!" name); 
  end
  else add symtab name sym



(*
** Check for symbol
*)
let sym_check_sym symtab sym = 
  let to_mod name = 
        let name' = String.copy name in
        name'.[0] <- char_of_int ((int_of_char name'.[0])-32);
        name' in
  let of_mod name =    
        let name' = String.copy name in
        name'.[0] <- char_of_int ((int_of_char name'.[0])+32);
        name' in

  let check name = mem symtab name in
 
  let rec check_sym sym =
  match sym with
  | Sym_mod md -> check md.mod_name;
  | Sym_rule md -> check md.rl_name;
  | Sym_pro pr -> check ("PRO_"^pr.pro_name);
  | Sym_block bl -> check bl.db_name;
  | Sym_fun f -> check f.fun_name;
  | Sym_type tp ->
        let tp_name =
            match tp with
            | Type_const tc -> tc.tc_name;
            | Type_struct ts -> ts.ts_name;
            | Type_bit tb -> tb.tb_name;
            | Type_abstract ta -> ta.ta_name;
            | Type_module tm -> sprintf "%s.%s" tm.tm_module tm.tm_type;
            | Type_exc tx -> tx.tx_name;
            in
        check tp_name
  | Sym_obj ob ->
  begin
    let rec get_name ob =
        match ob with
        | OT_const co -> co.co_name;
        | OT_named_value (name,_) -> name;
        | OT_signal co -> co.co_name;
        | OT_reg co -> co.co_name;
        | OT_var co -> co.co_name;
        | OT_channel ch -> ch.ch_obj.co_name;
        | OT_queue qu -> qu.qu_obj.co_name;
        | OT_array at -> at.at_name;
        | OT_array_sel (at,sel) -> 
            sprintf "%s_%d" at.at_name sel;
        | OT_struct st -> st.st_name;
        | OT_component st -> st.st_name;
        | OT_object ao -> ao.ao_name;
        | OT_value _ -> "%V";
        | _ -> error 338273 "";
        in
    let name = get_name ob in
    check name;
  end;
  | Sym_mon (dbg,mon) -> check_sym mon;
  in
  check_sym sym



(*
** Delete symbol from symbol table
*)
let sym_delete symtab sym =
  let rec name sym =
    match sym with
    | Sym_mod md -> md.mod_name 
    | Sym_rule md -> md.rl_name 
    | Sym_pro pr -> ("PRO_"^pr.pro_name)
    | Sym_block bl -> bl.db_name 
    | Sym_fun f -> f.fun_name
    | Sym_type tp ->
      let tp_name =
              match tp with
              | Type_const tc -> tc.tc_name;
              | Type_struct ts -> ts.ts_name;
              | Type_bit tb -> tb.tb_name;
              | Type_abstract ta -> ta.ta_name;
              | Type_module tm -> sprintf "%s.%s" tm.tm_module tm.tm_type;
              | Type_exc tx -> tx.tx_name;
              in
      tp_name
    | Sym_obj ob ->
    begin
      let rec get_name ob =
          match ob with
          | OT_const co -> co.co_name;
          | OT_named_value (name,_) -> name;
          | OT_signal co -> co.co_name;
          | OT_reg co -> co.co_name;
          | OT_var co -> co.co_name;
          | OT_channel ch -> ch.ch_obj.co_name;
          | OT_queue qu -> qu.qu_obj.co_name;
          | OT_array at -> at.at_name;
          | OT_array_sel (at,sel) -> 
              sprintf "%s_%d" at.at_name sel;
          | OT_struct st -> st.st_name;
          | OT_component st -> st.st_name;
          | OT_object ao -> ao.ao_name;
          | _ -> error 804620 "";
          in
      let name = get_name ob in
      name 
    end;
    | Sym_mon (dbg,mon) -> name mon
    in
  let name = name sym in
  let found = sym_check symtab name in
  if not found then warning (sprintf "Can't delete symbol <%s>: not found." name) else
  remove symtab name


(*
** Move symbol from one symbol table to another
*)

let sym_move src dst name =
    let sym = sym_lookup src name in
    sym_delete src sym;
    sym_add dst sym


(*
** Create a (process local) copy of a symbol
*)
let sym_copy pro sym =
  let co_copy co =
    {co with 
      co_guard=None;
      co_reader=List.filter (fun pro' -> pro=pro') co.co_reader;
      co_writer=List.filter (fun pro' -> pro=pro') co.co_writer;
      co_process=Some pro;
    } in
    
  match sym with
  | Sym_obj ot ->
  begin
    match ot with
    | OT_const co -> Sym_obj (OT_const (co_copy co));
    | OT_signal co -> Sym_obj (OT_signal (co_copy co));
    | OT_reg co -> Sym_obj (OT_reg (co_copy co));
    | OT_var co -> Sym_obj (OT_var (co_copy co));
    | _ -> sym;
  end;
  | Sym_block db -> error 392578 "TODO";
  | _ -> sym
  
(*
** Return list of all symbols contained in symbol table
*)
let list_of_sym symtab =
    let l = ref [] in
    Hashtbl.iter (fun name sym ->
                l := !l @ [sym];
            ) symtab;
    !l


let length symtab =
    let num = ref 0 in
    Hashtbl.iter (fun name sym ->
                incr num;
            ) symtab;
    !num
