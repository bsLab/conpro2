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
**    $CREATED:     16.4.2010
**    $VERSION:     2.11
**
**    $INFO:
**
**  C backend: high-level synthesis of ConPro module, process, and object model to 
**              C imeprative language model
**
**    Abstraction level: high (default)
**
**    $ENDOFINFO
**
*)
open Cp_types
open Cp_syntax
open Cp_common
open Cp_utils
open Cp_ml_types
open Cp_symbol
open Printf
open Cp_print
open Cp_expr
open Cp_data
open Cp_c_types

let c_et = ref []
let c_exc_map = ref false
let c_defs = ref [] 
let c_evals = ref [] 
let c_exc_env = ref 0 
let c_mod_dep = ref []
let c_expr_close = ref ""

let err str =
  error 0 (sprintf "C: %s" str)
let rec env_search envl name = 
  match envl with
  | env :: tl -> if Hashtbl.mem env name then true else env_search tl name
  | [] -> false
  
let rec env_get envl name = 
  match envl with
  | env :: tl -> if Hashtbl.mem env name then Hashtbl.find env name else env_get tl name
  | [] -> err (sprintf "env_get: %s not found" name)
  
let env_add env name sym = 
  debug "c:env_add" with (sprintf "Adding <%s>." name);
  if not (Hashtbl.mem env name) then Hashtbl.add env name sym
let sprint_env env =
  match env with
  | C_struct c -> "C_struct()"
  | C_array c -> "C_array()"
  | C_enum c -> "C_enum()"
  | C_type c -> "C_type()"
  | C_abstract_type c -> "C_abstract_type()"
  | C_exception c -> "C_exception()"
  | C_storage c -> "C_storage()"
  | C_abstract c -> "C_abstract()"
  | C_fun c -> "C_fun()"
  | C_unknown -> "C_unknown"

let print_env envl =
  out_ "Environment:";
  List.iter (Hashtbl.iter (fun name env ->
    out_ (sprintf "%s:%s" name (sprint_env env))
    )) envl
(*
** Check for some immutable storage, for example loop counters.
** Returns: mutable?,fun,reference, reduced original name (substitution)
*)
let c_env_transform name =
  let check_loop = Str.string_match (Str.regexp "LOOP_") name 0 in
  let check_fun_arg = Str.string_match (Str.regexp "ARG_FUN_") name 0 in
  let check_fun_ret_arg = Str.string_match (Str.regexp "RET_FUN_") name 0 in
  let check_process_id = name = "#" in
  if check_loop then
  begin
    false,false,false,Str.global_replace (Str.regexp "\(LOOP_\)\(.\)\(_.+\)") "\2" name
  end
  else if check_fun_arg then
  begin
    true,true,false,Str.global_replace (Str.regexp "ARG_FUN_\(.+\)") "\1" name
  end
  else if check_fun_ret_arg then
  begin
    true,true,true,Str.global_replace (Str.regexp "RET_FUN_\(.+\)") "\1" name
  end
  else if check_process_id then
    false,false,false,"process_id"
  else
    true,false,false,""
  
(*
** Evaluate a boolean expression. Parameter list contains
** parameter values replaced by parameters inside expression.
*)

let eval expr paraml =
  try
  begin
    let s2i = int_of_string in
    let s op =
      let rec find paraml =
        match paraml with
        | (n,v) :: tl -> if n = op then v else find tl 
        | [] -> op in
      find paraml in
    let tokens = List.filter (fun token -> token <> "") 
                  (Str.split (Str.regexp " ") expr) in
    let rec eval tokenl =
      match tokenl with
      | [op1;"<";op2] -> (s2i (s op1)) < (s2i (s op2))
      | [op1;">";op2] -> (s2i (s op1)) > (s2i (s op2))
      | [op1;"<>";op2] -> (s2i (s op1)) <> (s2i (s op2))
      | [op1;"<=";op2] -> (s2i (s op1)) <= (s2i (s op2))
      | [op1;">=";op2] -> (s2i (s op1)) >= (s2i (s op2))
      | [op1;"=";op2] -> (s op1) = (s op2)
      | _ -> false in
    eval tokens;
  end
  with _ -> false

let fix_name name = 
  match name with
  | "#" -> "process_id"
  | "main" -> "Main"
  | _ -> name

let oc = ref Pervasives.stdout   
let out_indent = ref 0
let out str = output_string !oc ((ibox !out_indent str)^"\n")
let ind_incr () = out_indent := !out_indent + 2  
let ind_decr () = out_indent := !out_indent - 2  

(*
** Create object descriptor: Extract structure and static array selectors from
** object name and return environment object:
**
**  obj ... -> num_sel=[],sel=[]
**  obj_0 ... -> num_sel=[0]
**  obj_0_1 ... -> num_sel=[0;1]
**  obj_sel ... -> sel=[sel]
**  obj_sel_0 ... -> num_sel=[0],sel=[sel]
**
** Returns object descriptor or raises error <Not_found>
*)
let c_obj_desc envl name ar str =
  let sym_name = name in
  let tokens = Str.split (Str.regexp "_") sym_name in
  let num_sel = ref [] in
  let sel = ref "" in
  let is_num s = protects(__(int_of_string s)) in
  let get_num s = int_of_string s in
  let scan = ref true in
  let remains = ref tokens in
  let ar = List.filter (fun at ->
    not (List.mem AT_dyn at.at_flags)
    ) ar in

    
  (*
  ** Extract array selector
  *)
  if ar <> [] then 
  begin
    remains := [];
    List.iter (fun token ->
    if (is_num token) && !scan then
      num_sel := (get_num token) :: !num_sel
    else
    begin
      remains := token :: !remains;
      scan := false;
    end;
    ) (List.rev tokens);
  end;
  (*
  ** Incremental object lookup - find longest token list match!
  **  a_b_c matches both a_b and a_b_c!
  *)
  let rec find tokenl name last =
    match tokenl with
    | token :: tl ->
      let name = sprintf "%s%s%s" name (if name <> "" then "_" else "") token in
      if env_search envl name then find tl name (Some (name,tl))
      else if last <> None then get_some last else find tl name None
    | [] -> if last <> None then get_some last else "",[] in
  let env_obj,remains' = find !remains "" None in
  (*
  ** Extract structure selector
  *)
  List.iter (fun token ->
    if !sel = "" then sel := token
    else sel := sprintf "%s_%s" !sel token
    ) remains';
    
  if not (env_search envl env_obj) then
    err (sprintf "Environment object <%s> not found." sym_name)
  else match env_get envl env_obj with
  | C_storage sto -> 
  begin
    match sto.sto_type with
    | C_type _ | C_enum _ ->
      {desc_name=sto.sto_name;desc_obj=C_storage sto;
       desc_num_sel=[];desc_sel=[];
       desc_array=false;desc_struct=false;
       desc_mutable=sto.sto_mutable;desc_export=sto.sto_export;desc_subst=sto.sto_subst;desc_ref=sto.sto_ref;}
    | C_array ar ->
      {desc_name=ar.array_name;desc_obj=C_storage sto;
       desc_num_sel= !num_sel;desc_sel=[!sel];
       desc_array=true;desc_struct=(match ar.array_type with C_struct _ -> true|_ ->false);
       desc_mutable=sto.sto_mutable;desc_subst=sto.sto_subst;desc_export=sto.sto_export;desc_ref=sto.sto_ref;}
    | C_struct st ->
      {desc_name=sto.sto_name;desc_obj=C_storage sto;
       desc_num_sel=[];desc_sel=[!sel];
       desc_array=false;desc_struct=true;
       desc_mutable=sto.sto_mutable;desc_subst=sto.sto_subst;desc_export=sto.sto_export;desc_ref=sto.sto_ref;}
    | _ -> err (sprintf "Unexpected storage object type for <%s> found." env_obj)
  end;
  | C_abstract ab ->
  begin
    match ab.abstr_type with
    | C_abstract_type _ ->
      {desc_name=ab.abstr_name;desc_obj=C_abstract ab;
       desc_num_sel=[];desc_sel=[];
       desc_array=false;desc_struct=false;
       desc_mutable=false;desc_subst=ab.abstr_subst;desc_export=false;desc_ref=false}
    | C_array ar ->
      {desc_name=ar.array_name;desc_obj=C_abstract ab;
       desc_num_sel= !num_sel;desc_sel=[!sel];
       desc_array=true;desc_struct=(match ar.array_type with C_struct _ -> true|_ ->false);
       desc_mutable=false;desc_subst=ab.abstr_subst;desc_export=false;desc_ref=false}
    | C_struct st ->
      {desc_name=ab.abstr_name;desc_obj=C_abstract ab;
       desc_num_sel=[];desc_sel=[!sel];
       desc_array=false;desc_struct=true;
       desc_mutable=false;desc_subst=ab.abstr_subst;desc_export=false;desc_ref=false}
    | _ -> err (sprintf "Unexpected abstract object type for <%s> found." env_obj)
    
  end;
  | _ -> err (sprintf "Unexpected environment object <%s> found." env_obj)

let c_get_type envl name =
  if not (env_search envl name) then
  begin
    print_env envl;
    err (sprintf "Can't find environment type <%s>." name);
  end
  else match env_get envl name with
  | C_struct _ | C_enum _ -> env_get envl name
  | _ -> err (sprintf "Environment symbol <%s> is not a type." name)
   
let c_get_exc envl id =
  let name = sprintf "EXC%d" id in
  if not (env_search envl name) then
    err (sprintf "Can't find environment exception <EXC%d>." id)
  else match env_get envl name with
  | C_exception exc -> exc
  | _ -> err (sprintf "Environment symbol <%s> is not an exception type." name)



(*
** Transform symbol table <objs> into
** ML environment
*)
let c_env glob_env modu objs =
  let syms = list_of_sym objs in
  let types = List.flatten (List.map (fun sym -> match sym with Sym_type tp -> [tp] | _ -> []) syms) in
  let objs = List.flatten (List.map (fun sym -> match sym with Sym_obj ot -> [ot] | _ -> []) syms) in
  let funs = List.flatten (List.map (fun sym -> match sym with Sym_fun f -> [f] | _ -> []) syms) in
  let env = Hashtbl.create 100 in

  (*
  ** Incremental object lookup - find longest token list match!
  **  a_b_c matches both a_b and a_b_c!
  *)
  let rec find tokenl name last =
    match tokenl with
    | token :: tl ->
      let name = sprintf "%s%s%s" name (if name <> "" then "_" else "") token in
      if env_search (env::glob_env) name then find tl name (Some (name,tl))
      else if last <> None then get_some last else find tl name None
    | [] -> if last <> None then get_some last else "",[] in

  let rec name_ il =
    match il with
    | hd :: tl -> (hd)^(if tl <> [] then "_" else "")^(name_ tl)
    | [] -> "" in

  let add_ao name ao env_obj =
    let name,name' = name,fix_name name in
    let code = ref [] in
    let fname = sprintf "%s.ml" ao.ao_type.ta_name in
    let pathl = ""::(compiler.t_lib@compiler.t_incl) in        
    let rec f_open pathl =
      match pathl with
      | path :: tl ->
      begin
        try open_in (sprintf "%s%s%s" path (if path <> "" then "/" else "") fname)
        with _ -> f_open tl
      end;
      | [] -> err (sprintf "Can't open ML ADTO file <%s>." fname) in
    let ic = f_open pathl in
    let more = ref true in
    while !more 
    do
      more := protects (code := !code @ [input_line ic]);    
    done;
    close_in ic;

    (*
    ** Try to extract default environment parameter values
    *)
    let params = ref [] in
    List.iter (fun line ->
        let re = Str.regexp " *val mutable \(.+\) = \(.+\)" in
        let nv = Str.global_replace re "\1:\2" line in
        match Str.split (Str.regexp ":") nv with
        | [n;v] -> 
          params := !params @ [Str.global_replace (Str.regexp " ") "" n,
                               Str.global_replace (Str.regexp " ") "" v];

        | _ -> ();       
      ) !code;

    (*
    ** Extract environment depending values and methods
    *)

    let discard = ref false in
    let filter = ref false in
    let mclass = ref "" in
    
    List.iter (fun line ->
        let re1 = Str.regexp " *class *\([a-zA-Z_0-9]+\) .+ *when *\(.+\) *= *" in
        let re2 = Str.regexp " *class *\([a-zA-Z_0-9]+\) .+ *= *" in
        let re11 = Str.regexp " *class \['a\] *\([a-zA-Z_0-9]+\) .+ *when *\(.+\) *= *" in
        let re21 = Str.regexp " *class \['a\] *\([a-zA-Z_0-9]+\) .+ *= *" in
        let re3 = Str.regexp " *end class *" in
        let p1 = Str.global_replace re1 "\2" line in
        let p2 = Str.global_replace re2 "" line in
        let p11 = Str.global_replace re11 "\2" line in
        let p21 = Str.global_replace re21 "" line in
        let p3 = Str.global_replace re3 "" line in
        let in_discard = !discard in
        if p1 <> line then
        begin
          let v = eval p1 !params in
          discard := not v;
          if not !discard then 
          begin
            mclass := Str.global_replace re1 "\1" line;
            filter := true;
          end;
        end
        else if p11 <> line then
        begin
          let v = eval p11 !params in
          discard := not v;
          if not !discard then 
          begin
            mclass := Str.global_replace re11 "\1" line;
            filter := true;
          end;
        end
        else if p2 <> line  then
        begin
          mclass := Str.global_replace re2 "\1" line;
          discard := false;
        end
        else if p21 <> line  then
        begin
          mclass := Str.global_replace re21 "\1" line;
          discard := false;
        end
        else if p3 <> line  then
        begin
          discard := false;
          filter := true;
        end;
        ) !code;
    if !mclass = "" then err (sprintf "Can't find class for abstract object <%s>." name);
    env_add env name (C_abstract {
        abstr_name = name;
        abstr_env = ao.ao_env;
        abstr_code = [];
        abstr_type = env_obj;
        abstr_class = !mclass;
        abstr_class_type = ao.ao_type.ta_name;
        abstr_subst = if name' <> name then name' else "";
      }) in

  List.iter (fun fdef ->
    env_add env fdef.fun_name (C_fun {
      fn_name=fdef.fun_name;
      fn_args=List.map2 (fun a b -> a,b) fdef.fun_args fdef.fun_args_obj;
      fn_rets=List.map2 (fun a b -> a,b) fdef.fun_ret fdef.fun_ret_obj;
      fn_inline=fdef.fun_inline;
      })
    ) funs;
  List.iter (fun tp ->
    match tp with
    | Type_const tc ->
      env_add env tc.tc_name (C_enum {
        enum_name=tc.tc_name;
        enum_elems=List.map (fun ot -> 
            match ot with
            | OT_const co -> co.co_name;
            | _ -> progerr "tc_elems/=OT_const") tc.tc_elems;
        enum_numbers=List.map (fun ot -> 
            match ot with
            | OT_const co -> int_of_val co.co_init;
            | _ -> progerr "tc_elems/=OT_const") tc.tc_elems;
      })
    | Type_struct ts ->
      env_add env ts.ts_name  (C_struct {
        strut_name = ts.ts_name;
        strut_elems = List.map (fun te ->
          (te.te_name,(let dt,_ = te.te_type in dt))
          ) ts.ts_elems;
        strut_names = List.map (fun te ->
          sprintf "%s_%s" ts.ts_name te.te_name
          ) ts.ts_elems;
      })
    | Type_exc ex ->
      env_add env ex.tx_name (C_exception {
        exc_name=ex.tx_name;
        exc_id=ex.tx_id;
      });
      env_add env (sprintf "EXC%d" ex.tx_id) (C_exception {
        exc_name=ex.tx_name;
        exc_id=(-1);
      });
    | _ -> ();
    ) types;
  List.iter (fun ot ->
    match ot with
    | OT_signal co | OT_reg co | OT_var co ->
    begin
      match co.co_array with
      | [at] | [_;at] -> 
        let name,name' = at.at_name,fix_name at.at_name in
        env_add env at.at_name  (C_storage {
          sto_name = name;
          sto_mutable = true;
          sto_export = sym_check_obj modu.mod_export at.at_name;
          sto_subst = if name' <> name then name' else "";
          sto_def = None;
          sto_ref = false;
          sto_sym = false;
          sto_type = C_array {
            array_name = at.at_name;
            array_dims = at.at_dim;
            array_type = (
              match co.co_struct with
              | [st] -> 
                  if env_search (env::glob_env) st.st_type.ts_name then
                    env_get (env::glob_env) st.st_type.ts_name
                  else
                    err (sprintf "Unknown structure type <%s> found." st.st_type.ts_name)
              | _ ->
                  C_type co.co_type);
            }});
      | _ -> 
      begin
        match co.co_struct with
        | [st] ->
          let name,name' = st.st_name,fix_name st.st_name in          
          env_add env st.st_name  (C_storage {
            sto_name = name;
            sto_mutable = true;
            sto_export = sym_check_obj modu.mod_export st.st_name;
            sto_subst = if name' <> name then name' else "";
            sto_def = None;
            sto_ref = false;
            sto_sym = false;
            sto_type = (
              if env_search (env::glob_env) st.st_type.ts_name then
                env_get (env::glob_env) st.st_type.ts_name
              else
                err (sprintf "Unknown structure type <%s> found." st.st_type.ts_name));
            });
        | _ -> 
          let is_mut,is_fun,is_ref,subst = c_env_transform co.co_name in
          let name,name' = 
            if is_fun then
            begin
              let namel = Str.split (Str.regexp "_") subst in
              let fun_name,namel' = find namel "" None in
              co.co_name,sprintf "%s" (name_ namel')
            end
            else if subst = "" then co.co_name,fix_name co.co_name
            else co.co_name,subst in                     
          env_add env co.co_name  (C_storage {
            sto_name = name;
            sto_mutable = is_mut;
            sto_export = sym_check_obj modu.mod_export co.co_name;
            sto_subst = if name' <> name then name' else "";
            sto_type = (if co.co_type_name <> "" then
                          c_get_type (env::glob_env) co.co_type_name
                        else C_type co.co_type);
            sto_def = None;
            sto_sym = false;
            sto_ref = is_ref;
            })
      end;
    end;
    | OT_queue qu ->
    begin
      let co = qu.qu_obj in
      let ao = {qu.qu_ao with ao_type={qu.qu_ao.ao_type with ta_name="queue";}} in
      if not (List.mem "queue" !c_mod_dep) then c_mod_dep := !c_mod_dep @ ["queue"];
      match co.co_array with
      | [at] | [_;at] -> 
        add_ao at.at_name ao (C_array {
            array_name = at.at_name;
            array_dims = at.at_dim;
            array_type = (C_abstract_type {ao with ao_obj=Some (OT_queue qu)});
            });
      | _ -> 
      begin
        add_ao co.co_name ao (C_abstract_type ao); 
      end;
    end;
    | OT_array at ->
    begin
      match at.at_objs.(0) with
      | OT_signal co | OT_reg co | OT_var co ->
        let at' = match co.co_array with [at'] | [_;at'] -> at'
                  | _ -> err (sprintf "Array object <%s> without parent array <%s> link!" co.co_name at.at_name) in
        let name,name' = at'.at_name,fix_name at'.at_name in
        env_add env at'.at_name  (C_storage {
          sto_name = name;
          sto_mutable = true;
          sto_export = sym_check_obj modu.mod_export name;
          sto_subst = if name' <> name then name' else "";
          sto_def = None;
          sto_ref = false;
          sto_sym = false;
          sto_type = C_array {
            array_name = at'.at_name;
            array_dims = at'.at_dim;
            array_type = (
              match co.co_struct with
              | [st] -> 
                  if env_search (env::glob_env) st.st_type.ts_name then
                    env_get (env::glob_env) st.st_type.ts_name
                  else
                    err (sprintf "Unknown structure type <%s> found." st.st_type.ts_name)
              | _ ->
                  C_type co.co_type);
            }}); 
      | OT_queue qu ->
      begin
        let co = qu.qu_obj in
        let ao = {qu.qu_ao with ao_type={qu.qu_ao.ao_type with ta_name="queue";}} in

        let name,name' = at.at_name,fix_name at.at_name in
        add_ao at.at_name ao (C_array {
              array_name = at.at_name;
              array_dims = at.at_dim;
              array_type = (C_abstract_type {ao with ao_obj=Some (OT_queue qu)});
              });
        if not (List.mem "queue" !c_mod_dep) then c_mod_dep := !c_mod_dep @ ["queue"];
        
      end;
      | OT_object ao when (ao.ao_type.ta_name <> "process" && ao.ao_type.ta_name <> "function") ->
        let at' = match ao.ao_array with [at'] | [_;at'] -> at'
                  | _ -> err (sprintf "Array object <%s> without parent array <%s> link!" ao.ao_name at.at_name) in
        add_ao at'.at_name ao (C_array {
            array_name = at'.at_name;
            array_dims = at'.at_dim;
            array_type = C_abstract_type ao;});
        if not (List.mem ao.ao_type.ta_name  !c_mod_dep) then c_mod_dep := !c_mod_dep @ [ao.ao_type.ta_name];
      | _ -> ();     
    end;
    | OT_object ao when (ao.ao_type.ta_name <> "process" && ao.ao_type.ta_name <> "function") ->
      add_ao ao.ao_name ao (C_abstract_type ao) ;
      if not (List.mem ao.ao_type.ta_name  !c_mod_dep) then c_mod_dep := !c_mod_dep @ [ao.ao_type.ta_name];
    | OT_named_value (nv,v) ->
    begin
      let _,_,_,subst = c_env_transform nv in
      let dt = 
        match v with
        | V_int i64 -> 
          DT_int (const_width v)
        | V_bool _ -> DT_bool
        | V_char _ -> DT_char
        | V_logic s -> DT_logic (String.length s)
        | _ -> err (sprintf "Unsupported named value <%s> data type." nv) in
      let name,name' = 
        if subst = "" then nv,fix_name nv
        else nv,subst in                     
      env_add env nv  (C_storage {
            sto_name = name;
            sto_subst = if name' <> name then name' else "";
            sto_type = C_type dt;
            sto_def = Some v;
            sto_ref = false;
            sto_sym = false;
            sto_mutable = false;
            sto_export = false;
            })      
    end;
    | OT_const co ->
    begin
      let _,_,_,subst = c_env_transform co.co_name in
      let dt = 
        match co.co_init with
        | V_int i64 -> 
          DT_int (const_width co.co_init)
        | V_bool _ -> DT_bool
        | V_char _ -> DT_char
        | V_logic s -> DT_logic (String.length s)
        | _ -> err (sprintf "Unsupported constant value <%s> data type." co.co_name) in
      let name,name' = 
        if subst = "" then 
          co.co_name,(if co.co_type_name = "" then fix_name co.co_name else co.co_name)
        else 
          co.co_name,subst in                     
      env_add env co.co_name  (C_storage {
            sto_name = name;
            sto_subst = if name' <> name then name' else "";
            sto_type = (if co.co_type_name <> "" then
                          c_get_type (env::glob_env) co.co_type_name 
                        else C_type co.co_type);
            sto_def = Some co.co_init;
            sto_ref = false;
            sto_mutable = false;
            sto_sym = co.co_type_name <> "";
            sto_export = false;
            })      
    end;
    | _ -> ();
    ) objs;
  env 
  
  
let rec c_type  envo =
  match envo with
  | C_type dt ->
  begin
    match dt with
    | DT_int n  -> 
      if n < 31 then
        "int"
      else
        "int64"
    | DT_bool ->
      "bool"
    | DT_char ->
      "char"
    | DT_logic n ->
      if n < 31 then
        "int"
      else
        "int64"
    | _ -> err ("No rule to map data type.")
  end;
  | C_array ar ->
      sprintf "%s*" (c_type ar.array_type)
  | C_struct st ->
      sprintf "struct %s" st.strut_name
  | C_enum en -> en.enum_name
  | _ -> err (sprintf "No rule to map object type: %s" (sprint_env envo))


(*
** Return string representation of default/initial value
** for object <obj>
*)
let c_def  obj =
  let rec def with_ref obj =
    match obj with
    | C_enum en -> sprintf "%s" (List.hd en.enum_elems)
    | C_type dt  -> 
    begin
      match dt with
      | DT_logic n -> 
        if n < 31 then 
          "0" 
        else 
          "Int64.zero"
      | DT_int n -> 
        if n < 31 then 
          "0"
        else 
           "Int64.zero" 
      | DT_char -> "' '"
      | DT_bool -> "false"
      | _ -> "?C_type"
    end;
    | C_array at ->
    begin
      sprintf "(%s *)malloc(%d*sizeof(%s))"
              (c_type at.array_type)
              (proda at.array_dims)
              (c_type at.array_type)
    end;
    | C_struct st ->
    begin
      let s = ref "" in
      List.iter2 (fun (el,dt) el' ->
          s := sprintf "%s%s%s=%s" !s (if !s <> "" then "," else "")  el
                       (def false (C_type dt));
        ) st.strut_elems st.strut_names;  
      sprintf "{%s}" !s
    end;
    | C_abstract_type ao ->
    begin
      match ao.ao_obj with
      | Some (OT_queue qu) -> def false (C_type qu.qu_obj.co_type)
      | Some (OT_channel ch) -> def false (C_type ch.ch_obj.co_type)
      | _ -> err (sprintf "Can't provide default value for abstract object <%s>." ao.ao_name);
    end;
    | C_abstract ab -> def false ab.abstr_type
    | _ -> err ("No rule to map default value.") in
  def true obj

let c_val  v =
  match v with
  | V_int i64  ->
    let n = const_width v in
    if n < 31 then
      sprintf "%s" (Int64.to_string i64)
    else
      sprintf "Int64.of_string(\"%s\")" (Int64.to_string i64)
  | V_bool b  ->
    if b then "true" else "false"
  | V_char c  ->
    sprintf "'%c'" c
  | V_logic s ->
    if (String.length s) < 31 then
      sprintf "0x%d" (int_of_string (sprintf "0b%s" s))
    else
      sprintf "Int64.of_string(\"0x%s\")" (Int64.format "%x" (Int64.of_string (sprintf "0b%s" s)))
  | _ -> err ("No rule to map value.")

 
let c_expr_type envl pi =
  match pi with
  | PI_obj (opl,ot) ->
  begin
    let name = name_of_ot ot in
    if not (env_search envl name) then 
    begin
      let _,et = expr_type pi false in
      et      
    end
    else match env_get envl name with
    | C_storage sto ->
    begin
      match sto.sto_type with
      | C_enum en -> DT_object en.enum_name
      | _ -> 
        let _,et = expr_type pi false in
        et
    end;
    | _ -> 
      let _,et = expr_type pi false in
      et
  end; 
  | _ ->
    let _,et = expr_type pi false in
    et
  
(*
** Output all structures and type definitions 
*) 
let c_emit_types env  =
  let header =  [] in
     
  List.iter out header;
  let objs =list_of_sym env in
  List.iter (fun obj ->
    match obj with
    | C_struct st ->
      let s = ref (sprintf "struct %s {\n" (fix_name st.strut_name)) in
      List.iter2 (fun (el,dt) el' ->
        s := sprintf "%s  %s %s;\n" !s 
                    (match dt with
                      | DT_logic n -> if n < 31 then "int" else "int64"
                      | DT_int n  -> if n < 31 then "int" else "int64"
                      | DT_char  -> "char"
                      | DT_bool  -> "bool"
                      | _ -> err "No rule to map structure type."
                    ) (fix_name el);
                     
        ) st.strut_elems st.strut_names;
      s := sprintf "%s};" !s;
      out !s;
    | C_enum en ->
      let s = ref (sprintf "typedef enum { \n" ) in
      let n = ref (List.length en.enum_elems) in
      let i = ref 0 in
      List.iter (fun el ->
        decr n; incr i;
        s := sprintf "%s  %s=%d%s\n" !s el !i (if !n > 0 then "," else "");
        ) en.enum_elems;
      s := sprintf "%s} %s;" !s (fix_name en.enum_name);
      out !s;   
    | _ -> ();
    ) objs;
  List.iter (fun obj ->
    match obj with
    | C_exception exc -> 
      if exc.exc_id <> (-1) then
        out (sprintf "#define %s %d" exc.exc_name exc.exc_id);
        
    | _ -> ();
    ) objs

(*
** Output all object definitions
*)    
let c_emit_objs env  local =
  let rec lst2 il =
    match il with
    | (n,v) :: tl -> (sprintf "%s=%s" n v)^(if tl <> [] then ":" else "")^(lst2 tl)
    | [] -> "" in
  let defs = ref [] in
  let evals = ref [] in
  let objs =list_of_sym env in
  List.iter (fun obj ->
    match obj with
    | C_storage sto ->
      let is_fun =
          let check_fun_arg = Str.string_match (Str.regexp "ARG_FUN_") sto.sto_name 0 in
          let check_fun_ret_arg = Str.string_match (Str.regexp "RET_FUN_") sto.sto_name 0 in
          check_fun_arg || check_fun_ret_arg in
      let is_excep = Str.string_match (Str.regexp "PRO_.*_EXCEPTION") sto.sto_name 0 in
          
      if sto.sto_mutable && not is_fun && not is_excep then
      begin
        defs := !defs @ [
                sprintf "%s %s;" 
                (c_type sto.sto_type)
                (if sto.sto_subst <> "" then sto.sto_subst else sto.sto_name)
                ];
        let default = 
          match sto.sto_def with
          | Some v -> c_val  v
          | None -> (c_def  sto.sto_type) in
        if default <> "" && default.[0] <> '{' then
          evals := !evals @ [ 
                    sprintf "%s=%s;"
                    (if sto.sto_subst <> "" then sto.sto_subst else sto.sto_name)    
                    default
                    ]
        else
        begin
          (*
          ** Structure?
          *)
          let default' = String.sub default 1 ((String.length default)-2)  in
          let vl = Str.split (Str.regexp ",") default' in
          evals := !evals @ (List.map (fun v ->
            let vn = Str.split (Str.regexp "=") v in
            match vn with
            | [el;dv] ->  
              sprintf "%s.%s=%s;"
                    (if sto.sto_subst <> "" then sto.sto_subst else sto.sto_name)    
                    el dv
            | _ -> error 0 ("c_emit_objs: invalid default value list");
            ) vl);
        end;
      end;
    | C_abstract ab ->
    begin
      match ab.abstr_type with
      | C_abstract_type ao ->
        defs := !defs @ [
            sprintf "%s_t* %s;"
            (ab.abstr_class)
            (fix_name ab.abstr_name)
            ];
        evals := !evals @ [            
             sprintf "%s = %s_new(%s);"
            (fix_name ab.abstr_name)
            (ab.abstr_class)
            (if ab.abstr_env <> [] then sprintf "\"%s\"" (lst2 ab.abstr_env) else "\"\"")
            ];
      | C_array ar ->
        defs := !defs @ [
            sprintf "%s_t* %s[%d];"
            (ab.abstr_class)
            (fix_name ab.abstr_name)
            (proda ar.array_dims)
            ];
        for i = 0 to (proda ar.array_dims)-1
        do
          evals := !evals @ [            
               sprintf "%s[%d] = %s_new(%s);"
              (fix_name ab.abstr_name)
              i
              (ab.abstr_class)
              (if ab.abstr_env <> [] then sprintf "\"%s\"" (lst2 ab.abstr_env) else "\"\"")
              ];
        done;
      | _ -> ();
    end;
    | _ -> ();
    ) objs;
  !defs, !evals
  

(*
** Output all object declarations
*)    
let c_emit_objs_int env  local =
  let rec lst2 il =
    match il with
    | (n,v) :: tl -> (sprintf "%s=%s" n v)^(if tl <> [] then ":" else "")^(lst2 tl)
    | [] -> "" in
  let rec get_type tp =
    match tp with
    | C_type dt -> 
    begin
      match dt with
      | DT_logic n | DT_int n -> if n < 31 then "int" else "int64"
      | DT_char -> "char"
      | DT_bool -> "bool"
      | _ -> "?C_type"
    end;
    | C_enum en -> en.enum_name
    | C_array ar ->
      sprintf "%s*" (get_type ar.array_type)
    | C_struct st ->
      sprintf "struct %s" st.strut_name
    | _ -> "?get_type"  in
      
  let objs =list_of_sym env in
  List.iter (fun obj ->
    match obj with
    | C_storage sto ->
      let is_fun =
          let check_fun_arg = Str.string_match (Str.regexp "ARG_FUN_") sto.sto_name 0 in
          let check_fun_ret_arg = Str.string_match (Str.regexp "RET_FUN_") sto.sto_name 0 in
          check_fun_arg || check_fun_ret_arg in
      let is_excep = Str.string_match (Str.regexp "PRO_.*_EXCEPTION") sto.sto_name 0 in
          
      if sto.sto_mutable && not is_fun && not is_excep then
        out (sprintf "extern %s %s;"
                   (get_type sto.sto_type)
                   (if sto.sto_subst <> "" then sto.sto_subst else sto.sto_name)
                    );
    | C_abstract ab ->
    begin
      match ab.abstr_type with
      | C_abstract_type ao ->
        out (sprintf "extern %s_t* %s;"
                     (ab.abstr_class)
                     (fix_name ab.abstr_name)
                     );
      | C_array ar ->
        out (sprintf "extern %s_t* %s[];"
                     (ab.abstr_class)
                     (fix_name ar.array_name)
                     );
      | _ -> ();
    end;
    | _ -> ();
    ) objs

(*
** Output all constant definitions
*)    
let c_emit_const env  local =
  ()    

(*
** Output all constant declarations
*)    
let c_emit_const_int env  local =
  let rec lst2 il =
    match il with
    | (n,v) :: tl -> (sprintf "%s=%s" n v)^(if tl <> [] then ":" else "")^(lst2 tl)
    | [] -> "" in
    
  let objs =list_of_sym env in
  let consts = List.sort (fun a b -> if a.sto_name > b.sto_name then 1 else -1)
    (List.map (fun obj ->
        match obj with
        | C_storage sto -> sto
        | _ -> progerr "c_emit_const: consts")
      (List.filter (fun obj ->
        match obj with
        | C_storage sto -> not sto.sto_mutable && not sto.sto_sym && sto.sto_def <> None
        | _ -> false      
        ) objs)) in
  
  List.iter (fun sto ->
        out (sprintf "#define %25s %10s"
                   (if sto.sto_subst <> "" then sto.sto_subst else sto.sto_name)
                   (match sto.sto_def with
                    | Some v -> c_val  v
                    | None -> (c_def  sto.sto_type))
            );
    ) consts



(*
** Transform expression operation
*)
let rec c_op et op op1 op2 =
  match et with
  | DT_logic n | DT_int n ->
    if n < 31 then
    begin
      match op with
      | OP_add -> sprintf  "%s + %s" op1 op2
      | OP_sub -> sprintf  "%s - %s" op1 op2
      | OP_mul -> sprintf  "%s * %s" op1 op2
      | OP_div -> sprintf  "%s / %s" op1 op2
      | OP_eq  -> sprintf  "%s == %s" op1 op2
      | OP_neq -> sprintf  "%s != %s" op1 op2
      | OP_lt  -> sprintf  "%s < %s" op1 op2
      | OP_le  -> sprintf  "%s <= %s" op1 op2
      | OP_gt  -> sprintf  "%s > %s" op1 op2
      | OP_ge  -> sprintf  "%s >= %s" op1 op2
      | OP_land -> sprintf  "%s & %s" op1 op2
      | OP_lor  -> sprintf  "%s | %s" op1 op2
      | OP_lxor  -> sprintf  "%s ^ %s" op1 op2
      | OP_lsl -> sprintf  "%s << %s" op1 op2
      | OP_lsr -> sprintf  "%s >> %s" op1 op2
      | _ -> "?OP"
    end
    else
    begin
      match op with
      | OP_add -> sprintf  "Int64.add(%s,%s)" op1 op2
      | OP_sub -> sprintf  "Int64.sub(%s,%s)" op1 op2
      | OP_mul -> sprintf  "Int64.mul(%s,%s)" op1 op2
      | OP_div -> sprintf  "Int64.div(%s,%s)" op1 op2
      | OP_eq  -> sprintf  "%s == %s" op1 op2
      | OP_neq -> sprintf  "%s != %s" op1 op2
      | OP_lt  -> sprintf  "%s < %s" op1 op2
      | OP_le  -> sprintf  "%s <= %s" op1 op2
      | OP_gt  -> sprintf  "%s > %s" op1 op2
      | OP_ge  -> sprintf  "%s >= %s" op1 op2
      | OP_land -> sprintf  "Int64.logand(%s,%s)" op1 op2
      | OP_lor  -> sprintf  "Int64.logor(%s,%s)" op1 op2
      | OP_lxor  -> sprintf  "Int64.logxor(%s,%s)" op1 op2
      | OP_lsl  -> sprintf  "Int64.shift_left(%s,%s)" op1 op2
      | OP_lsr  -> sprintf  "Int64.shift_right(%s,%s)" op1 op2
      | _ -> "?OP"
    end;
  | DT_char -> "?"
  | DT_bool ->
    begin
      match op with
      | OP_band -> sprintf  "%s && %s" op1 op2
      | OP_bor  -> sprintf  "%s || %s" op1 op2
      | OP_bxor  -> sprintf  "(%s ^ %s)" op1 op2 
      | OP_eq  -> sprintf  "%s == %s" op1 op2
      | OP_neq -> sprintf  "%s != %s" op1 op2
      | OP_lt  -> sprintf  "%s < %s" op1 op2
      | OP_le  -> sprintf  "%s <= %s" op1 op2
      | OP_gt  -> sprintf  "%s > %s" op1 op2
      | OP_ge  -> sprintf  "%s >= %s" op1 op2
      | _ -> "?OP"
    end;    
  | _ -> "?c_op"

(*
** Map structure selector elements to C substitutes...
*)
let c_sel_map desc = 
  let rec map namel1 namel2 name =
    match namel1,namel2 with
    | (name1::tl1),((name2,_)::tl2) ->
    begin
      if name = name2 then name1 else
        map tl1 tl2 name
    end;
    | _ -> name in
  match desc.desc_obj with
  | C_storage sto ->
  begin
    match sto.sto_type with
    | C_struct st ->
       desc.desc_sel
    | C_array ar ->
    begin
      match ar.array_type with
      | C_struct st ->
         desc.desc_sel
      | _ -> desc.desc_sel
    end;
    | _ -> desc.desc_sel
  end; 
  | _ -> desc.desc_sel

(*
** Returns string representation of object access (read & write)
*)
let rec c_obj env  et lhs ot opl =
  let rec index il =
    match il with
    | hd :: tl -> (sprintf "[%d]" hd)^(index tl)
    | [] -> "" in
  let rec sel il =
    match il with
    | hd :: tl -> (sprintf ".%s" hd)^(sel tl)
    | [] -> "" in
  let rec args il =
    match il with
    | hd :: tl -> (hd)^(if tl <> [] then "," else "")^(args tl)
    | [] -> "" in
  let dt = 
    match dt_of_ot ot with
    | Some dt -> dt
    | None -> err "OT without real DT!" in
  let n = size_of_dt dt in
  
  let conv et desc expr =
    let rec get_dt obj = 
      match obj with
      | C_storage sto ->
        get_dt sto.sto_type;
      | C_abstract ab -> 
        get_dt ab.abstr_type;
      | C_type dt -> dt;
      | C_abstract_type ao -> 
      begin
        match ao.ao_obj with
        | Some ot -> get_some (dt_of_ot ot)
        | None -> err (sprintf "Abstract object with out real type <%s>." ao.ao_name);
      end;
      | C_struct st ->
        let _,dt = List.find (fun (sel,_) -> [sel] = desc.desc_sel) st.strut_elems in
        dt
      | C_array ar ->
        get_dt ar.array_type;
      | C_enum en -> DT_object en.enum_name
      | _ -> err "Can't determine data type." in 
    let dt = get_dt desc.desc_obj in
    if lhs then expr else match et with
    | DT_logic n ->
    begin
        match dt with
        | DT_int n' | DT_logic n' ->
          if n < 31 && n' < 31 then expr
          else if n < 31 && n' > 30 then sprintf "Int64.to_int(%s)" expr
          else if n > 30 && n' < 31 then sprintf "Int64.of_int(%s)" expr
          else expr
        | DT_char -> 
          if n < 31 then sprintf "int_of_char(%s)" expr
          else sprintf "Int64.of_int (int_of_char(%s))" expr
        | DT_bool ->
          if n < 31 then sprintf "(%s==true?1:0)" expr
          else sprintf "(%s==true?Int64.one:Int64.zero)" expr
        | DT_object o ->
          (*
          ** Enumeration type
          *) 
          let n' = 20 in
          if n < 31 && n' < 31 then expr
          else if n < 31 && n' > 30 then sprintf "Int64.to_int(%s)" expr
          else if n > 30 && n' < 31 then sprintf "Int64.of_int(%s)" expr
          else expr
          
        | _ -> "?c_obj.logic"
    end;
    | DT_int n ->
    begin
        match dt with
        | DT_int n'  ->
          if n < 31 && n' < 31 then expr
          else if n < 31 && n' > 30 then sprintf "Int64.to_int(%s)" expr
          else if n > 30 && n' < 31 then sprintf "Int64.of_int(%s)" expr
          else expr
        | DT_logic n' ->
          if n < 31 && n' < 31 then sprintf "sign(%d,%s)" (max n n') expr
          else if n < 31 && n' > 30 then sprintf "Int64.to_int(%s)" expr
          else if n > 30 && n' < 31 then sprintf "Int64.of_int(%s)" expr
          else expr
        | DT_char -> 
          if n < 31 then sprintf "int_of_char(%s)" expr
          else sprintf "Int64.of_int (int_of_char (%s))" expr
        | DT_bool ->
          if n < 31 then sprintf "(%s==true? 1 : 0)" expr
          else sprintf "(%s==true? Int64.one : Int64.zero)" expr
        | _ -> "?c_obj.int"
    end;
    | DT_char ->
    begin
        match dt with
        | DT_int n' | DT_logic n' ->
          if n' < 31 then sprintf "char_of_int(%s)" expr
          else sprintf "char_of_int (Int64.to_int (%s))" expr
        | DT_char -> expr
        | DT_bool ->
          sprintf "(%s==true? 't' : 'f')" expr
        | _ -> "?c_obj.char"      
    end;
    | DT_bool ->
    begin
        match dt with
        | DT_int n' | DT_logic n' ->
          if n' < 31 then sprintf "(%s == 1)" expr
          else sprintf "(%s == Int64.one)" expr
        | DT_char -> sprintf "(%s == 't')" expr
        | DT_bool -> expr
        | _ -> "?c_obj.bool"      
    end;
    | DT_object n1 ->
    begin
      match dt with
      | DT_object n2 -> 
        if n1 <> n2 then err (sprintf "Expected type <%s>, but got <%s>." n1 n2);
        expr
      | _ -> "?c_obj.obj"
    end;
    | _ -> "?c_obj" in
  
  let sub =
    if is_sub opl then
    begin
      let a,b = obj_sub opl in
      sprintf "%d,%d" a b
    end 
    else if is_index opl then
    begin
      let a = obj_index opl in
      sprintf "%d,%d" a a
    end 
    else if is_index_obj opl then
    begin
      match obj_index_obj opl with
      | PI_obj (_,ot) ->
        let i = c_obj env  (DT_int 0) false ot [] in
        sprintf "%s,%s" i i
      | _ -> "?"
    end
    else "" in    
    
  let neg =
    let rec is_neg opl = 
      match opl with 
      | op :: tl -> if op = OD_aneg || op = OD_lneg then true else is_neg tl
      | [] -> false in
    let rec obj_neg opl = 
      match opl with 
      | op :: tl -> if op = OD_aneg || op = OD_lneg then op else obj_neg tl
      | [] -> progerr "obj_neg" in
      
    if is_neg opl then
    begin
      match obj_neg opl with
      | OD_lneg -> if dt = DT_bool then "!" else  "~"
      | OD_aneg -> if n > 30 then "Int64.neg" else "-"
      | _ -> progerr "obj_neg"
    end
    else "" in
 
  match ot with
  | OT_signal co
  | OT_var co 
  | OT_reg co 
  | OT_const co -> 
  begin
    let desc = c_obj_desc env co.co_name co.co_array co.co_struct in
    if lhs && sub <> "" then c_expr_close := ")";
    conv et desc (
    if not desc.desc_array && not desc.desc_struct then
      sprintf "%s%s%s%s%s%s%s%s"
        (if neg <> "" then sprintf "%s(" neg else "") 
        (if sub <> "" && not lhs then sprintf "%s(%s," (if n < 31 then "bit" else "bit64") sub 
         else "")
        (if desc.desc_ref then "*" else "")
        (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name)
        (if sub <> "" && not lhs then ")" else "")
        (if lhs then " =" else "")
        (if sub <> "" && lhs then
           sprintf " %s(%s,%s," 
              (if n < 31 then "bitw" else "bitw64")
              sub 
              (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name)
         else "")
        (if neg <> "" then ")" else "")
    else if desc.desc_array && not desc.desc_struct then
      sprintf "%s%s%s%s%s%s%s%s"  
        (if neg <> "" then sprintf "(%s" neg else "") 
        (if sub <> "" && not lhs then sprintf "%s(%s," (if n < 31 then "bit" else "bit64") sub 
         else "")
        (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name) 
        (index desc.desc_num_sel)
        (if sub <> "" && not lhs then ")" else "")
        (if lhs then " =" else "")
        (if sub <> "" && lhs then
           sprintf " %s(%s,%s%s," sub 
           (if n < 31 then "bitw" else "bitw64")
           (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name)
           (index desc.desc_num_sel)
         else "")
        (if neg <> "" then ")" else "")
    else if not desc.desc_array && desc.desc_struct then
      sprintf "%s%s%s%s%s%s%s%s"  
        (if neg <> "" then sprintf "%s(" neg else "") 
        (if sub <> "" && not lhs then sprintf "%s(%s," (if n < 31 then "bit" else "bit64") sub 
         else "")
        (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name) 
        (sel (c_sel_map desc))
        (if sub <> "" && not lhs then ")" else "")
        (if lhs then " =" else "")
        (if sub <> "" && lhs then
           sprintf " %s(%s,%s%s," sub 
           (if n < 31 then "bitw" else "bitw64")
           (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name)
           (sel (c_sel_map desc))
         else "")
        (if neg <> "" then ")" else "")
    else err "No rule to map object.");
  end;
  | OT_queue qu ->
  begin
    let co = qu.qu_obj in
    let desc = c_obj_desc env co.co_name co.co_array co.co_struct in
    if lhs then c_expr_close := ")";
    conv et desc (
    if not desc.desc_array && not desc.desc_struct && desc.desc_mutable then 
      sprintf "%s->%s(%s%s" 
        (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name) 
        (if lhs then "write" else "read")
        (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name)
        (if lhs then "," else ")")
    else if not desc.desc_array && not desc.desc_struct && not desc.desc_mutable then
      sprintf "%s->%s(%s%s"
        (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name) 
        (if lhs then "write" else "read")
        (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name)
        (if lhs then "," else ")")
    else if desc.desc_array && not desc.desc_struct then
      sprintf "%s%s->%s(%s%s%s"  
        (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name) 
        (index desc.desc_num_sel)
        (if lhs then "write" else "read")
        (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name) 
        (index desc.desc_num_sel)
        (if lhs then "," else ")")
    else if not desc.desc_array && desc.desc_struct then
      sprintf "%s->%s(%s%s%s"  
        (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name) 
        (if lhs then "write" else "read")
        (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name) 
        (sel (c_sel_map desc))
        (if lhs then "," else ")")
    else err "No rule to map object."); 
  end;  
  | OT_array at ->
  begin
    match at.at_objs.(0) with
    | OT_reg _ | OT_var _ | OT_signal _ ->
      let desc = c_obj_desc env at.at_name [at] [] in
      let at_sel = 
        if is_sel opl then
          (index (Array.to_list (obj_sel opl)))
        else if is_sel_obj opl then
          sprintf "[%s]" (c_expr env  false (DT_int 0) (obj_sel_obj opl))
        else "?"  in
      if lhs && sub <> "" then c_expr_close := ")";
      conv et desc (sprintf "%s%s%s%s%s%s%s%s"
        (if neg <> "" then sprintf "%s(" neg else "") 
        (if sub <> "" && not lhs then sprintf "bit(%s," sub 
         else "")
        (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name)
        at_sel
        (if desc.desc_struct then (sel (c_sel_map desc)) else "")
        (if lhs then " =" else "")
        (if sub <> "" && lhs then
           sprintf " bitw(%s,%s%s," sub 
           (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name)
           at_sel
         else "")
         (if neg <> "" then ")" else ""))
    | OT_queue qu ->
      let desc = c_obj_desc env at.at_name [at] [] in
      let at_sel = 
        if is_sel opl then
          (index (Array.to_list (obj_sel opl)))
        else if is_sel_obj opl then
          sprintf "[%s]" (c_expr env  false (DT_int 0) (obj_sel_obj opl))
        else "?"  in
      if lhs then c_expr_close := ")";
      conv et desc (sprintf "%s%s->%s(%s%s%s%s" 
        (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name) 
        at_sel
        (if lhs then "write" else "read")
        (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name) 
        at_sel
        (if desc.desc_struct then (sel (c_sel_map desc)) else "")
        (if lhs then "," else ")"))
    | OT_object ao -> "?#?"
    | _ -> err (sprintf "Unexpected array element type found <%s>" at.at_name) 
  end;
  | OT_value v ->
  begin
    let rec sv v =
      match v with
      | V_int i64 ->
      begin
        match et with
        | DT_logic n | DT_int n | DT_natural n ->
          if n < 31 then sprintf "%s%s%s" 
              (if neg <> "" then sprintf "%s(" neg else "")
              (Int64.to_string i64) 
              (if neg <> "" then ")" else "") 
          else if i64 = Int64.one then sprintf "%s%s%s" 
              (if neg <> "" then sprintf "%s(" neg else "")
               "Int64.one" 
              (if neg <> "" then ")" else "")
          else if i64 = Int64.zero then sprintf "%s%s%s" 
              (if neg <> "" then sprintf "%s(" neg else "")
              "Int64.zero"
              (if neg <> "" then ")" else "")            
          else sprintf "%s(Int64.of_string \"%s\")%s" 
              (if neg <> "" then sprintf "%s(" neg else "")
              (Int64.to_string i64)
              (if neg <> "" then ")" else "")            
        | DT_char ->
          sprintf "'%c'" (char_of_int (Int64.to_int i64))
        | DT_bool ->
          if i64=Int64.one then "true" else "false"
        | _ -> "V_int?"
      end;
      | V_bool b ->
      begin
        match et with
        | DT_logic n | DT_int n | DT_natural n ->
          if n < 31 then (if b then "1" else "0") else 
                         (if b then "Int64.one" else "Int64.zero")
        | DT_char ->
          if b then "'t'" else "'f'"
        | DT_bool ->
          if b then "true" else "false"
        | _ -> "V_bool?"      
      end;
      | V_char c ->
      begin
        match et with
        | DT_logic n | DT_int n |  DT_natural n ->
          if n < 31 then sprintf "%d" (int_of_char c) else 
                         sprintf "Int64.of_string(\"%s\")" (Int64.to_string
                         (Int64.of_int (int_of_char c)))
        | DT_char ->
          sprintf "'%c'" c
        | DT_bool ->
          if c='t' then "true" else "false"
        | _ -> "V_char?"      
      end;
      | V_logic s ->
      begin
        let n' = String.length s in
        match et with
        | DT_logic n | DT_int n | DT_natural n ->
          if n < 31 then sprintf "0x%x" (int_of_string (sprintf "0b%s" s))
          else sprintf "Int64.of_string(\"0x%s\")" (Int64.format "%x" (Int64.of_string (sprintf "0b%s" s)))
        | DT_char ->
          sprintf "char_of_int(0b%s)" s
        | DT_bool ->
          sprintf "(0x%x=1)" (int_of_string (sprintf "0b%s" s))
        | _ -> "V_logic?"
      end;
      | V_list vl ->
        sprintf "{%s}" (args (List.map sv vl))
      | V_freq (i64,fu) ->
      begin
        match fu with
        | Hz -> Int64.to_string i64
        | Khz -> Int64.to_string (Int64.mul i64 (Int64.of_int 1000))  
        | Mhz -> Int64.to_string (Int64.mul i64 (Int64.of_int 1000000))  
        | Ghz -> Int64.to_string (Int64.mul i64 (Int64.of_int 1000000000))  
      end;
      | V_time (i64,tu) ->
      begin
        match tu with
        | Nsec -> Int64.to_string (Int64.div i64 (Int64.of_int 1000))  
        | Usec -> Int64.to_string i64
        | Cycles -> Int64.to_string i64
        | Msec -> Int64.to_string (Int64.mul i64 (Int64.of_int 1000))  
        | Sec -> Int64.to_string (Int64.mul i64 (Int64.of_int 1000000))  
      end;
      | V_string s -> sprintf "\"%s\"" s
      | _ -> "?V" in
    sv v
  end;
  | OT_named_value (nv,v) ->
  begin
    match v with
    | V_int i64 ->
    begin
      let n' = const_width v in
      match et with
      | DT_logic n | DT_int n | DT_natural n ->
        if n < 31 && n' > 30 then sprintf "Int64.to_int(%s)" (fix_name nv) 
        else if n > 30 && n' < 31 then sprintf "Int64.of_int(%s)" (fix_name nv) 
        else (fix_name nv)
      | DT_char ->
        if n' < 31 then sprintf "char_of_int(%s)" (fix_name nv)
        else sprintf "char_of_int(Int64.to_int (%s))" (fix_name nv)
      | DT_bool ->
        sprintf "(%s==Int64.one)" (fix_name nv)
      | _ -> "?"
    end;
    | V_bool b ->
    begin
      match et with
      | DT_logic n | DT_int n | DT_natural n ->
        if n < 31 then (sprintf "(%s==true? 1 : 0)" (fix_name nv)) else 
                       (sprintf "(%s==true ? Int64.one : Int64.zero)" (fix_name nv))
      | DT_char ->
        (sprintf "(%s==true? 't' : 'f')" (fix_name nv)) 
      | DT_bool ->
        (fix_name nv)
      | _ -> "?"      
    end;
    | V_char c ->
    begin
      match et with
      | DT_logic n | DT_int n |  DT_natural n ->
        if n < 31 then sprintf "int_of_char(%s)" (fix_name nv) else 
                       sprintf "Int64.of_int (int_of_char (%s))" (fix_name nv)
      | DT_char ->
        (fix_name nv)
      | DT_bool ->
        sprintf "(%s=='t' ? true : false)" (fix_name nv)
      | _ -> "?"      
    end;
    | V_logic s ->
    begin
      let n' = String.length s in
      match et with
      | DT_logic n | DT_int n | DT_natural n ->
        if n < 31 && n' > 30 then sprintf "Int64.to_int(%s)" (fix_name nv) 
        else if n > 30 && n' < 31 then sprintf "Int64.of_int(%s)" (fix_name nv) 
        else (fix_name nv)
      | DT_char ->
        if n' < 31 then sprintf "char_of_int(%s)" (fix_name nv)
        else sprintf "char_of_int (Int64.to_int (%s))" (fix_name nv)
      | DT_bool ->
        sprintf "(%s==Int64.one)" (fix_name nv)
      | _ -> "?"
    end;
    | _ -> "?"
  end;
  | _ -> "OT?"
(*
** Returns string representation of (nested) expression
*)    
and c_expr env  lhs et pi =
  match pi with
  | PI_obj (opl,ot) -> c_obj env  et lhs ot opl
  | PI_arithm (op,op1,op2) ->
    let is_expr op = match op with | PI_arithm _ | PI_bool _ -> true|_ -> false in
    let par = (is_expr op1) || (is_expr op2) in
    sprintf "%s%s%s"
      (if par then "(" else "")
      (c_op et op
        (c_expr env  false et op1) 
        (c_expr env  false et op2))
      (if par then ")" else "")
  | PI_bool (kind,op,op1,op2) -> 
    let is_expr op = match op with | PI_arithm _ | PI_bool _ -> true|_ -> false in
    let par = (is_expr op1) || (is_expr op2) in
    let et = 
      match kind with
      | Cp_syntax.Bool -> DT_bool
      | Cp_syntax.Relational -> let et = c_expr_type env pi in et in
       
    sprintf "%s%s%s"
      (if par then "(" else "")
      (c_op et op
        (c_expr env  false et op1) 
        (c_expr env  false et op2))
      (if par then ")" else "")
  | _ -> "?"    

(*
** Extract process instruction depenendencies (function/process calls)
*)
let rec c_instr_dep env  local  pi =
  match pi with
  | PI_assign (src,lhs,rhs) ->
  begin
    line src;
    []
  end;
  | PI_block (il,bf) -> 
    List.flatten (List.map (c_instr_dep env  local) il)
  | PI_list il -> List.flatten (List.map (c_instr_dep env local) il)
  | PI_forloop (src,expr,dir,lim1,lim2,block) ->
    line src;
    c_instr_dep env  local block
  | PI_loop (src,Cp_syntax.Loop,expr,block)
  | PI_loop (src,Cp_syntax.Loop_while,expr,block) ->
    line src;
    c_instr_dep env local block 
  | PI_branch (src,expr,b1,b2) ->
    line src;
    (c_instr_dep env  local b1)@
    (match b2 with
     | PI_nop | PI_block ([PI_nop],_) -> []
     | _ ->
     begin
       c_instr_dep env local b2
     end)
  | PI_select (src,expr,casel) ->
    line src;
    c_instr_dep env  local casel
  | PI_case (src,exprl,b) ->
    line src;
    c_instr_dep env  local  b
  | PI_try (b,casel) ->
    (c_instr_dep env  local b)@ 
    (c_instr_dep env  local casel)
  | PI_raise exn -> []
  | PI_waitfor (src,expr,n,tu,ef,et) ->
    line src;
    []
  | PI_fun (src,(opl,ot),sel,argl) ->
  begin
    line src;
      
    match ot with
    | OT_object ao when (ao.ao_type.ta_name = "function") -> 
      let name = Str.global_replace (Str.regexp "function_\(.+\)") "\1" ao.ao_name in
      if not (List.mem "process" !c_mod_dep) then c_mod_dep := !c_mod_dep @ ["process"];
      if not (List.mem "mutex" !c_mod_dep) then c_mod_dep := !c_mod_dep @ ["mutex"];
      [sprintf "FUN_%s" name]
    | OT_object ao when (ao.ao_type.ta_name = "process") -> 
      let name = Str.global_replace (Str.regexp "function_\(.+\)") "\1" ao.ao_name in
      if not (List.mem ao.ao_type.ta_name !c_mod_dep) then c_mod_dep := !c_mod_dep @ [ao.ao_type.ta_name];
      [name]
    | OT_object ao when (sel = "interface") -> 
      []
    | OT_object ao ->
      if not (List.mem ao.ao_type.ta_name !c_mod_dep) then c_mod_dep := !c_mod_dep @ [ao.ao_type.ta_name];
      []
    | OT_array at ->
    begin
      match at.at_objs.(0) with
      | OT_object ao when (ao.ao_type.ta_name = "function") ->
        if not (List.mem "process" !c_mod_dep) then c_mod_dep := !c_mod_dep @ ["process"];
        if not (List.mem "mutex" !c_mod_dep) then c_mod_dep := !c_mod_dep @ ["mutex"];
        [sprintf "FUN_%s" (Str.global_replace (Str.regexp "function_\(.+\)") "\1" at.at_name)]
      | OT_object ao when (ao.ao_type.ta_name = "process") ->
        if not (List.mem ao.ao_type.ta_name !c_mod_dep) then c_mod_dep := !c_mod_dep @ ["process"];
        [Str.global_replace (Str.regexp "function_\(.+\)") "\1" at.at_name ]
      | OT_object ao when (sel = "interface") -> 
        []
      | OT_object ao ->
        if not (List.mem ao.ao_type.ta_name !c_mod_dep) then c_mod_dep := !c_mod_dep @ [ao.ao_type.ta_name];
       []
      | _ -> []
    end;
    | _ -> [];
  end;  
  | _ -> []
  
let c_dep_instr env  local pil =
  List.flatten (List.map (c_instr_dep env  local) pil)

(*
** Transform process instructions, returns
** string representation list
*)

let rec c_instr envl  local indent pi =
  let rec index il =
    match il with
    | hd :: tl -> (sprintf "[%d]" hd)^(index tl)
    | [] -> "" in
  let rec sel il =
    match il with
    | hd :: tl -> (sprintf ".%s" hd)^(sel tl)
    | [] -> "" in
  let rec args il =
    match il with
    | hd :: tl -> (hd)^(if tl <> [] then "," else "")^(args tl)
    | [] -> "" in
  let rec cargs il =
    match il with
    | hd :: tl -> ("case "^hd)^(if tl <> [] then " " else "")^(args tl)
    | [] -> "" in
  let rec args' il =
    match il with
    | hd :: tl -> (hd)^(if tl <> [] then "," else "")^(args' tl)
    | [] -> "" in


  (*
  ** Remove nested blocks of single instructions
  *)
  let rec flatten pil =
    match pil with
    | [PI_block ([i],bf)] -> (flatten [i])
    | [PI_block (il,bf)] -> [PI_block (flatten il,bf)]
    | (PI_block ([i],_)) :: tl  -> (flatten [i])@(flatten tl)
    | (PI_block (il,bf)) :: tl  -> [PI_block (flatten il,bf)]@(flatten tl)
    | pi :: tl -> [pi]  @ (flatten tl) 
    | [] -> [] in
    
  
  let rec filter pil =
    let pil' = flatten pil in
    (*
    ** Search for user function call patterns....
    *)
    match pil' with
    | (PI_block (il,bf)) :: tl -> (filter (flatten il)) @ (filter tl);
    | (PI_fun (_,_,"lock",_)) :: 
      (PI_fun (a,b,"call",c)) :: 
      (PI_fun (_,_,"unlock",_)) :: tl ->
      (c_emit_fun_call envl local indent [] [] b) @ (filter tl)
    | (PI_fun (_,_,"lock",_)) :: 
      (PI_assign (_,_,arg1)) :: 
      (PI_fun (a,b,"call",c)) :: 
      (PI_fun (_,_,"unlock",_)) :: tl ->
      (c_emit_fun_call envl local indent [arg1] [] b) @ (filter tl)
    | (PI_fun (_,_,"lock",_)) :: 
      (PI_assign (_,_,arg1)) :: 
      (PI_assign (_,_,arg2)) :: 
      (PI_fun (a,b,"call",c)) :: 
      (PI_fun (_,_,"unlock",_)) :: tl ->
      (c_emit_fun_call envl local indent [arg1;arg2] [] b) @ (filter tl)
    | (PI_fun (_,_,"lock",_)) :: 
      (PI_assign (_,_,arg1)) :: 
      (PI_assign (_,_,arg2)) :: 
      (PI_assign (_,_,arg3)) :: 
      (PI_fun (a,b,"call",c)) :: 
      (PI_fun (_,_,"unlock",_)) :: tl ->
      (c_emit_fun_call envl local indent [arg1;arg2;arg3] [] b) @ (filter tl)
    | (PI_fun (_,_,"lock",_)) :: 
      (PI_assign (_,_,arg1)) :: 
      (PI_assign (_,_,arg2)) :: 
      (PI_assign (_,_,arg3)) :: 
      (PI_assign (_,_,arg4)) :: 
      (PI_fun (a,b,"call",c)) :: 
      (PI_fun (_,_,"unlock",_)) :: tl ->
      (c_emit_fun_call envl local indent [arg1;arg2;arg3;arg4] [] b) @ (filter tl)
    | (PI_fun (_,_,"lock",_)) :: 
      (PI_assign (_,_,arg1)) :: 
      (PI_assign (_,_,arg2)) :: 
      (PI_assign (_,_,arg3)) :: 
      (PI_assign (_,_,arg4)) :: 
      (PI_assign (_,_,arg5)) :: 
      (PI_fun (a,b,"call",c)) :: 
      (PI_fun (_,_,"unlock",_)) :: tl ->
      (c_emit_fun_call envl local indent [arg1;arg2;arg3;arg4;arg5] [] b) @ (filter tl)
    | (PI_fun (_,_,"lock",_)) :: 
      (PI_fun (a,b,"call",c)) :: 
      (PI_assign (_,r1,_)) :: 
      (PI_fun (_,_,"unlock",_)) :: tl ->
      (c_emit_fun_call envl local indent [] [r1] b) @ (filter tl)
    | (PI_fun (_,_,"lock",_)) :: 
      (PI_assign (_,_,arg1)) :: 
      (PI_fun (a,b,"call",c)) :: 
      (PI_assign (_,r1,_)) :: 
      (PI_fun (_,_,"unlock",_)) :: tl ->
      (c_emit_fun_call envl local indent [arg1] [r1] b) @ (filter tl)
    | (PI_fun (_,_,"lock",_)) :: 
      (PI_assign (_,_,arg1)) :: 
      (PI_assign (_,_,arg2)) :: 
      (PI_fun (a,b,"call",c)) :: 
      (PI_assign (_,r1,_)) :: 
      (PI_fun (_,_,"unlock",_)) :: tl ->
      (c_emit_fun_call envl local indent [arg1;arg2] [r1] b) @ (filter tl)
    | (PI_fun (_,_,"lock",_)) :: 
      (PI_assign (_,_,arg1)) :: 
      (PI_assign (_,_,arg2)) :: 
      (PI_assign (_,_,arg3)) :: 
      (PI_fun (a,b,"call",c)) :: 
      (PI_assign (_,r1,_)) :: 
      (PI_fun (_,_,"unlock",_)) :: tl ->
      (c_emit_fun_call envl local indent [arg1;arg2;arg3] [r1] b) @ (filter tl)
    | (PI_fun (_,_,"lock",_)) :: 
      (PI_assign (_,_,arg1)) :: 
      (PI_assign (_,_,arg2)) :: 
      (PI_assign (_,_,arg3)) :: 
      (PI_assign (_,_,arg4)) :: 
      (PI_fun (a,b,"call",c)) :: 
      (PI_assign (_,r1,_)) :: 
      (PI_fun (_,_,"unlock",_)) :: tl ->
      (c_emit_fun_call envl local indent [arg1;arg2;arg3;arg4] [r1] b) @ (filter tl)
    | (PI_fun (_,_,"lock",_)) :: 
      (PI_assign (_,_,arg1)) :: 
      (PI_assign (_,_,arg2)) :: 
      (PI_assign (_,_,arg3)) :: 
      (PI_assign (_,_,arg4)) :: 
      (PI_assign (_,_,arg5)) :: 
      (PI_fun (a,b,"call",c)) :: 
      (PI_assign (_,r1,_)) :: 
      (PI_fun (_,_,"unlock",_)) :: tl ->
      (c_emit_fun_call envl local indent [arg1;arg2;arg3;arg4;arg5] [r1] b) @ (filter tl)
    | (PI_fun (_,_,"lock",_)) :: 
      (PI_fun (a,b,"call",c)) :: 
      (PI_assign (_,r1,_)) :: 
      (PI_assign (_,r2,_)) :: 
      (PI_fun (_,_,"unlock",_)) :: tl ->
      (c_emit_fun_call envl local indent [] [r1;r2] b) @ (filter tl)
    | (PI_fun (_,_,"lock",_)) :: 
      (PI_assign (_,_,arg1)) :: 
      (PI_fun (a,b,"call",c)) :: 
      (PI_assign (_,r1,_)) :: 
      (PI_assign (_,r2,_)) :: 
      (PI_fun (_,_,"unlock",_)) :: tl ->
      (c_emit_fun_call envl local indent [arg1] [r1;r2] b) @ (filter tl)
    | (PI_fun (_,_,"lock",_)) :: 
      (PI_assign (_,_,arg1)) :: 
      (PI_assign (_,_,arg2)) :: 
      (PI_fun (a,b,"call",c)) :: 
      (PI_assign (_,r1,_)) :: 
      (PI_assign (_,r2,_)) :: 
      (PI_fun (_,_,"unlock",_)) :: tl ->
      (c_emit_fun_call envl local indent [arg1;arg2] [r1;r2] b) @ (filter tl)
    | (PI_fun (_,_,"lock",_)) :: 
      (PI_assign (_,_,arg1)) :: 
      (PI_assign (_,_,arg2)) :: 
      (PI_assign (_,_,arg3)) :: 
      (PI_fun (a,b,"call",c)) :: 
      (PI_assign (_,r1,_)) :: 
      (PI_assign (_,r2,_)) :: 
      (PI_fun (_,_,"unlock",_)) :: tl ->
      (c_emit_fun_call envl local indent [arg1;arg2;arg3] [r1;r2] b) @ (filter tl)
    | pi :: tl  -> (c_instr envl local indent pi) @ (filter tl)
    | [] -> [] in

  match pi with
  | PI_assign (src,lhs,rhs) ->
  begin
    line src;
    c_expr_close := "";
    let et = c_expr_type envl lhs in
    let lhs',monitor = 
      match lhs with
      | PI_obj (opl,ot) -> 
        let obj = c_obj envl  et true ot opl in
        let mon =
          match ot with
          | OT_reg co | OT_signal co ->
            let desc = c_obj_desc envl co.co_name co.co_array co.co_struct in
            if desc.desc_export then
            begin
              if not desc.desc_array && not desc.desc_struct then 
                sprintf " X(\"%s\",(int)%s);" 
                  (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name)
                  (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name)
              else if desc.desc_array && not desc.desc_struct then
                sprintf " Xn(\"%s\",%s,(int)%s%s);" 
                  (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name)
                  (index desc.desc_num_sel)
                  (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name)
                  (index desc.desc_num_sel)
              else ""
            end
            else ""
          | OT_array at ->
          begin
            match at.at_objs.(0) with
            | OT_reg _  | OT_signal _ ->
              let desc = c_obj_desc envl at.at_name [at] [] in
              let at_sel = 
                if is_sel opl then
                  (index (Array.to_list (obj_sel opl)))
                else if is_sel_obj opl then
                  sprintf "%s" (c_expr envl  false (DT_int 0) (obj_sel_obj opl))
                else "?"  in
              if desc.desc_export then
                 sprintf " Xn(\"%s\",%s,(int)%s[%s]);" 
                    (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name)
                    at_sel
                    (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name)
                    at_sel 
              else ""
            | _ -> ""
          end;
          | _ -> "" in
        obj,mon
      | _ -> err "Unexpected LHS object found." in
   [ibox indent (sprintf "%s %s%s;%s" 
            lhs'  (c_expr envl  false et rhs) !c_expr_close
            monitor
        );]
  end;
  | PI_block (il,bf) -> 
    filter il
  | PI_list il -> List.flatten (List.map (c_instr envl  local indent) il)
  | PI_forloop (src,expr,dir,lim1,lim2,block) ->
    let et = c_expr_type envl expr in
    
    let aux = sprintf "%s %s;" (c_type (C_type et)) (c_expr envl  false (DT_int 0) expr) in
    if not (List.mem aux !c_defs) then c_defs := !c_defs @ [aux];
     
    line src;
    [
      ibox indent (sprintf "for(%s %s;%s <= %s;%s %s%s%s){"
              (c_expr envl  true (DT_int 0) expr)
              (c_expr envl  false (DT_int 0) lim1)
              (c_expr envl  false (DT_int 0) expr)
              (c_expr envl  false (DT_int 0) lim2)
              (c_expr envl  true (DT_int 0) expr)
              (c_expr envl  false (DT_int 0) expr)
              (if dir = 'd' then "-" else "+")
              ("1")
              )
    ]@(c_instr envl  local (indent+2)block)@
    [
      ibox indent "};"
    ]
  | PI_loop (src,Cp_syntax.Loop,expr,block)
  | PI_loop (src,Cp_syntax.Loop_while,expr,block) ->
    line src;
    let expr = 
      match expr with
      | PI_nop -> "true"
      | _ -> c_expr envl  false DT_bool expr in
    [
      ibox indent (sprintf "while(%s){" expr)
    ]@(c_instr envl  local (indent+2)block)@
    [
      ibox indent "};"
    ]
  | PI_branch (src,expr,b1,b2) ->
    line src;
    let expr = c_expr envl  false DT_bool expr in
    [
      ibox indent (sprintf "if (%s)" expr);
      ibox indent "{";
    ]@(c_instr envl  local (indent+2) b1)@
    (match b2 with
     | PI_nop | PI_block ([PI_nop],_) -> [ibox indent "};"]
     | _ ->
     begin
      [
        ibox indent "}";
        ibox indent "else";
        ibox indent "{";
      ]@(c_instr envl  local (indent+2) b2)@
      [
        ibox indent "};"
      ]
     end)
  | PI_select (src,expr,casel) ->
    line src;
    let et = c_expr_type envl expr in
    c_et := et :: !c_et;
    let expr = c_expr envl  false (List.hd !c_et) expr in
    let complete =
      match casel with
      | PI_list cl | PI_block (cl,_) ->
        let rec iter el =
          match el with
          | (PI_case (_,[],_)) :: tl -> true;
          | (PI_block ([(PI_case (_,[],_))],_)) :: tl -> true;
          | _ :: tl -> iter tl
          | [] -> false in
        iter cl
      | _ -> true in
    let l = [
      ibox indent (sprintf "switch (%s) {" expr);
    ]@(c_instr envl  local (indent+2) casel)@
    (if not complete then [ibox (indent+2) "default: printf(\"#%s:%d\\n\",__FILE__,__LINE__);conpro_err(\"unmatched value\");"] else [])@
    [
      ibox indent "};"
    ] in
    c_et := List.tl !c_et;
    l
  | PI_case (src,exprl,b) ->
    line src;
    let exprl = 
      if not !c_exc_map && exprl <> [] then
        List.map (c_expr envl  false (List.hd !c_et)) exprl
      else if exprl <> [] then
        List.map (fun s ->
          let id = int_of_string s in
          (c_get_exc envl id).exc_name
          ) (List.map (c_expr envl  local (List.hd !c_et)) exprl)
      else ["default"];
       in
    [
      ibox indent (sprintf "%s:" (if exprl <> ["default"] then (cargs exprl) else "default"));
      ibox indent "{";
    ]@(c_instr envl  local (indent+2) b)@
    [
      ibox (indent+2) "break;";
      ibox indent "};"
    ] 
    
  | PI_try (b,casel) ->
    let b' = c_instr envl  local (indent+2) b in
    let exc = !c_exc_env in
    incr c_exc_env;
    
    let complete =
      match casel with
      | PI_list cl | PI_block (cl,_) ->
        let rec iter el =
          match el with
          | (PI_case (_,[],_)) :: tl -> true;
          | _ :: tl -> iter tl
          | [] -> false in
        iter cl
      | _ -> true in
    let casel' = 
      c_exc_map := true; 
      c_et := (DT_int 0):: !c_et;
      let l = c_instr envl  local indent casel in
      c_exc_map := false; 
      c_et := List.tl !c_et;
      l in
    let l = [
      ibox indent (sprintf "try(%d)" exc);
      ibox indent "{";
    ]@b'@
    [
      ibox indent "};";
      ibox indent (sprintf "with(%d)" exc);
      ibox indent "{";
    ]@casel'@
    [
      ibox indent "};";
      ibox indent (sprintf "with_end(%d);" exc);
    ] in
    l
  | PI_raise exn ->
  begin
    [
      ibox indent (sprintf "raise(%s);" (c_get_exc envl exn).exc_name );
    ]
  end;
  | PI_waitfor (src,expr,n,tu,ef,et) ->
    line src;
    if n > 0 then
      [
        sprintf "process_delay(%d);" (
          match tu with
          | Nsec -> max 1 (n/1000)
          | Usec -> n 
          | Msec -> n*1000
          | Sec -> n*1000000
          | Cycles -> n
          );
      ]
    else
      [(
        let expr = 
          match expr with
          | PI_nop -> "true"
          | _ -> c_expr envl  false DT_bool expr in
        ibox indent (sprintf "while(!(%s)){};" expr)
      )] 
  | PI_fun (src,(opl,ot),sel,argl) ->
  begin
    line src;
    let ao_instr ao inline name at_sel =
      let _,descl = List.find (fun (sel',_) -> sel=sel') ao.ao_type.ta_rules.rl_methods in
      let argl' = List.map2 (fun arg desc -> arg,desc) argl descl in
      let argl_lhs = List.filter (fun (arg,desc) -> (desc.arg_type = Arg_lhs) || (desc.arg_type = Arg_lrhs)) argl' in
      let argl_rhs = List.filter (fun (arg,desc) -> desc.arg_type = Arg_rhs) argl' in 
      let argl_lhs' = List.map (fun (arg,desc) ->
        let et = desc.arg_data_type in
        sprintf "&%s" (c_expr envl  false et arg)
        ) argl_lhs in    
      let argl_rhs' = List.map (fun (arg,desc) ->
        let et = desc.arg_data_type in
        c_expr envl  false et arg
        ) argl_rhs in  
      
      List.map (ibox indent) ([
        if not inline then
          sprintf "%s->%s(%s%s%s);" 
            (if at_sel = "" then name else sprintf "%s%s" name at_sel) 
            sel
            (if at_sel = "" then name else sprintf "%s%s" name at_sel) 
            (if argl_rhs' <> [] then sprintf ",%s" (args argl_rhs') else "")
            (if argl_lhs' <> [] then sprintf ",%s" (args argl_lhs') else "")
        else
          sprintf "%s(%s%s);" 
            (if at_sel = "" then name else sprintf "%s%s" name at_sel) 
            (if argl_rhs' <> [] then sprintf "%s" (args argl_rhs') else "")
            (if argl_lhs' <> [] then sprintf "%s%s" (if argl_rhs' <> [] then "," else "")
                                                    (args argl_lhs') else "")
        ]) in
    let ao_core ao name dt size at_sel =
      let _,descl = List.find (fun (sel',_) -> sel=sel') ao.ao_type.ta_rules.rl_methods in
      let argl' = List.map2 (fun arg desc -> arg,desc) argl descl in
      let argl_lhs = List.filter (fun (arg,desc) -> (desc.arg_type = Arg_lhs) || (desc.arg_type = Arg_lrhs)) argl' in
      let argl_rhs = List.filter (fun (arg,desc) -> desc.arg_type = Arg_rhs) argl' in 
      let argl_lhs' = List.map (fun (arg,desc) ->
        sprintf "&%s" (c_expr envl  false dt arg)
        ) argl_lhs in    
      let argl_rhs' = List.map (fun (arg,desc) ->
        c_expr envl  false dt arg
        ) argl_rhs in  
      match sel with
      | "copy" ->
        List.map (ibox indent) [
          sprintf "{";
          sprintf "  %s %s_const[%d]=%s;" 
            (c_type (C_type dt)) 
            (if at_sel = "" then name else sprintf "%s%s" name at_sel) 
            size
            (if argl_rhs' <> [] then sprintf "%s" (args argl_rhs') else "");
          sprintf "  memcpy(%s,%s_const,%d);"
            (if at_sel = "" then name else sprintf "%s%s" name at_sel) 
            (if at_sel = "" then name else sprintf "%s%s" name at_sel) 
            size;
          sprintf "};";
          ]
      | "unlock" ->
        List.map (ibox indent) [
          sprintf "%s->%s(%s);" 
            (if at_sel = "" then name else sprintf "%s%s" name at_sel) 
            sel
            (if at_sel = "" then name else sprintf "%s%s" name at_sel) 
          ];
      | _ -> ["?"] in
      
    match ot with
    | OT_object ao when (ao.ao_type.ta_name = "function") -> 
      let name = Str.global_replace (Str.regexp "function_\(.+\)") "\1" ao.ao_name in
      if env_search envl name then
      begin
        match env_get envl name with
        | C_fun f-> 
          if f.fn_inline then ao_instr ao true name ""
          else ao_instr ao false name "";
        | _ -> ao_instr ao false name "";      
      end
      else
        ao_instr ao false name ""
    | OT_object ao when (ao.ao_type.ta_name = "Core") -> 
    begin
      let name = Str.global_replace (Str.regexp "function_\(.+\)") "\1" ao.ao_name in
      let dt,size = 
        match ao.ao_array with
        | [at] -> (get_some (dt_of_ot at.at_objs.(0))),proda at.at_dim;
        | _ -> DT_int 0,0 in
      ao_core ao name dt size ""
    end;
    | OT_object ao when (sel = "interface") -> 
      []
    | OT_object ao ->
      ao_instr ao false ao.ao_name ""
    | OT_array at ->
    begin
      let at_sel = 
        if is_sel opl then
          (index (Array.to_list (obj_sel opl)))
        else if is_sel_obj opl then
          sprintf "[%s]" (c_expr envl  false (DT_int 0) (obj_sel_obj opl))
        else "?"  in
      
      match at.at_objs.(0) with
      | OT_object ao when (ao.ao_type.ta_name = "function") ->
        let name = Str.global_replace (Str.regexp "function_\(.+\)") "\1" at.at_name in
        ao_instr (match at.at_objs.(0) with 
             | OT_object ao -> ao
             | OT_queue qu -> qu.qu_ao
             | _ -> err (sprintf "Invalid abstract object array <%s>" at.at_name))
             false name at_sel
      | OT_object ao when (ao.ao_type.ta_name = "Core") ->
        let name = at.at_name in
        let dt = get_some (dt_of_ot at.at_objs.(0))  in
        let size = proda at.at_dim in
        ao_core (match at.at_objs.(0) with 
             | OT_object ao -> ao
             | OT_queue qu -> qu.qu_ao
             | _ -> err (sprintf "Invalid abstract object array <%s>" at.at_name))
             name dt size at_sel
      | OT_object ao when (sel = "interface") -> 
        []
      | _ -> 
        let name = at.at_name in
        ao_instr (match at.at_objs.(0) with 
             | OT_object ao -> ao
             | OT_queue qu -> qu.qu_ao
             | _ -> err (sprintf "Invalid abstract object array <%s>" at.at_name))
             false name at_sel
    end;
    | _ -> ["?#?;"];
  end;  
  | _ -> []
  
and c_emit_fun_call envl local indent args rets func =
  let rec fargs il =
    match il with
    | hd :: tl -> (hd)^(if tl <> [] then "," else "")^(fargs tl)
    | [] -> "" in
  let opl,ot = func in
  let fname = Str.global_replace (Str.regexp "FUN_\(.+\)") "\1" (name_of_ot ot) in
  let f = 
    match env_get envl fname with
    | C_fun f-> f
    | _ -> error 0 ("c_emit_fun_call: function environment name expected") in
  let args = fargs (List.map2 (fun arg (_,co) ->
    let et = co.co_type in
    c_expr envl false et arg
    ) args f.fn_args) in
  let rets = fargs (List.map2 (fun arg (_,co) ->
    let et = co.co_type in
    sprintf "&%s" (c_expr envl false et arg)
    ) rets f.fn_rets) in
  [ibox indent (sprintf "%s(%s%s%s);" fname args (if rets <> "" then "," else "") rets)]
  
and c_emit_instr envl  local pil =
  let instrl = c_instr envl local 0 (PI_block(pil,Cp_block_frame.nilbf)) in
  List.iter out !c_defs;
  List.iter out !c_evals;
  List.iter out instrl

let c_emit_models env = 
  List.iter (fun c_mod ->
    let code = ref [] in
    let fname = 
      match c_mod with
      | "Core" -> "conpro.c"
      | _ -> sprintf "%s.c" (of_mod c_mod) in
    let pathl = ""::(compiler.t_lib@compiler.t_incl) in        
    let rec f_open pathl =
      match pathl with
      | path :: tl ->
      begin
        try open_in (sprintf "%s%s%s" path (if path <> "" then "/" else "") fname)
        with _ -> f_open tl
      end;
      | [] -> err (sprintf "Can't open file <%s>." fname) in
    let ic = f_open pathl in
    let more = ref true in
    while !more 
    do
      more := protects (
        let line = input_line ic in  
        let check_inc = Str.string_match (Str.regexp "#include[ ]*\"") line 0 in
        if not check_inc then
          code := !code @ [line]);    
    done;
    close_in ic;
    List.iter out !code;
    ) (List.sort (fun a b -> if a = "Core" && b = "process" then 0
                  else if a = "Core"  || a = "process" then -1
                  else 1) !c_mod_dep)
    
let c_emit_models_int env = 
  List.iter (fun c_mod ->
    let code = ref [] in
    let fname = 
      match c_mod with
      | "Core" -> "conpro.h"
      | _ -> sprintf "%s.h" (of_mod c_mod) in
    let pathl = ""::(compiler.t_lib@compiler.t_incl) in        
    let rec f_open pathl =
      match pathl with
      | path :: tl ->
      begin
        try open_in (sprintf "%s%s%s" path (if path <> "" then "/" else "") fname)
        with _ -> f_open tl
      end;
      | [] -> err (sprintf "Can't open file <%s>." fname) in
    let ic = f_open pathl in
    let more = ref true in
    while !more 
    do
      more := protects (
        let line = input_line ic in  
        let check_inc = Str.string_match (Str.regexp "#include[ ]*\"") line 0 in
        if not check_inc then
          code := !code @ [line]);    
    done;
    close_in ic;
  
    List.iter out !code;
    ) (List.sort (fun a b -> if a = "Core" && b = "process" then 0
                  else if a = "Core"  || a = "process" then -1
                  else 1) !c_mod_dep)
    



(*
** Transform and emit a module
*)
let c_emit modu =
  let main_defs = ref [] in
  let main_init = ref [] in
  c_mod_dep := [];
  
  let rec args il =
    match il with
    | hd :: tl -> (hd)^(if tl <> [] then " " else "")^(args tl)
    | [] -> "" in
  let rec targs il =
    match il with
    | hd :: tl -> (hd)^(if tl <> [] then ", " else "")^(targs tl)
    | [] -> "" in
  let rec largs il =
    match il with
    | hd :: tl -> (hd)^(if tl <> [] then "; " else "")^(largs tl)
    | [] -> "" in
  let rec fiargs il =
    match il with
    | hd :: tl -> (hd)^(if tl <> [] then " -> " else "")^(fiargs tl)
    | [] -> "" in
  let rec ftargs il =
    match il with
    | hd :: tl -> (hd)^(if tl <> [] then " * " else "")^(ftargs tl)
    | [] -> "" in

  let env_merge env1 env2 =
    let env1 = Hashtbl.copy env1 in
    Hashtbl.iter (fun k s ->
      Hashtbl.add env1 k s ) env2;
    env1 in
      
  let create fname =
   (try
     begin
      oc := open_out (sprintf "%s%s.c" compiler.t_output fname);
      out_ (sprintf "Creating <%s.c>..." fname);
     end
    with
    _ -> err (sprintf "Can't open file <%s%s.c>." compiler.t_output fname)) in
    
  let finish fname =
    close_out !oc in

  let create_make fname =
   (try
     begin
      oc := open_out (sprintf "%s%s" compiler.t_output fname);
      out_ (sprintf "Creating <%s>..." fname);
     end
    with
    _ -> err (sprintf "Can't open file <%s%s>." compiler.t_output fname)) in
    
  let finish_make fname =
    close_out !oc in

  let create_int fname =
   (try
     begin
      oc := open_out (sprintf "%s%s.h" compiler.t_output fname);
      out (sprintf "#ifndef __%s" fname);
      out (sprintf "#define __%s" fname);
      out_ (sprintf "Creating <%s.h>..." fname);
     end
    with
    _ -> err (sprintf "Can't open file <%s%s.mli>." compiler.t_output fname)) in
    
  let finish_int fname =
    out (sprintf "#endif /* !__%s */" fname);
    close_out !oc in
    
  out_ (sprintf "Synthesis of C model for module <%s>..." modu.mod_name);
  Cp_common.ind_incr ();
  
  let level = C_level_high in

  let glob_env = c_env [] modu modu.mod_objs in
  let defs,evals = c_emit_objs glob_env  false in
  main_defs := !main_defs @ defs;
  main_init := !main_init @ evals;

  (*
  ** Process interface
  *)
  List.iter (fun pro ->
    let env = c_env [glob_env] modu pro.pro_objs in
    let is_fun,fun_name,fun_def = 
      if Str.string_match (Str.regexp "FUN_.+") pro.pro_name 0 then
      begin
        let fname = Str.global_replace (Str.regexp "FUN_\(.+\)") "\1" pro.pro_name in
        if sym_check_fun modu.mod_objs fname then
          true,fname,Some (sym_get_fun modu.mod_objs fname)
        else false,"",None
      end
      else
        false,"",None in
    let is_array,aname,i,size =
      if sym_check_obj modu.mod_objs pro.pro_name then
        false,"",(-1),0
      else 
      begin
        let name = Str.global_replace (Str.regexp "\(.+\)_[0-9]+") "\1" pro.pro_name in
        match sym_get_obj modu.mod_objs name with
        | OT_array at -> 
          true,
          name,
          int_of_string (Str.global_replace (Str.regexp ".+_\([0-9]+\)") "\1" pro.pro_name),
          proda at.at_dim
        | OT_object ao -> false,"",(-1),0
        | _ -> false,"",(-1),0
      end in
    let fname = 
      if not is_array then sprintf "%s_%s" (of_mod modu.mod_name) pro.pro_name 
      else sprintf "%s_%s" (of_mod modu.mod_name) aname in
    if not is_array || (is_array && i = 0) then 
    begin
      create_int fname;  
      if not (List.mem "Core" !c_mod_dep) then c_mod_dep := !c_mod_dep @ ["Core"];
      if not (List.mem "process" !c_mod_dep) then c_mod_dep := !c_mod_dep @ ["process"];
      if not (List.mem "event" !c_mod_dep) then c_mod_dep := !c_mod_dep @ ["event"];
      if is_fun && not (List.mem "mutex" !c_mod_dep) then c_mod_dep := !c_mod_dep @ ["mutex"];
      out (sprintf "#include \"%s_types.h\"" (of_mod modu.mod_name));
      out (sprintf "#include \"%s_const.h\"" (of_mod modu.mod_name));
      out (sprintf "#include \"%s_models.h\"" (of_mod modu.mod_name));
      out (sprintf "#include \"%s_objs.h\"" (of_mod modu.mod_name));
      
      let deps = c_dep_instr env false pro.pro_instr in
      let deps' = ref [] in
      List.iter (fun dep ->
        if not (List.mem dep !deps') then
          out (sprintf "#include \"%s_%s.h\"" (of_mod modu.mod_name) dep);
        deps' := dep :: !deps';
        ) deps;
    

      let fargs =
        if is_fun  then
        begin
          let f = get_some fun_def in
          sprintf "%s%s" 
            (if f.fun_args_obj = [] then ""
             else sprintf "%s" (targs (List.map2 (fun a co -> 
                  sprintf "%s %s" (match co.co_type with
                  | DT_int n | DT_logic n -> if n < 31 then "int" else "int64"
                  | DT_char -> "char"
                  | DT_bool -> "bool"
                  | _ -> "?") a) f.fun_args f.fun_args_obj)))
            (if f.fun_ret_obj = [] then ""
             else sprintf "%s%s" 
              (if f.fun_args_obj = [] then "" else ",")
              (targs (List.map2 (fun r co -> 
                  sprintf "%s* %s" (match co.co_type with
                  | DT_int n | DT_logic n -> if n < 31 then "int" else "int64"
                  | DT_char -> "char"
                  | DT_bool -> "bool"
                  | _ -> "?") r ) f.fun_ret f.fun_ret_obj)));
        end
        else "" in
      if not is_array then
      begin
        if is_fun  then
          out (sprintf "extern void %s(%s);"  (fix_name fun_name) fargs)   
        else
        begin
          out (sprintf "extern void PRO_%s();"   (fix_name pro.pro_name));   
          out (sprintf "extern process_t* %s;"   (fix_name pro.pro_name));   
        end;
      end
      else
      begin
        if is_fun  then
          out (sprintf "extern void %s(%s);"  (fix_name fun_name) fargs)   
        else
        begin
          out (sprintf "extern void PRO_%s(int process_id);"   (fix_name aname));   
          out (sprintf "extern process_t* %s[];"   (fix_name aname));   
        end;
    end; 
      finish_int fname;
    end;
    ) modu.mod_procs;
  let prol = ref [] in


  (*
  ** Process implementation
  *)
  List.iter (fun pro ->
    let env = c_env [glob_env] modu pro.pro_objs in
    
    let is_fun,is_inline,fun_name,fun_def = 
      if Str.string_match (Str.regexp "FUN_.+") pro.pro_name 0 then
      begin
        let fname = Str.global_replace (Str.regexp "FUN_\(.+\)") "\1" pro.pro_name in
        if sym_check_fun modu.mod_objs fname then
        begin
          let f = sym_get_fun modu.mod_objs fname in
          true,f.fun_inline,fname,Some f
        end
        else false,false,"",None
      end
      else
        false,false,"",None in
      
    let is_array,aname,i,size =
      if sym_check_obj modu.mod_objs pro.pro_name then
        false,"",(-1),0
      else 
      begin
        let name = Str.global_replace (Str.regexp "\(.+\)_[0-9]+") "\1" pro.pro_name in
        match sym_get_obj modu.mod_objs name with
        | OT_array at -> 
          true,
          name,
          int_of_string (Str.global_replace (Str.regexp ".+_\([0-9]+\)") "\1" pro.pro_name),
          proda at.at_dim
        | OT_object ao -> false,"",(-1),0
        | _ -> false,"",(-1),0
      end in
    let fname = 
      if not is_array then sprintf "%s_%s" (of_mod modu.mod_name) pro.pro_name 
      else sprintf "%s_%s" (of_mod modu.mod_name) aname in
    let pname = 
      if not is_array then pro.pro_name 
      else  aname in
    if not is_array || (is_array && i = 0) then 
    begin
      create fname;  
      out (sprintf "#include \"%s.h\"" fname);
      c_emit_types env;
    end;
    if not is_array then prol := !prol @ [pname];
    if is_array && i = 0 then prol := !prol @ [pname];
    let fargs =
      if is_fun  then
      begin
        let f = get_some fun_def in
        sprintf "%s%s" 
          (if f.fun_args_obj = [] then ""
           else sprintf "%s" (targs (List.map2 (fun a co -> 
                sprintf "%s %s" (match co.co_type with
                | DT_int n | DT_logic n -> if n < 31 then "int" else "int64"
                | DT_char -> "char"
                | DT_bool -> "bool"
                | _ -> "?") a) f.fun_args f.fun_args_obj)))
          (if f.fun_ret_obj = [] then ""
           else sprintf "%s%s" 
              (if f.fun_args_obj = [] then "" else ",")
              (targs (List.map2 (fun r co -> 
                sprintf "%s* %s" (match co.co_type with
                | DT_int n | DT_logic n -> if n < 31 then "int" else "int64"
                | DT_char -> "char"
                | DT_bool -> "bool"
                | _ -> "?") r ) f.fun_ret f.fun_ret_obj)));
      end
      else "" in
    
    if is_fun then
    begin
      let f = get_some fun_def in
      let s = sprintf "void %s(%s)" (fix_name fun_name) fargs in
      out s;
    end
    else if not is_array then
      out (sprintf "void PRO_%s()" (fix_name pname))
    else if is_array && i = 0 then
      out (sprintf "void PRO_%s(int process_id)" pname);
      
    if not is_array || i = 0 then
    begin
      out "{";
      ind_incr ();
      let defs,evals = c_emit_objs env  true in
      c_defs := defs;
      c_evals := evals;
      if is_fun && not is_inline then c_evals := !c_evals @ [sprintf "LOCK_FUN_%s->lock(LOCK_FUN_%s);" fun_name fun_name];
      c_emit_instr [env;glob_env]  true pro.pro_instr;    
      if not is_fun then out "process_end();";
      if is_fun && not is_inline then out (sprintf "LOCK_FUN_%s->unlock(LOCK_FUN_%s);" fun_name fun_name);
      ind_decr ();
      out "}";
    end;
    if not is_fun && not is_array then
    begin
      out (sprintf "process_t* %s;" (fix_name pname));   
      main_init := !main_init @ [
        sprintf "%s=process_new(\"%s\",PRO_%s,0);" (fix_name pname) (fix_name pname) (fix_name pname);
      ];
    end;
    if is_array then
    begin
      main_init := !main_init @ [
        sprintf "%s[%d]=process_new(\"%s\",PRO_%s,%d);" (fix_name pname) i (fix_name pro.pro_name) (fix_name pname) i;
      ];      
    end;
    if not is_array  then  finish fname;
    if is_array && i = size-1 then
    begin
      out (sprintf "process_t *%s[%d];" (fix_name aname) size);
      finish fname;
    end;
    ) modu.mod_procs;
  let fname = sprintf "%s" (of_mod modu.mod_name) in
  create fname;  
  if not (List.mem "Core" !c_mod_dep) then c_mod_dep := !c_mod_dep @ ["Core"];
  if not (List.mem "process" !c_mod_dep) then c_mod_dep := !c_mod_dep @ ["process"];
  if not (List.mem "event" !c_mod_dep) then c_mod_dep := !c_mod_dep @ ["event"];
  out (sprintf "#include \"%s.h\"" (of_mod modu.mod_name));
  List.iter (fun pro ->
    out (sprintf "#include \"%s_%s.h\"" (of_mod modu.mod_name) pro);
    ) !prol;
  out "event_t *_exit;";  
  out "int conpro(){";
  ind_incr ();
  out "process_init();";
  out "_exit=event_new(\"\");";
  out "_exit->init(_exit);";
  List.iter (fun init ->
    out init;
    ) !main_init;
  (*
  ** Emit toplevel instructions (ADTO calls)
  *)
  c_defs := []; c_evals := [];
  c_emit_instr [glob_env]  false modu.mod_instr;  
  
  out "Main->call(Main);";  
  ind_decr ();
  out "}";
  finish fname;

  create_int fname;
  out (sprintf "#include \"%s_types.h\"" (of_mod modu.mod_name));
  out (sprintf "#include \"%s_const.h\"" (of_mod modu.mod_name));
  out (sprintf "#include \"%s_models.h\"" (of_mod modu.mod_name));
  out (sprintf "#include \"%s_objs.h\"" (of_mod modu.mod_name));
  out "extern int conpro();";
  out "extern event_t *_exit;";  
  finish_int fname;

  (*
  ** Objects & Types
  *)
  let fname = sprintf "%s_types" (of_mod modu.mod_name) in
  create_int fname;  
  c_emit_types glob_env;
  finish_int fname;
  let fname = sprintf "%s_models" (of_mod modu.mod_name) in
  create fname;  
  out (sprintf "#include \"%s_models.h\"" (of_mod modu.mod_name));
  c_emit_models glob_env;
  finish fname;
  create_int fname;  
  out (sprintf "#include \"%s_const.h\"" (of_mod modu.mod_name));
  out (sprintf "#include \"%s_types.h\"" (of_mod modu.mod_name));
  c_emit_models_int glob_env;
  finish_int fname;
  let fname = sprintf "%s_const" (of_mod modu.mod_name) in
  create fname;  
  out (sprintf "#include \"%s_const.h\"" (of_mod modu.mod_name));
  c_emit_const glob_env  false;
  finish fname;
  create_int fname;  
  c_emit_const_int glob_env  false;
  finish_int fname;
  let fname = sprintf "%s_objs" (of_mod modu.mod_name) in
  create fname;  
  out (sprintf "#include \"%s_objs.h\"" (of_mod modu.mod_name));
  List.iter out !main_defs;
  finish fname;
  create_int fname;  
  out (sprintf "#include \"%s_types.h\"" (of_mod modu.mod_name));
  out (sprintf "#include \"%s_const.h\"" (of_mod modu.mod_name));
  out (sprintf "#include \"%s_models.h\"" (of_mod modu.mod_name));
  c_emit_objs_int glob_env  false;
  finish_int fname;
  
  
  (*
  ** Makefile: 
  *)
  out_ (sprintf "Creating makefile <makefile>..."); 
  let fname = "makefile" in
  create_make fname;
  let m = of_mod modu.mod_name in
  List.iter out ([
    sprintf "PROJ=%s" m;
    sprintf "CC=cc";
    sprintf "INCL=-I. -I/opt/Conpro2/lib";
    sprintf "%%.o: %%.c";
    sprintf "\t$(CC) -c $(INCL) $*.c";
    sprintf "help:";
    sprintf "\t@echo \"Targets: [core impl clean all]\"";

    sprintf "clean:";
    sprintf "\trm -f *.o *.a";
    sprintf "core: %s_const.o %s_models.o %s_objs.o" m m m;
    sprintf "impl: \\" ;    
    ]@(let i = ref (List.length modu.mod_procs) in
       List.filter (fun line -> line <> "") (List.map (fun pro ->
          let is_array,aname,n,size =
            if sym_check_obj modu.mod_objs pro.pro_name then
              false,"",(-1),0
            else 
            begin
              let name = Str.global_replace (Str.regexp "\(.+\)_[0-9]+") "\1" pro.pro_name in
              match sym_get_obj modu.mod_objs name with
              | OT_array at -> 
                true,
                name,
                int_of_string (Str.global_replace (Str.regexp ".+_\([0-9]+\)") "\1" pro.pro_name),
                proda at.at_dim
              | OT_object ao -> false,"",(-1),0
              | _ -> false,"",(-1),0
            end in
          decr i;
          let fname = 
            if not is_array then
              sprintf "%s_%s.o" (of_mod modu.mod_name) pro.pro_name
            else
              sprintf "%s_%s.o" (of_mod modu.mod_name) aname in          
          if not is_array || (is_array && n = 0) then
            sprintf "\t%s \\" fname 
          else 
            "";
      ) modu.mod_procs))@
    [
      sprintf "\t%s.o" (of_mod modu.mod_name);
    ]@      
    [
      sprintf "lib%s.a: core impl" (of_mod modu.mod_name); 
      sprintf "\tar rcv lib%s.a \\"  (of_mod modu.mod_name);
      sprintf "\t\t%s_const.o \\" (of_mod modu.mod_name);
      sprintf "\t\t%s_models.o \\" (of_mod modu.mod_name);
      sprintf "\t\t%s_objs.o \\" (of_mod modu.mod_name);
      
    ]@(let i = ref (List.length modu.mod_procs) in
       List.filter (fun line -> line <> "") (List.map (fun pro ->
          let is_array,aname,n,size =
            if sym_check_obj modu.mod_objs pro.pro_name then
              false,"",(-1),0
            else 
            begin
              let name = Str.global_replace (Str.regexp "\(.+\)_[0-9]+") "\1" pro.pro_name in
              match sym_get_obj modu.mod_objs name with
              | OT_array at -> 
                true,
                name,
                int_of_string (Str.global_replace (Str.regexp ".+_\([0-9]+\)") "\1" pro.pro_name),
                proda at.at_dim
              | OT_object ao -> false,"",(-1),0
              | _ -> false,"",(-1),0
            end in
          decr i;
          let fname = 
            if not is_array then
              sprintf "%s_%s.o" (of_mod modu.mod_name) pro.pro_name
            else
              sprintf "%s_%s.o" (of_mod modu.mod_name) aname in          
          if not is_array || (is_array && n = 0) then
            sprintf "\t\t%s \\" fname 
          else 
            "";
      ) modu.mod_procs))@
    [
      sprintf "\t%s.o" (of_mod modu.mod_name);
      sprintf "all: lib%s.a" (of_mod modu.mod_name); 
      sprintf "%s: main.o lib%s.a" (of_mod modu.mod_name) (of_mod modu.mod_name); 
      sprintf "\t$(CC) -o %s main.c lib%s.a" (of_mod modu.mod_name) (of_mod modu.mod_name);
    ];    
      
    );
  finish_make fname;
  Cp_common.ind_decr ();
  ()
  
