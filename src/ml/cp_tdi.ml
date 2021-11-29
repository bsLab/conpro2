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
**      BSSLAB, Dr. Stefan Bosse http://www.bsslab.de
**
**      COPYRIGHT: THIS SOFTWARE, EXECUTABLE AND SOURCE CODE IS OWNED BY BSSLAB.
**                 THIS SOURCE CODE MAY NOT BE COPIED, EXTRACTED,
**                 MODIFIED, OR OTHERWISE USED IN A CONTEXT
**                 OUTSIDE OF THE SOFTWARE SYSTEM.
**
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2006-2009 BSSLAB
**    $CREATED:     17.10.2008
**    $VERSION:     2.22
**
**    $INFO:
**
**  Tool Description Interface
**
**    $ENDOFINFO
**
*)


open Cp_version
open Cp_common
open Cp_syntax
open Cp_symbol
open Cp_types
open Cp_module
open Printf
open Cp_tdi_types
open Cp_tdi_interp
open Cp_tdi_analysis
open Cp_resource
open Cp_utils

let vhdl_tools = ref []

(*
** Default settings
*)
let synth_tools = ref []

let open_synth_tools () =
  let tools = "tools" in
  let e str = error 0 (sprintf "TDI: synth_tools: %s" str) in  
  out (sprintf "TDI: Opening tools file <%s>..." tools);
  let old_pos = an.a_curpos in 
  an.a_curpos <- nilsrc ();  
  ind_incr ();
  let tops,lmap = Cp_tools_analysis.syntax_of_file tools in
  an.a_modu.mod_fmap <- an.a_modu.mod_fmap @ lmap;  
  let cur_pos p =
    if p <> {f_name="";f_cpos=0} then an.a_curpos <- source_of_pos p  in
    
  let version = ref "?" in
  let rec tsl tops = 
    match tops with
    | (TS_version (pos,str)) :: tl -> 
      version := str;
      tsl tl;
    | (TS_structs tsl) :: _ -> tsl;
    | _ :: tl -> tsl tl;
    | _ -> e "Unexpected tools file found (TS_structs not found)." in
  let tsl = tsl tops in
  
  let get_str name tsel =
    match (List.filter (fun ts ->
      match ts with
      | TS_param (str1,(pos,_)) -> 
        cur_pos pos;
        str1 = name
      | _ -> false) tsel) with
    | [TS_param (_,(pos,str))] ->
      cur_pos pos;
      str
    | _ -> e (sprintf "Unexpected tools file found: %s missing." name) in

  let get_int name tsel =
    match (List.filter (fun ts ->
      match ts with
      | TS_param (str1,(pos,_)) -> 
        cur_pos pos;
        str1 = name
      | _ -> false) tsel) with
    | [TS_param (_,(pos,str))] ->
    begin
      cur_pos pos;
      try
          int_of_string str
      with _ -> e "Unexpected tools file found (TS_param with invalid integer value)."
    end;
    | _ -> e (sprintf "Unexpected tools file found: %s missing." name) in

  let get_strl name tsel =
    match (List.filter (fun ts ->
      match ts with
      | TS_paraml (str1,_) -> 
        str1 = name
      | _ -> false) tsel) with
    | [TS_paraml (_,sl)] ->
      List.map (fun (p,s) ->
        cur_pos p;
        s
        ) sl;
    | _ -> e (sprintf "Unexpected tools file found: %s missing." name) in

  let get_strl2 name tsel =
    match (List.filter (fun ts ->
      match ts with
      | TS_paraml2 (str1,_) -> 
        str1 = name
      | _ -> false) tsel) with
    | [TS_paraml2 (_,sl)] ->
      List.map (fun ((p1,s1),(p2,s2)) ->
        cur_pos p1;
        s1,s2
        ) sl;
    | _ -> e (sprintf "Unexpected tools file found: %s missing." name) in

  let get_strl21 name tsel =
    match (List.filter (fun ts ->
      match ts with
      | TS_paraml21 (str1,_) -> 
        str1 = name
      | _ -> false) tsel) with
    | [TS_paraml21 (_,sl21)] ->
      List.map (fun ((p1,s1),sl) ->
        cur_pos p1;
        s1,(List.map (fun ((p1,s1),(p2,s2)) ->
              cur_pos p1;
              s1,s2
              ) sl);
        ) sl21;
    | _ -> [] in

  synth_tools :=   List.map (fun ts ->
    let st =
      match ts with
      | TS_struct tsel ->
      begin
        let tool = get_str "syn_tool" tsel in
        let st = {
          syn_tool = tool;
          syn_top = get_str "syn_top" tsel; 
          syn_ver = get_int "syn_ver" tsel; 
          syn_vhdl_map= get_strl2 "syn_vhdl_map" tsel;
          syn_vhdl_lib= get_strl "syn_vhdl_lib" tsel;
          syn_synth_set = get_strl21 "syn_synth_set" tsel;
          syn_tech_set = get_strl21 "syn_tech_set" tsel;
          } in
        let success = ref false in
        let paths = ref (""::(compiler.t_lib@compiler.t_incl@[sprintf "%s/toolset" conpro_path])) in
        while !paths <> [] && not !success
        do
          let ok = protects (
              let file = 
                  match !paths with
                  | ""::tl -> paths := tl; 
                              sprintf "%s.lib" tool;
                  | hd::tl -> paths := tl;
                              sprintf "%s/%s.lib" hd tool;
                  | [] -> sprintf "%s.lib" tool in
              let ic = open_in file in
              out (sprintf "Opening tool library file <%s>..." file);
              protect(while true do st.syn_vhdl_lib <- st.syn_vhdl_lib @ [input_line ic]; done);
              close_in ic;
              ) in
          if not ok && !paths = [] then
            error 0 (sprintf "Failed to read tool library file <%s.lib>. File not found." tool);
          success := ok;
        done;
        st
      end;
      | _ -> e "Unexpected tools file found. (TS_struct not found)" in
    info (sprintf "Added synthesis tool defintion <%s>, version %s." st.syn_tool !version);
    st
    ) tsl;
  ind_decr ();
  an.a_curpos <- old_pos
  
let open_generic () = 
  let tools = "tools" in
  let e str = error 0 (sprintf "TDI: synth_tools: %s" str) in  
  out (sprintf "TDI: Opening tools file <%s>..." tools);
  let old_pos = an.a_curpos in 
  an.a_curpos <- nilsrc ();  
  ind_incr ();
  let tops,lmap = Cp_tools_analysis.syntax_of_file tools in
  an.a_modu.mod_fmap <- an.a_modu.mod_fmap @ lmap;  
  let cur_pos p =
    if p <> {f_name="";f_cpos=0} then an.a_curpos <- source_of_pos p  in
    
  let version = ref "?" in
  let rec tsl tops = 
    match tops with
    | (TS_version (pos,str)) :: tl -> 
      version := str;
      tsl tl;
    | (TS_structs tsl) :: _ -> tsl;
    | _ :: tl -> tsl tl;
    | _ -> e "Unexpected tools file found (TS_structs not found)." in
  let tsl = tsl tops in
  
  let get_str name tsel =
    match (List.filter (fun ts ->
      match ts with
      | TS_param (str1,(pos,_)) -> 
        cur_pos pos;
        str1 = name
      | _ -> false) tsel) with
    | [TS_param (_,(pos,str))] ->
      cur_pos pos;
      str
    | _ -> e (sprintf "Unexpected tools file found: %s missing." name) in

  let get_int name tsel =
    match (List.filter (fun ts ->
      match ts with
      | TS_param (str1,(pos,_)) -> 
        cur_pos pos;
        str1 = name
      | _ -> false) tsel) with
    | [TS_param (_,(pos,str))] ->
    begin
      cur_pos pos;
      try
          int_of_string str
      with _ -> e "Unexpected tools file found (TS_param with invalid integer value)."
    end;
    | _ -> e (sprintf "Unexpected tools file found: %s missing." name) in

  let get_strl name tsel =
    match (List.filter (fun ts ->
      match ts with
      | TS_paraml (str1,_) -> 
        str1 = name
      | _ -> false) tsel) with
    | [TS_paraml (_,sl)] ->
      List.map (fun (p,s) ->
        cur_pos p;
        s
        ) sl;
    | _ -> e (sprintf "Unexpected tools file found: %s missing." name) in

  let get_strl2 name tsel =
    match (List.filter (fun ts ->
      match ts with
      | TS_paraml2 (str1,_) -> 
        str1 = name
      | _ -> false) tsel) with
    | [TS_paraml2 (_,sl)] ->
      List.map (fun ((p1,s1),(p2,s2)) ->
        cur_pos p1;
        s1,s2
        ) sl;
    | _ -> e (sprintf "Unexpected tools file found: %s missing." name) in

  let get_strl21 name tsel =
    match (List.filter (fun ts ->
      match ts with
      | TS_paraml21 (str1,_) -> 
        str1 = name
      | _ -> false) tsel) with
    | [TS_paraml21 (_,sl21)] ->
      List.map (fun ((p1,s1),sl) ->
        cur_pos p1;
        s1,(List.map (fun ((p1,s1),(p2,s2)) ->
              cur_pos p1;
              s1,s2
              ) sl);
        ) sl21;
    | _ -> [] in
  let tsl = List.filter (fun ts ->
    match ts with TS_struct tsel -> (get_str "syn_tool" tsel) = "generic") tsl in
  synth_tools := List.map (fun ts ->
    let st =
      match ts with
      | TS_struct tsel ->
      begin
        let tool = get_str "syn_tool" tsel in
        let st = {
          syn_tool = tool;
          syn_top = get_str "syn_top" tsel; 
          syn_ver = get_int "syn_ver" tsel; 
          syn_vhdl_map= get_strl2 "syn_vhdl_map" tsel;
          syn_vhdl_lib= get_strl "syn_vhdl_lib" tsel;
          syn_synth_set = get_strl21 "syn_synth_set" tsel;
          syn_tech_set = get_strl21 "syn_tech_set" tsel;
          } in
        let success = ref false in
        let paths = ref (""::(compiler.t_lib@compiler.t_incl@[sprintf "%s/toolset" conpro_path])) in
        while !paths <> [] && not !success
        do
          let ok = protects (
              let file = 
                  match !paths with
                  | ""::tl -> paths := tl; 
                              sprintf "%s.lib" tool;
                  | hd::tl -> paths := tl;
                              sprintf "%s/%s.lib" hd tool;
                  | [] -> sprintf "%s.lib" tool in
              let ic = open_in file in
              out (sprintf "Opening tool library file <%s>..." file);
              protect(while true do st.syn_vhdl_lib <- st.syn_vhdl_lib @ [input_line ic]; done);
              close_in ic;
              ) in
          if not ok && !paths = [] then
            error 0 (sprintf "Failed to read tool library file <%s.lib>. File not found." tool);
          success := ok;
        done;
        st
      end;
      | _ -> e "Unexpected tools file found. (TS_struct not found)" in
    info (sprintf "Added synthesis tool defintion <%s>, version %s." st.syn_tool !version);
    compiler.t_synth_tool <- st;
    st) tsl
      




let vhdl_src modu tool =    
  let src = ref [] in
  let src_vhdl = ref [] in
  let rec add_path pl file =
      let path,tl = 
          match pl with
          | hd::tl -> if hd <> "" then sprintf "%s%s%s" 
                                               (if Filename.is_relative hd then 
                                                  sprintf "%s/%s" (Sys.getcwd ()) hd else hd)
                                               (if hd.[(String.length hd)-1] <> '/' then "/" else "") 
                                               file,tl
                                  else "",tl;
          | [] -> file,[] in
      let ok = protects (
              let ic = open_in (path^".vhdl") in
              close_in ic) in
      if ok then path
      else if tl <> [] then add_path tl file 
      else error 0 (sprintf "%s: can't find VHDL file <%s.vhdl>" tool file) in
  let rec find_src modu =
      src := !src @ [
              (of_mod modu.mod_name);
          ] @ (
              List.map (fun pro -> 
                  sprintf "%s_%s" (of_mod modu.mod_name) pro.pro_name
              ) modu.mod_procs;
          ) @ (
              let syms = list_of_sym modu.mod_objs in
              let comps = 
                  List.filter (fun sym ->
                      match sym with
                      | Sym_obj (OT_component st) -> 
                        not (sym_check_obj modu.mod_export st.st_name) &&
                        true; 
                      | _ -> false;
                  ) syms in
              List.map (fun sym ->
                      match sym with
                      | Sym_obj (OT_component st) -> 
                          let vhdl = add_path (compiler.t_incl@[compiler.t_output])
                                              st.st_type.ts_name in
                          src_vhdl := !src_vhdl @ [vhdl];
                          vhdl;
                      | _ -> error 88948 "";
                  ) comps;
          );
      (*
      ** Examine VHDL source hierarchy and extract more VHDL components.
      *)
      let rec analyze vhdl =
          let file = sprintf "%s.vhdl" vhdl in
          out (sprintf "Analyzing VHDL hierarchy in <%s>..." file);
          let ic = open_in file in
          let comps = ref [] in
          let more = ref true in
          let in_arch = ref false in
          while !more 
          do
              let line = 
                  let str = ref "" in
                  more := protects (str := input_line ic);
                  !str in
              let find str = protects (
                      __(Str.search_forward (Str.regexp str) line 0)) in
              let is_arch = find "architecture" in
              let is_comp = find "component" in
              let is_pack = find "package" in     (* ignore *)

              if is_pack then more := false else
              if is_arch then in_arch := true
              else if is_comp then
              begin
                  (*
                  ** Assumption: XXX component <compnam> XXXX
                  *)
                  let sl = Str.split (Str.regexp " ") line in
                  let sl = List.filter (fun str -> str <> "") sl in
                  let cname =
                      match sl with
                      | "component"::(name::_) -> name;
                      | _::("component"::(name::_)) -> name;
                      | "end"::_ -> "";
                      | _ -> out (sprintf 
                                  "Warning: %s: can't recognize component in VHDL file <%s>. Expectzed syntax: XXX component <compname> XXXX."
                                  tool file);
                             "" 
                      in
                  if cname <> "" then
                  begin
                      let path = add_path (compiler.t_incl@[compiler.t_output]) cname in
                      if not (List.mem path !comps) then
                          comps := !comps @ [path];
                  end;
              end;    
          done;
          src := !src @ !comps;
          List.iter (fun vhdl -> analyze vhdl) !comps;
          close_in ic
          in
      List.iter (fun vhdl -> analyze vhdl) !src_vhdl;
      List.iter (fun modu -> find_src modu) modu.mod_external;
      in
  find_src modu;
  !src


let open_toolset tool =   
  let e str = error 0 (sprintf "TDI: open_toolset: %s" str) in  
  out (sprintf "TDI: Opening tool <%s>..." tool);
  let synth_tool =
    match List.filter (fun st -> st.syn_tool = tool) !synth_tools with
    | [st] -> st;
    | _ -> e (sprintf "Can't find synthesis tool descriptor for tool <%s>." tool) in
  compiler.t_synth_tool <- synth_tool; 
  let old_pos = an.a_curpos in 
  an.a_curpos <- nilsrc ();  
  ind_incr ();
  let tops,lmap = syntax_of_file tool in
  an.a_modu.mod_fmap <- an.a_modu.mod_fmap @ lmap;
  
  let tdi = new tdi tool tops lmap in
  __(tdi#interp "init");
  ind_decr ();
  an.a_curpos <- old_pos;
  vhdl_tools := !vhdl_tools @ [tool,tdi]


let emit_toolset modu =
  if !vhdl_tools <> [] then
  begin
    let e str = error 0 (sprintf "TDI: emit_toolset: %s" str) in  
    let proj = compiler.t_top  in

    let mod_obj = Cp_module.modu modu in

    let portl,_ = mod_obj#vhdl_port in
    let ports = 
      let i = ref 0 in
      List.map (fun s ->
        incr i;
        sprintf "port[%d]" !i, 
          TDE_str (
            let s' = Str.global_replace (Str.regexp "[ ]+signal ") "" s in
            let s' = Str.global_replace (Str.regexp ":\|;") "" s' in
            match Str.split (Str.regexp " \|(\|)") s' with
            | [n;d;t;r1;rd;r2] -> sprintf "%s:%s:%s:%s %s %s" n d t r1 rd r2;
            | [n;d;t] -> sprintf "%s:%s:%s" n d t;
            | _ -> e (sprintf "Unexpected signal port format <%s> found." s);  
            s'
          )
        ) (List.filter (fun s -> Str.string_match (Str.regexp "[ ]+signal") s 0) portl) in
    let targets = List.rev (List.map (fun em -> 
                                match em with 
                                | ENV_str s -> s;
                                | _ -> error 0 (sprintf "Unexpected environment target format found."))
                                (get_env_val "targets")) in
    let dev = 
      match targets with
      | dev :: _ -> dev;
      | _ -> "unknown device" in

    List.iter (fun (tool,tdi) ->
      if compiler.t_synth_tool.syn_vhdl_lib = [] 
        then error 0 (sprintf "No VHDL library found for tool <%s>." tool);
      let vhdl_srcs = 
        let l = ref [] in
        List.iter (fun vhdl ->
          if not (List.mem vhdl !l) then l := !l @ [vhdl];
          ) (vhdl_src modu tool);
        !l @ (if compiler.t_synth_tool.syn_vhdl_lib <> [] then ["conpro"] else []) in
      let (env: (string*tdi_expr) list) = 
        (let n = ref 0 in 
          List.map (fun target -> 
            incr n;
            sprintf "target[%d]" !n,TDE_str target
            ) targets) @
        (let n = ref 0 in 
          List.map (fun target -> 
            incr n;
            let strl = Str.split (Str.regexp "-") target in
            match strl with
            | [dev;pkg;spd] ->
              sprintf "target_device[%d]" !n,TDE_str dev;
            | [dev;pkg] ->
              sprintf "target_device[%d]" !n,TDE_str dev;
            | [dev] -> 
              sprintf "target_device[%d]" !n,TDE_str dev;
            | _ -> e (sprintf "Unexpected target <%s> found." target); 
            ) targets) @
        (let n = ref 0 in 
          List.map (fun target -> 
            incr n;
            let strl = Str.split (Str.regexp "-") target in
            match strl with
            | [dev;pkg;spd] ->
              sprintf "target_package[%d]" !n,TDE_str pkg;
            | [dev;pkg] ->
              sprintf "target_package[%d]" !n,TDE_str pkg;
            | [dev] -> 
              "?",TDE_str "?";
            | _ -> e (sprintf "Unexpected target <%s> found." target); 
            ) targets) @
        (let n = ref 0 in 
          List.map (fun target -> 
            incr n;
            let strl = Str.split (Str.regexp "-") target in
            match strl with
            | [dev;pkg;spd] ->
              sprintf "target_speed[%d]" !n,TDE_str spd;
            | [dev;pkg] ->
              "?",TDE_str "?";
            | [dev] -> 
              "?",TDE_str "?";
            | _ -> e (sprintf "Unexpected target <%s> found." target); 
            ) targets) @
        (let n = ref 0 in 
          List.map (fun target -> 
            incr n;
            let dev = get_dev_info target in
            sprintf "target_class[%d]" !n,TDE_str dev.dev_class
            ) targets) @
        (let n = ref 0 in 
          List.map (fun target -> 
            incr n;
            let dev = get_dev_info target in
            sprintf "target_library[%d]" !n,TDE_str dev.dev_library
            ) targets) @
        (let n = ref 0 in 
          List.map (fun target -> 
            incr n;
            let dev = get_dev_info target in
            sprintf "target_family[%d]" !n,TDE_str dev.dev_family
            ) targets) @
        (let n = ref 0 in
          List.map (fun vhdl ->
            incr n;
            sprintf "vhdl[%d]" !n,TDE_str vhdl
            ) vhdl_srcs) @
        ["proj",TDE_str proj] @
        ["simu_cycles",TDE_str (sprintf "%d" (get_env_int "simu_cycles"));
         "simu_res",TDE_str (sprintf "%d" (get_env_int "simu_res"));
         "clock",TDE_str (Int64.to_string (get_env_int64 "clock"));
         "clock_level",TDE_str (Int64.to_string (get_env_int64 "clock_level"));
         "reset_level",TDE_str (Int64.to_string (get_env_int64 "reset_level"));
         "period",TDE_str (Int64.to_string (per_of_freq (get_env_int64 "clock") Hz))] @ 
        ( let n = ref 0 in
          List.concat (List.map (fun (devre,devoptl) ->
            let devre' = Str.global_replace (Str.regexp "*") "[.]*" devre in
            if Str.string_match (Str.regexp devre') dev  0 then  
              (List.map (fun (o,v) -> 
                incr n;
                sprintf "syn_synth_set[%d]" !n,TDE_str (sprintf "%s %s" o v)
                ) devoptl)
            else
              []
          ) compiler.t_synth_tool.syn_synth_set)) @
        ( let n = ref 0 in
          let pcl = ref ["",1] in
          let rec get_n l r p =
            match l with
            | (p',n') :: tl ->
              if p' = p then
              begin
                pcl := r @ [p,n'+1] @ tl;
                n'
              end
              else get_n tl (r@[p',n']) p;
            | [] -> 
                pcl := r @ [p,1];
                1 in

          List.concat (List.map (fun (devre,devoptl) ->
            let devre' = Str.global_replace (Str.regexp "*") "[.]*" devre in
            if Str.string_match (Str.regexp devre') dev  0 then 
            begin 
              (List.map (fun (o,v) -> 
                let e,p,o = 
                  match Str.split (Str.regexp ":") o with
                  | [p;o] -> "",sprintf "_%s" p,o
                  | [o] when (o <> "" && o.[0] <> '$') -> "","",o
                  | [o] when (o <> "" && o.[0] = '$') -> String.sub o 1 ((String.length o)-1),"",""
                  | _ -> e (sprintf "Unexpected option format <%s> found." o) in
                if e <> "" then
                  e,TDE_str v
                else
                begin
                  let n = get_n !pcl [] p in
                  sprintf "syn_tech_set%s[%d]" p n,TDE_str (sprintf "%s %s" o v)
                end;
                ) devoptl)
            end
            else
              []
          ) compiler.t_synth_tool.syn_tech_set)) @
        ports
        in

      let obj = tdi#new_obj proj (List.filter (fun (e,v) -> (e,v) <> ("?",TDE_str "?")) env) in
      __(obj#interp "init");  
      __(obj#interp "compile");
      ) !vhdl_tools
  end;    
    
