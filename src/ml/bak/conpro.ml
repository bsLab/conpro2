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
**    $INITIAL:     (C) 2006-2010 BSSLAB
**    $CREATED:     1.3.2006
**    $VERSION:     2.19
**
**    $INFO:
**
**  CONPRO frontend
**
**    $ENDOFINFO
**
*)

open Cp_version
open Cp_common
open Cp_syntax
open Cp_types
open Cp_block_frame
open Cp_utils
open Cp_analysis
open Cp_synthesis
open Cp_Core
open Cp_printtypes
open Cp_graph
open Cp_emi
open Cp_flex
open Cp_tdi
open Cp_stat
open Cp_symbol
open Cp_module

open Printf 
open Unix



let max_opt = 3 
let tool = ref "" 

let usage str =
    let authorized = license_check "CONPRO2" in
    compiler.t_silent <- false;
    let msg = 
    "\n"^
    (version ())^"\n\n"^
    (sprintf "License check: %b.\n\n" authorized)^
    "usage: conpro <file.cp>\n"^ 
    "       -I #: add source include/search path #\n"^
    "       -L #: add library include/search path #\n"^
    "       -O #: output directory #\n"^
    "       -D #=#: create or overide project constant with new value\n"^
    "       -uc: emit MicroCode only, no VHDL output\n"^
    "       -ml: emit ML only, no VHDL output\n"^
    "       -C: emit C only, no VHDL output\n"^
    "       -proj #: short project name #\n"^
    "       -ucframe: print block frame info in MicroCode assembler code\n"^
    "       -tool #[,#]: VHDL synthesis tool support\n"^
    "          xilinx#v: Xilinx ISE [S,B]\n"^
    "          actel#v: Actel Designer[S]\n"^
    "          leonardo#v: Mentor Graphics Leonardo Spectrum[S]\n"^
    "          alliance#v: LIP6 Alliance[S,B]\n"^
    "            #S,#B: Synthesis tool (VHDL) and backend (place&route)\n"^
    "            #v:1..99: version (optional)\n"^
    "       -check: don't generate output file\n"^
    "       -opt #,#,...: optimization [default: fold,dead,ucomp,ubind]\n"^
    "          no: no default optimization or empty default set [experts only or clearance of defaults]\n"^
    "          fold: constant folding in expressions\n"^
    "          move: constant expression motion out of loops and branches\n"^
    "          dead: remove unreferenced objects and related expressions\n"^
    "          ucomp: compact microcode\n"^
    "          ubind: bind microcode\n"^
    "       -dump #,#,...: dump internal type structures\n"^
    "          ast:syntax tree\n"^ 
    "          pi: process instructions (tree format)\n"^
    "          cp: process instructions (conpro format)\n"^
    "          ui: microcode instructions\n"^
    "          st: RTL states)\n"^
    "       -graph #,#,...: emit additional graph file (dot format)[requires dot & psutils:epsffit]\n"^
    "          ft: process frame times\n"^
    "          ftu: process frame times with linked instruction code\n"^
    "          st: process states\n"^
    "       -dev #,#,...: device targets\n"^
    "          xc2s100-tq144-5,xc18v01 [examples]\n"^
    "       -trace: print backtrace on exception [expert]\n"^
    "       -noft: no blockframe time calculation\n"^
    "       -debug #,#,...: debug or verbose class function name\n"^
    "       -Debug: add additional debug informations\n"^
    "       -debug_targets: print all debug targets\n"^
    "       -nosilent: print detailed messages\n"^
    "       -V: version\n"^
    "       -VM: module versions\n"^
    "       -h: help\n"^ 
    "       -M #,#,..: open module #\n"^ 
    "       -flexgen #,#,#: generate license file for\n"^
    "                feature,hostid,filen\n"^
    "       -hostid: print hostid\n\n"^
    "<file.cp>: Toplevel CONPRO file without a preceeding path!\n"^ 
    "           Use -I # for specifying path to different source location!\n"^ 
    "#########################################################\n"^str^"\n"^ 
    "#########################################################\n"
    in
    out msg;
    out_ str;
    exit 1

let my () =
    compiler.t_silent <- false;
    let msg = version () in
    out msg;
    exit 1

let fail msg =
    out ("CONPRO: "^msg);
    exit 1


let main () =
  try
  begin
    let start_time = Unix.time () in
    let args = List.tl (Array.to_list Sys.argv) in

    let rec iter args =
      try
        match args with
        | hd::tl ->
        begin
            match hd with
            | "-dump" ->
              let get_args l =
                  match l with
                  | args::tl ->
                  begin
                    let argl = Str.split (Str.regexp ",") args in
                    List.iter (fun arg ->
                      match arg with
                      | "ast" | "pi" | "ui" | "st" | "cp" ->
                        if not (List.mem arg compiler.t_dump) then 
                          compiler.t_dump <- compiler.t_dump @ [arg];
                      | "all" -> 
                        compiler.t_dump <- compiler.t_dump @ ["ast" ; "pi" ; "ui" ; "st"];
                      |  _ -> usage (sprintf "Unknown dump type <%s>." arg);
                      ) argl;
                    tl;                    
                  end;
                  | [] -> usage "Missing dump type arguments." in
              iter (get_args tl);
            | "-graph" ->
              let get_args l =
                  match l with
                  | args::tl ->
                  begin
                    let argl = Str.split (Str.regexp ",") args in
                    List.iter (fun arg ->
                      match arg with
                      | "ft" | "ftu" | "st" ->
                        compiler.t_graph <- compiler.t_graph @ [arg];
                      |  _ -> usage (sprintf "Unknown graph type <%s>." arg);
                      ) argl;
                    tl;                    
                  end;
                  | [] -> usage "Missing graph type arguments." in
              iter (get_args tl);
            | "-opt" ->
              let get_args l =
                  match l with
                  | args::tl ->
                  begin
                    let argl = Str.split (Str.regexp ",") args in
                    List.iter (fun arg ->
                      match arg with
                      | "fold" | "move" | "dead" | "ucomp" | "ubind" ->
                        compiler.t_opt <- compiler.t_opt @ [arg];
                      | "no" ->
                        compiler.t_opt <- []; 
                      |  _ -> usage (sprintf "Unknown optimization type <%s>." arg);
                      ) argl;
                    tl;                    
                  end;
                  | [] -> usage "Missing optimization type arguments." in
              iter (get_args tl);
            | "-tool" ->
              let get_args l =
                  match l with
                  | args::tl ->
                  begin
                    let argl = Str.split (Str.regexp ",") args in
                    let synth = ref "" in
                    match argl with
                    | [sb] -> tool:= sb; tl;
                    | [s;b] -> tool := sprintf "%s_%s" s b; tl;
                    | _ -> usage (sprintf "Unknown tool type <%s>." args);
                  end;
                  | [] -> usage "Missing toolset arguments." in
              iter (get_args tl);
            | "-dev" ->
              let get_args l =
                  match l with
                  | args::tl ->
                  begin
                    let argl = Str.split (Str.regexp ",") args in
                    List.iter (fun arg ->
                        set_env "targets" (ENV_str arg);
                      ) argl;
                    tl;                    
                  end;
                  | [] -> usage "Missing target device arguments." in
              iter (get_args tl);
            | "-I" -> 
              let incl = 
                match tl with
                | hd::tl -> iter tl; [hd];
                | [] -> usage "Usage:" in
              compiler.t_incl <- incl @ compiler.t_incl;
            | "-L" -> 
              let lib = 
                match tl with
                | hd::tl -> iter tl; [hd];
                | [] -> usage "Usage:" in
              compiler.t_lib <- lib @ compiler.t_lib;
            | "-O" -> 
              compiler.t_output <- (
                        match tl with
                        | hd::tl -> iter tl; 
                          let path = hd in
                          if path.[(String.length path)-1] <> '/' then
                            path^"/"
                          else
                            path
                        | [] -> usage "Usage:";
                        );
            | "-uc" -> compiler.t_ucode <- true; iter tl;
            | "-ml" -> 
              compiler.t_ml <- true;  
              iter tl;
            | "-C" -> 
              compiler.t_C <- true;  
              iter tl;
            | "-proj" ->
              compiler.t_proj <- (
                        match tl with
                        | hd::tl -> 
                          iter tl; 
                          hd
                        | [] -> usage "Usage:";
                        );
            | "-D" ->
              compiler.t_defs <- compiler.t_defs @ [
                        match tl with
                        | hd::tl -> 
                        begin
                          iter tl; 
                          let strl = Str.split (Str.regexp "=") hd in
                          match strl with
                          | [n;v] -> n,v
                          | _ -> usage "Invalid parameter definition"
                        end; 
                        | [] -> usage "Usage:";
                        ];
            | "-check" -> compiler.t_check <- true; iter tl;
            | "-s" -> compiler.t_module <- true; iter tl;
            | "-trace" -> compiler.t_trace <- true; iter tl;
            | "-debug" -> 
              let get_args l =
                  match l with
                  | args::tl ->
                  begin
                    let argl = Str.split (Str.regexp ",") args in
                    debug_it_select_list := !debug_it_select_list @ argl;
                    tl;                    
                  end;
                  | [] -> usage "Missing verbose type arguments." in
              iter (get_args tl);
            | "-Debug" -> compiler.t_debug <- true; iter tl;
            | "-debug_targets" ->
                let targets = debug_targets () in
                out "Debug targets:";
                
                List.iter (fun t->
                  out (sprintf "  %s" t)
                  ) targets;
                my ();
            | "-M" -> 
              let get_args l =
                  match l with
                  | args::tl ->
                  begin
                    let argl = Str.split (Str.regexp ",") args in
                    compiler.t_modules <- compiler.t_modules @ argl;
                    tl;                    
                  end;
                  | [] -> usage "Missing verbose type arguments." in
              iter (get_args tl);
            | "-ucframe" -> 
              debug_it_select_list := !debug_it_select_list @ ["ui_frame"]; iter tl;
            | "-noft" -> 
              compiler.t_bf_time_calc <- false;
              iter tl;
            | "-nosilent" -> 
              compiler.t_silent <- false;
              iter tl;
            | "-h" -> usage "Help:";
            | "-V" -> my ();
            | "-VM" -> List.iter (fun mv -> out_ mv) Cp_version.mod_versions; exit 0;
            | "-flexgen" -> 
            begin
              let get_args l =
                  match l with
                  | args::tl ->
                  begin
                    let argl = Str.split (Str.regexp ",") args in
                    argl;
                  end;
                  | [] -> usage "Missing arguments." in
              match get_args tl with
              | [feature;hostid;file] -> protect(license_gen feature hostid file false); exit 0;
              | _ -> usage "Wrong number of arguments.";
            end;
            | "-hostid" ->
              print_hostid ();
              exit 0;
            | _ -> compiler.t_files <- compiler.t_files @ [hd]; iter tl; 
        end;
        | [] -> ();
      with _ -> usage "Invalid option format.";
      in

    iter args;

    if compiler.t_output <> "" then
    begin
      try __(Unix.stat compiler.t_output) with _ -> 
      error 0 (sprintf "Output directory <%s> doesn't exists or isn't accessable."  compiler.t_output);
    end;

    let version = version () in
    out version;
    out_ version;
    
    init_rules ();

    if compiler.t_modules <> [] then
    begin
      List.iter (fun mname ->
          __(open_module mname);
          ) compiler.t_modules; 
    end;



    if compiler.t_files = [] then usage "No filename(s) specified!";
    if (List.filter (fun f -> f.[0] = '-' || (Filename.dirname f) <> ".") compiler.t_files)  <> [] 
      then usage "Invalid file argument(s)!";
    
    let cpl = List.filter (fun f -> Filename.check_suffix f "cp") compiler.t_files in
    let ucl = List.filter (fun f -> Filename.check_suffix f "UC") compiler.t_files in
    
    
    if cpl <> [] then
      compiler.t_top <-  Filename.chop_extension (List.hd cpl);
      
    if not compiler.t_check then
    begin
      try
      begin
        log_oc := open_out (sprintf "%s%s.log" compiler.t_output compiler.t_top);
        out_ (sprintf "Opened message file <%s%s.log>. Detailed informations can be found in this file."  compiler.t_output compiler.t_top);  
      end
      with
        _ -> error 0 (sprintf "Can't open log file <%s%s>." compiler.t_output compiler.t_top);
    end;

    let authorized = license_check "CONPRO2" in
    if authorized then
    begin  
      let modus = ref [] in
      main_module := None;
      
      out "Global block constraints:";
      ind_incr ();
      print_block_constr compiler.t_block_constr;
      out (sprintf "Include paths: ");
      ind_incr ();
      List.iter (fun p -> out (sprintf "Source:  %s" p)) compiler.t_incl;
      List.iter (fun p -> out (sprintf "Library: %s" p)) compiler.t_lib;
      ind_decr ();
      ind_decr ();

      if !tool <> "" then
      begin
        out_ (sprintf "Initializing synthesis tools...");
        open_synth_tools ();
        out_ (sprintf "Initializing tool <%s>..." !tool);
        open_toolset !tool;
      end;  
      
      List.iter (fun cp -> 
        let mod_name = Filename.chop_extension cp in

        (*
        ** Compile specified main module...
        *)
        out_ (sprintf "Analyzing ConPro source <%s>..." cp);
        let syntax,filemap = Cp_analysis.syntax_of_file cp in


        (*
        ** mod_name => Main (toplevel) module
        *)
        
        out_ (sprintf "Compiling ConPro source <%s>..." cp);

        let modu = module_of_syntax mod_name syntax filemap in
        if !main_module = None then main_module := Some modu;
         
        modu.mod_syntax <- syntax;
        modus := !modus @ [modu];
      

        ) cpl;
      
      
      List.iter (fun uc ->
          out_ (sprintf "Compiling MicroCode source <%s>..." uc);
          let syntax,filemap = Cp_uci_analysis.syntax_of_file uc in
          let pro = new Cp_uci_in.uci_in (Filename.chop_extension uc) syntax filemap in
          () 
        ) ucl;
      
      (*
      **  architecture modules, too
      *)
      List.iter (fun sym ->
        match sym with
        | Sym_mod modu' -> if modu'.mod_procs = [] then modus := !modus @ [modu'];
        | _ -> ();
        ) (list_of_sym top_syms);
      
      if compiler.t_ml then
      begin
        List.iter (fun modu ->
          Cp_ml.ml_emit modu
          ) !modus;
      end
      else if compiler.t_C then
      begin
        List.iter (fun modu ->
          Cp_c.c_emit modu
          ) !modus;
      end
      else List.iter (fun modu -> 
        let mod_name = modu.mod_name in
      
        if modu.mod_procs <> [] then
        begin

          synthesize modu;

          out_ (sprintf "Emitting frame time calculations for module <%s>..." modu.mod_name);
          if compiler.t_bf_time_calc then
            print_frame_trees modu;
        end
        else
        begin
          emit_modu modu;
        end;
        compiler.t_top <- of_mod modu.mod_name;
        if !tool <> "" then out_ (sprintf "Emitting toolset <%s>..." !tool);

        emit_toolset modu;
        ) !modus;
      List.iter (fun modu -> 
        let mod_name = modu.mod_name in          
        if compiler.t_dump <> [] then
          begin
            List.iter (fun dump ->
              match dump with
              | "ast" -> 
                ast_print_syntax_list mod_name modu.mod_syntax;
              | "pi" -> 
                List.iter (fun pro ->
                    pi_print_instr_list (sprintf "%s_%s" mod_name pro.pro_name) pro.pro_instr;
                  ) modu.mod_procs;
              | "cp" -> 
                List.iter (fun pro ->
                    cp_print_instr_list (sprintf "%s_%s" mod_name pro.pro_name) pro.pro_instr;
                  ) modu.mod_procs;
              | "ui" -> 
                List.iter (fun pro ->
                    ui_print_instr_list (sprintf "%s_%s" mod_name pro.pro_name) pro.pro_ucode;
                  ) modu.mod_procs;
              | "st" -> 
                List.iter (fun pro ->
                    st_print_state_block_list (sprintf "%s_%s" mod_name pro.pro_name) pro.pro_states;
                  ) modu.mod_procs;

              | _ -> error 0 (sprintf "Unknown dump type <%s>." dump); 
              ) compiler.t_dump;  
          end;
          if compiler.t_graph <> [] then
          begin
            List.iter (fun pro ->
              if (List.mem "ft" compiler.t_graph) ||
                 (List.mem "ftu" compiler.t_graph) then
                blockframe_graph modu pro;
              if (List.mem "st" compiler.t_graph) then
                state_graph modu pro;
              ) modu.mod_procs;
          end;
        ) !modus;
    end
    else
    begin
      error 0 (sprintf "Access not authorized for machine <%s>." (Crypto.system_hostid ()));
    end;
      
    
    let stop_time = Unix.time () in
    out (sprintf "Total CPU time consumed: %.1f seconds." (Sys.time ()));
    out (sprintf "Total time elapsed: %.1f seconds." (stop_time-.start_time));
    stat_set "synthesis time [sec]" "" (int_of_float (stop_time-.start_time)) 0; 
    print_stat ();
    if not compiler.t_check then
        close_out !log_oc;
  end
  with
  | Synthesis str when not compiler.t_trace -> 
    print_error str;
    if !log_oc <> Pervasives.stdout then close_out !log_oc;
    exit 1;
  | Exit when not compiler.t_trace -> 
    if !log_oc <> Pervasives.stdout then close_out !log_oc;
    exit 1;     
  | _ when not compiler.t_trace -> 
    print_error "Internal Error";
    if !log_oc <> Pervasives.stdout then close_out !log_oc;
    exit 1

let _ = 
  let id = Thread.create main () in
  Thread.join id  
