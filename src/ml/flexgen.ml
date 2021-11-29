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
**    $INITIAL:     (C) 2006-2008 BSSLAB
**    $CREATED:     27.9.2008
**    $VERSION:     2.02
**
**    $INFO:
**
**  FLEXGEN frontend
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
open Cp_flex

open Printf 
open Unix


let out str =
    print_string str;
    print_newline ()

let max_opt = 3 

let usage str =
    let msg = 
    "\n"^
    (version ())^"\n\n"^
    "usage: flexgen\n"^ 
    "       #,#,#: generate license file for\n"^
    "       feature,hostid,filen\n"^
    "       -hostid: print hostid\n\n"^
    "#########################################################\n"^str^"\n"^ 
    "#########################################################\n"
    in
    out_ msg;
    exit 1

let my () =
    let msg = version () in
    out_ msg;
    exit 1

let fail msg =
    out_ ("FLEXGEN: "^msg);
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
            | "-h" -> usage "Help:";
            | "-V" -> my ();
            | "-VM" -> List.iter (fun mv -> out mv) Cp_version.mod_versions; exit 0;
            | "-hostid" ->
              print_hostid ();
              exit 0;
            | _ -> 
            begin
              let get_args l =
                  match l with
                  | args::tl ->
                  begin
                    let argl = Str.split (Str.regexp ",") args in
                    argl;
                  end;
                  | [] -> usage "Missing arguments." in
              match get_args (hd::tl) with
              | [feature;hostid;file] -> protect(license_gen feature hostid file true); exit 0;
              | _ -> usage "Wrong number of arguments.";
            end;
        end;
        | [] -> ();
      with _ -> usage "Invalid option format.";
      in

    iter args;

    out_ (version ());
      
    let stop_time = Unix.time () in
    out_ (sprintf "Total CPU time consumed: %.1f seconds." (Sys.time ()));
    out_ (sprintf "Total time elapsed: %.1f seconds." (stop_time-.start_time));
  end
  with
  | Synthesis str when not compiler.t_trace -> 
    print_error str;
    if not compiler.t_check then
      close_out !log_oc;
    exit 1;
  | Exit when not compiler.t_trace -> 
    if not compiler.t_check then
      close_out !log_oc;
    exit 1;
  | _ when not compiler.t_trace -> 
    print_error "Internal Error";
    if not compiler.t_check then
      close_out !log_oc;
    exit 1

let _ = 
  let id = Thread.create main () in
  Thread.join id  
