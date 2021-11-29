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
**    $CREATED:     6.3.2009
**    $VERSION:     2.02
**
**    $INFO:
**
**  Statistic Module
**
**    $ENDOFINFO
**
*)

open Cp_common
open Cp_types
open Cp_symbol
open Cp_utils
open Cp_data
open Cp_analysis
open Cp_ucode
open Cp_expr
open Printf
open Cp_print

type stat_env = {
  stat_name: string;
  stat_desc: (string,stat_env) Hashtbl.t;
  mutable stat_count: int;
}

let stat_env:((string,stat_env) Hashtbl.t) =  Hashtbl.create 100


let lookup_env env name = 
  if Hashtbl.mem env name then
    Hashtbl.find env name
  else
  begin
    error 0 (sprintf "lookup_env: statistic parameter <%s> not found." name) 
  end
    
let try_lookup_env env name = 
  if Hashtbl.mem env name then
    Some (Hashtbl.find env name)
  else
    None

let get_env env = 
  let l = ref [] in
  Hashtbl.iter (fun name tde ->
              l := !l @ [tde];
          ) env;
  !l

let stat name desc =
  match try_lookup_env stat_env name with
  | Some sd ->
  begin
    sd.stat_count <- sd.stat_count + 1;
    if desc <> "" then
      match try_lookup_env sd.stat_desc desc with
      | Some sd' ->
        sd'.stat_count <- sd'.stat_count + 1;
      | None ->
        Hashtbl.add sd.stat_desc desc {
          stat_name=desc;
          stat_desc=Hashtbl.create 1;
          stat_count=1;
        };
  end;
  | None -> 
    let sd = {
        stat_name=name;
        stat_desc=Hashtbl.create 100;
        stat_count=1;
      } in
    if desc <> "" then 
      Hashtbl.add sd.stat_desc desc {
        stat_name=desc;
        stat_desc=Hashtbl.create 1;
        stat_count=1;
      };
    Hashtbl.add stat_env name sd

let stat_set name desc n m =
  match try_lookup_env stat_env name with
  | Some sd ->
  begin
    sd.stat_count <- n;
    if desc <> "" then
      match try_lookup_env sd.stat_desc desc with
      | Some sd' ->
        sd'.stat_count <- sd'.stat_count + 1;
      | None ->
        Hashtbl.add sd.stat_desc desc {
          stat_name=desc;
          stat_desc=Hashtbl.create 1;
          stat_count=m;
        };
  end;
  | None -> 
    let sd = {
        stat_name=name;
        stat_desc=Hashtbl.create 100;
        stat_count=n;
      } in
    if desc <> "" then 
      Hashtbl.add sd.stat_desc desc {
        stat_name=desc;
        stat_desc=Hashtbl.create 1;
        stat_count=m;
      };
    Hashtbl.add stat_env name sd

let print_stat () =
  compiler.t_silent <- false;
  let out_ind' = !out_ind in
  out_ind := 2;
  out "";
  let stat_major = 
    List.sort 
      (fun a b -> if a >b then 1 else if a < b then (-1) else 0) 
        (get_env stat_env) in
  let rows = ref [] in
  List.iter (fun sd ->
        rows := !rows @ [{
          row_cols = [
            {
              col_width = 45;
              col_align = 'l';
              col_cont = sd.stat_name;
            };
            {
              col_width = 30;
              col_align = 'l';
              col_cont = (
                if sd.stat_count > 0 then sprintf "%d" sd.stat_count
                else "");
            }];
          }];
             
        rows := !rows @ (
            let stat_minor = 
              List.sort 
                (fun a b -> if a >b then 1 else if a < b then (-1) else 0) 
                  (get_env sd.stat_desc) in
            List.map (fun sd' ->
              {
                row_cols = [
                  {
                    col_width = 45;
                    col_align = 'l';
                    col_cont = sprintf " -> %s" sd'.stat_name;
                  };
                  {
                    col_width = 30;
                    col_align = 'l';
                    col_cont = sprintf "%s%d%s" 
                                      (if sd.stat_count > 0 then "(" else "")
                                      sd'.stat_count
                                      (if sd.stat_count > 0 then ")" else "");
                  }];
              };
              ) stat_minor
          );
      ) stat_major;
  let tab = {
    tab_title = "SYNTHESIS RESULT SUMMARY";
    tab_outline = true;
    tab_width = 75;
    tab_header = [
     {
      row_cols = [
       {
          col_width = 45;
          col_align = 'l';
          col_cont = "DESCRIPTION";
        };
        {
          col_width = 30;
          col_align = 'l';
          col_cont = "VALUE";
        }];
      }];
    
    tab_rows = !rows;
    } in
  let lines = format_table tab in
  List.iter out lines;
  out "";
  out_ind := out_ind'

(*
*************************
** ANALYSIS SUMMARY
*************************
*) 
let analysed = ref []
let analyse_log loc str =
  out (sprintf "%s: %s" loc str);
  analysed := !analysed @ [loc,str]


let analyse_summary () =
  let out_ind' = !out_ind in
  out_ind := 2;
  out_ "";
  let rows = ref [] in
  List.iter (fun (loc,msg) ->
        rows := !rows @ [{
          row_cols = [
            {
              col_width = 30;
              col_align = 'l';
              col_cont = loc;
            };
            {
              col_width = 50;
              col_align = 'l';
              col_cont = msg;
            }];
          }];
             
      ) !analysed;
  let tab = {
    tab_title = "ANALYSIS SUMMARY";
    tab_outline = true;
    tab_width = 80;
    tab_header = [
     {
      row_cols = [
       {
          col_width = 30;
          col_align = 'l';
          col_cont = "DESCRIPTION";
        };
        {
          col_width = 50;
          col_align = 'l';
          col_cont = "MESSAGE";
        }];
      }];
    
    tab_rows = !rows;
    } in
  let lines = format_table tab in
  List.iter (fun line -> out_ line) lines;
  out_ "";
  out_ind := out_ind'
   
   
(*
*************************
** OPTIMIZE SUMMARY
*************************
*) 
let optimized = ref []
let optimize_log loc str =
  out (sprintf "%s: %s" loc str);
  if not (List.mem (loc,str) !optimized) then
    optimized := !optimized @ [loc,str]

let optimize_summary () =
  let out_ind' = !out_ind in
  out_ind := 2;
  out_ "";
  let rows = ref [] in
  List.iter (fun (loc,msg) ->
        rows := !rows @ [{
          row_cols = [
            {
              col_width = 30;
              col_align = 'l';
              col_cont = loc;
            };
            {
              col_width = 50;
              col_align = 'l';
              col_cont = msg;
            }];
          }];
             
      ) !optimized;
  let tab = {
    tab_title = "OPTIMIZATION SUMMARY";
    tab_outline = true;
    tab_width = 80;
    tab_header = [
     {
      row_cols = [
       {
          col_width = 30;
          col_align = 'l';
          col_cont = "LOCATION";
        };
        {
          col_width = 50;
          col_align = 'l';
          col_cont = "MESSAGE";
        }];
      }];
    
    tab_rows = !rows;
    } in
  let lines = format_table tab in
  List.iter (fun line -> out_ line) lines;
  out_ "";
  out_ind := out_ind'
  
(*
*************************
** UCODE SUMMARY
*************************
*) 

let ucoded = ref []
let ucode_log loc str =
  out (sprintf "%s: %s" loc str);
  if not (List.mem (loc,str) !ucoded) then
    ucoded := !ucoded @ [loc,str]

let ucode_summary () =
  let out_ind' = !out_ind in
  out_ind := 2;
  out_ "";
  let rows = ref [] in
  List.iter (fun (loc,msg) ->
        rows := !rows @ [{
          row_cols = [
            {
              col_width = 30;
              col_align = 'l';
              col_cont = loc;
            };
            {
              col_width = 50;
              col_align = 'l';
              col_cont = msg;
            }];
          }];
             
      ) !ucoded;
  let tab = {
    tab_title = "UCODE SYNTHESIS SUMMARY";
    tab_outline = true;
    tab_width = 80;
    tab_header = [
     {
      row_cols = [
       {
          col_width = 30;
          col_align = 'l';
          col_cont = "LOCATION";
        };
        {
          col_width = 50;
          col_align = 'l';
          col_cont = "MESSAGE";
        }];
      }];
    
    tab_rows = !rows;
    } in
  let lines = format_table tab in
  List.iter (fun line -> out_ line) lines;
  out_ "";
  out_ind := out_ind'

(*
*************************
** RTL SUMMARY
*************************
*) 

let rtled = ref []
let rtl_log loc str =
  out (sprintf "%s: %s" loc str);
  if not (List.mem (loc,str) !rtled) then
    rtled := !rtled @ [loc,str]

let rtl_summary () =
  let out_ind' = !out_ind in
  out_ind := 2;
  out_ "";
  let rows = ref [] in
  List.iter (fun (loc,msg) ->
        rows := !rows @ [{
          row_cols = [
            {
              col_width = 30;
              col_align = 'l';
              col_cont = loc;
            };
            {
              col_width = 50;
              col_align = 'l';
              col_cont = msg;
            }];
          }];
             
      ) !rtled;
  let tab = {
    tab_title = "RTL SYNTHESIS SUMMARY";
    tab_outline = true;
    tab_width = 80;
    tab_header = [
     {
      row_cols = [
       {
          col_width = 30;
          col_align = 'l';
          col_cont = "LOCATION";
        };
        {
          col_width = 50;
          col_align = 'l';
          col_cont = "MESSAGE";
        }];
      }];
    
    tab_rows = !rows;
    } in
  let lines = format_table tab in
  List.iter (fun line -> out_ line) lines;
  out_ "";
  out_ind := out_ind'

(*
*************************
** CALL GRAPH SUMMARY
*************************
*) 

let called = ref []
let call_log pro =
  let rec pl l =
    match l with
    | [pro] -> pro.pro_name
    | pro :: tl -> (sprintf "%s " pro.pro_name)^(pl tl)
    | [] -> "" in
  let rec el l =
    match l with
    | [e] -> e.tx_name
    | e :: tl -> (sprintf "%s " e.tx_name)^(el tl)
    | [] -> "" in
  let rec cl l =
    match l with
    | [e] -> sprintf "[%s]" (el e.catch_exl)
    | e :: tl -> (sprintf "[%s] " (el e.catch_exl))^(cl tl)
    | [] -> "" in
  out (sprintf "call_graph_build: process '%s': start={%s} stop={%s} call={%s} raise={%s} catch={%s}"
                 pro.pro_name
                 (pl pro.pro_control.pro_start)
                 (pl pro.pro_control.pro_stop)
                 (pl pro.pro_control.pro_call)
                 (el pro.pro_control.pro_raise)
                 (cl pro.pro_control.pro_catch));
  
  called := !called @ [
    pro.pro_name,sprintf "";
    ".. start",(pl pro.pro_control.pro_start);
    ".. stop",(pl pro.pro_control.pro_stop);
    ".. call",(pl pro.pro_control.pro_call);
    ".. raise",(el pro.pro_control.pro_raise);
    ".. catch",(cl pro.pro_control.pro_catch);
  ]
let call_exc_log co =
  let rec pl l =
    match l with
    | [pro] -> pro.pro_name
    | pro :: tl -> (sprintf "%s " pro.pro_name)^(pl tl)
    | [] -> "" in
  out (sprintf "call_graph_build: exception register'%s': read={%s} write={%s}"
                 co.co_name
                 (pl co.co_reader)
                 (pl co.co_writer));
  
  called := !called @ [
    co.co_name,sprintf "";
    ".. read",(pl co.co_reader);
    ".. write",(pl co.co_writer);
  ]

let call_summary () =
  let out_ind' = !out_ind in
  out_ind := 2;
  out_ "";
  let rows = ref [] in
  List.iter (fun (loc,msg) ->
        rows := !rows @ [{
          row_cols = [
            {
              col_width = 30;
              col_align = 'l';
              col_cont = loc;
            };
            {
              col_width = 50;
              col_align = 'l';
              col_cont = msg;
            }];
          }];
             
      ) !called;
  let tab = {
    tab_title = "CALL GRAPH SUMMARY";
    tab_outline = true;
    tab_width = 80;
    tab_header = [
     {
      row_cols = [
       {
          col_width = 30;
          col_align = 'l';
          col_cont = "PROCESS";
        };
        {
          col_width = 50;
          col_align = 'l';
          col_cont = "";
        }];
      }];
    
    tab_rows = !rows;
    } in
  let lines = format_table tab in
  List.iter (fun line -> out_ line) lines;
  out_ "";
  out_ind := out_ind'
