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
*)

open Version
open Cp_types
open Cp_common
open Cp_print
open Printf

let major = "2.1" 
let minor = "D197" 
let copyright = "(C) 2006-2017 by Dr. Stefan Bosse"
let vhdl_version = 
    "-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach\n"^   
    "--         "^copyright^"\n"^
    "--         Version: "^major^" Revision: "^minor^" Genetic size: "^genes^"\n"^
	"--         Compile date: "^compile_ver^"\n"^
 	"--         Compiled by:  "^compile_use^"\n"^
 	"--         Compiled on:  "^compile_mac^"\n"

let version () = 
  let tab = {
    tab_title = "CONPRO2";
    tab_outline = true;
    tab_width = 75;
    tab_header = [
     {
      row_cols = [
       {
          col_width = 75;
          col_align = 'c';
          col_cont = "High-Level Multi-Process Hardware Synthesis";
        }];
      }];
    
    tab_rows = [
     {
      row_cols = [
       {
          col_width = 15;
          col_align = 'l';
          col_cont = "Version:";
        };
        {
          col_width = 60;
          col_align = 'l';
          col_cont = major;
        }];
      };
    {
      row_cols = [
       {
          col_width = 15;
          col_align = 'l';
          col_cont = "Revision:";
        };
        {
          col_width = 60;
          col_align = 'l';
          col_cont = minor;
        }];
      };
     {
      row_cols = [
       {
          col_width = 15;
          col_align = 'l';
          col_cont = "Copyright:";
        };
        {
          col_width = 60;
          col_align = 'l';
          col_cont = copyright;
        }];
      };
    {
      row_cols = [
       {
          col_width = 15;
          col_align = 'l';
          col_cont = "Genetic size:";
        };
        {
          col_width = 60;
          col_align = 'l';
          col_cont = genes;
        }];
      };
     {
      row_cols = [
       {
          col_width = 15;
          col_align = 'l';
          col_cont = "Compile date:";
        };
        {
          col_width = 60;
          col_align = 'l';
          col_cont = compile_ver;
        }];
      };
     {
      row_cols = [
       {
          col_width = 15;
          col_align = 'l';
          col_cont = "Compiled by:";
        };
        {
          col_width = 60;
          col_align = 'l';
          col_cont = compile_use;
        }];
      };
     {
      row_cols = [
       {
          col_width = 15;
          col_align = 'l';
          col_cont = "Compiled on:";
        };
        {
          col_width = 60;
          col_align = 'l';
          col_cont = compile_mac;
        }];
      };
     {
      row_cols = [
       {
          col_width = 15;
          col_align = 'l';
          col_cont = "";
        };
        {
          col_width = 60;
          col_align = 'l';
          col_cont = "";
        }];
      }] @ (
      if (List.filter (fun f -> Filename.check_suffix f "cp") compiler.t_files) <> [] then       
     [{
      row_cols = [
       {
          col_width = 15;
          col_align = 'l';
          col_cont = "Input:";
        };
        {
          col_width = 60;
          col_align = 'l';
          col_cont = "ConPro Multi-Process Programming Language";
        }];
      }] else [])@ (
      if (List.filter (fun f -> Filename.check_suffix f "UC") compiler.t_files) <> [] then 
     [{
      row_cols = [
       {
          col_width = 15;
          col_align = 'l';
          col_cont = "Input:";
        };
        {
          col_width = 60;
          col_align = 'l';
          col_cont = "ConPro Intermediate MicroCode";
        }];
      }] else []) @ (if not compiler.t_ucode && not compiler.t_ml && not compiler.t_C then
     [{
      row_cols = [
       {
          col_width = 15;
          col_align = 'l';
          col_cont = "Output:";
        };
        {
          col_width = 60;
          col_align = 'l';
          col_cont = "VHDL Hardware RTL-Model";
        }];
      }] else [])@(if not compiler.t_ml && not compiler.t_C then [
     {
      row_cols = [
       {
          col_width = 15;
          col_align = 'l';
          col_cont = "Output:";
        };
        {
          col_width = 60;
          col_align = 'l';
          col_cont = "ConPro MicroCode";
        }];
      }] else if compiler.t_ml then
      [{
      row_cols = [
       {
          col_width = 15;
          col_align = 'l';
          col_cont = "Output:";
        };
        {
          col_width = 60;
          col_align = 'l';
          col_cont = "ML Functional Software Model";
        }];
      }] else    
      [{
      row_cols = [
       {
          col_width = 15;
          col_align = 'l';
          col_cont = "Output:";
        };
        {
          col_width = 60;
          col_align = 'l';
          col_cont = "C Imperative Software Model";
        }];
      }]    
      ) @(
      List.map (fun f -> {
      row_cols = [
       {
          col_width = 15;
          col_align = 'l';
          col_cont = "Input file:";
        };
        {
          col_width = 60;
          col_align = 'l';
          col_cont = f;
        }];
      }) compiler.t_files);
    } in
  let lines = format_table tab in
  let s = ref "  " in
  List.iter (fun l -> s := !s^"\n  "^l) lines;
  !s^"\n"
    

let mod_versions  =
  List.map (fun mv ->
      Str.global_replace (Str.regexp "\n") "" (Str.global_replace (Str.regexp "--") "    " mv);
    ) mod_versions
