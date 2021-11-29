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
**    $CREATED:     8.7.2008
**    $VERSION:     2.04
**
**    $INFO:
**
**  External Module Interface - Analysis
**
**    $ENDOFINFO
**
*)
open Cp_common
open Cp_types
open Cp_syntax
open Cp_symbol
open Cp_utils
open Cp_data
open Cp_printtypes
open Cp_data_core
open Unix
open Printf
open Cp_fun





let syntax_of_file name =
  let file = sprintf "%s.mod" name in
  file_name := file;
  out (sprintf "Searching module file <%s>..." file);
  an.a_curpos <- nilsrc();
  let tops = ref [] in
  let lmap = ref [] in

  try
  begin
    let paths = ref (""::(compiler.t_lib@compiler.t_incl)) in
    let success = ref false in
    let buf = ref "" in
    let prog = 
      while !paths <> [] && not !success
      do
        let ok = protects (
            let file = 
                match !paths with
                | ""::tl -> paths := tl; 
                            file;
                | hd::tl -> paths := tl;
                            sprintf "%s/%s" hd file;
                | [] -> file in

            let stat = Unix.stat file in
            let size = ref stat.st_size in
            let off = ref 0 in
            let fd = Unix.openfile file [O_RDONLY] 0 in
            out (sprintf "Opening module file <%s>..." file);

            buf := String.create !size;
            while (!size > 0)
            do
                let len = Unix.read fd !buf !off !size in
                off := !off + len;
                size := !size - len;
            done;
            Unix.close fd;
            ) in
        if not ok && !paths = [] then
        begin
            print_error (sprintf "Failed to read module file <%s>." file);
            exit 1;
        end;
        success := ok;
      done;
      !buf
      in

    (*
    ** File position to line mapping
    *)
    let line = ref 1 in
    let spos = ref 1 in
    let cpos = ref 1 in
    String.iter (fun c ->
            if c = '\n' then
            begin
                lmap := !lmap @ [file,!spos,!cpos,!line];
                incr line;
                spos := !cpos;
                incr cpos;
            end
            else
            begin
                incr cpos;
            end; 
        ) prog;

    let lb = Lexing.from_string prog in
    while true
    do
        try
        begin
            let res = Cp_parser_emi.main Cp_lexer_emi.token lb in
            tops := !tops @ [res];
        end
        with 
        | Cp_lexer_emi.EOF -> raise Exit;
        | _ ->
            let pos = lb.Lexing.lex_curr_pos in
            let l,p = line_of_pos {f_name= !file_name; f_cpos=pos} !lmap in
            print_syntax_error file l p;
            exit 1;
    done;
    !tops,!lmap
  end
  with
  | Sys_error str when not compiler.t_trace ->  
    raise (Synthesis (sprintf "Source code parsing failed: Sys_error %s" str));
  | Exit -> !tops,!lmap

