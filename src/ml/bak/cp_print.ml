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
**    $INITIAL:     (C) 2006-2009 BSSLAB
**    $CREATED:     16.10.2006
**    $VERSION:     2.03
**
**    $INFO:
**
**  Print formatted output
**
**    $ENDOFINFO
**
*)

open Printf


let box indent fmt str =
  if fmt <> "" && fmt <> "%s" then
  begin
    let strl = Str.split (Str.regexp "\n") str in
    let str' = ref (if strl <> [] then List.hd strl else "") in
    let spaces n = String.make n ' ' in
    let fmtl = Str.split (Str.regexp "%s") fmt in
    let indent = if indent = 0 then String.length (List.hd fmtl) else indent in
    let n = ref ((List.length strl)-2) in
    if strl <> [] then List.iter (fun line ->
      decr n;
      if line <> "" then
      begin
        str' := sprintf "%s\n%s%s" !str' (spaces indent) line;
      end;
        ) (List.tl strl);
    sprintf (Obj.magic fmt) !str'
  end else str
 
let align indent str =
    let strl = Str.split (Str.regexp "\n") str in
    let str' = ref (if strl <> [] then List.hd strl else "") in
    let spaces n = String.make n ' ' in
    let n = ref ((List.length strl)-2) in
    if strl <> [] then List.iter (fun line ->
      decr n;
      if line <> "" then
      begin
        str' := sprintf "%s\n%s%s" !str' (spaces indent) line;
      end;
        ) (List.tl strl);
    sprintf "%s" !str'
 
let remove_last_nl str =
   if str <> "" then
   begin 
    let n = String.length str in
    if str.[n-1] = '\n' then String.sub str 0 (n-1) else str
   end else str
  
let rec hlist f l =
  match l with
  | hd::[] -> f hd
  | hd::tl -> sprintf "%s;%s" (f hd) (hlist f tl)
  | [] -> ""
let rec vlist f l =
  match l with
  | hd::[] -> remove_last_nl (f hd)
  | hd::tl -> sprintf "%s;\n%s" (remove_last_nl (f hd)) (vlist f tl)
  | [] -> ""
let rec ilist f l =
  match l with
  | hd::[] -> (f hd)
  | hd::tl -> sprintf "%s\n%s" (f hd) (ilist f tl)
  | [] -> ""
  

(*
** Add indentition to all lines
*)
let ibox indent str =
    let strl = Str.split (Str.regexp "\n") str in
    let str' = ref "" in
    let spaces n = String.make indent ' ' in
    let first = ref true in
    if strl <> [] then List.iter (fun line ->
      if line <> "" then
      begin
        str' := if !first then 
                  sprintf "%s%s" (spaces indent) line
                else
                  sprintf "%s\n%s%s" !str' (spaces indent) line;
        first := false;
      end;
        ) strl;
    sprintf "%s" !str'
(*
** Add indentition to all lines except the first
*)
  
let lbox indent str =
    let strl = Str.split (Str.regexp "\n") str in
    let str' = ref "" in
    let spaces n = String.make indent ' ' in
    let first = ref true in
    if strl <> [] then List.iter (fun line ->
      if line <> "" then
      begin
        str' := if !first then 
                  line
                else
                  sprintf "%s\n%s%s" !str' (spaces indent) line;
        first := false;
      end;
        ) strl;
    sprintf "%s" !str'
 
(*
** Formatted tables
*)
 
type table = {
  tab_outline: bool;
  tab_width: int;
  tab_title: string;
  mutable tab_header: tab_row list;
  mutable tab_rows: tab_row list;
}
and tab_row = {
  row_cols: tab_col list;
}
and tab_col = {
  col_width: int;
  col_align: char;
  col_cont: string;
}

let format_table tab =
  let lines = ref [] in
  let pr line = lines := !lines @ [line] in
  let cn n c = 
    if n > 0 then String.make n c 
    else "" in
  let toff = if tab.tab_outline then 2 else 0 in
  let tw = tab.tab_width in 
  let colmarg = 1 in

  let fr tr =
    let col_str col str =
      let cw = col.col_width in
      let len = String.length str in
      let lf,rf =
        match col.col_align with
        | 'l' -> colmarg,(cw-len-colmarg)
        | 'r' -> (cw-len-colmarg),colmarg
        | 'c' -> 
          let n' = cw-len in
          let s1 = n' / 2 in
          let s2 = n' - s1 in
          s1,s2
        | _ -> 0,0 in 
      let str' = Str.global_replace (Str.regexp "\n") " " str in
      sprintf "%s%s%s"
              (cn lf ' ')
              str'
              (cn rf ' ') in
      
    let max_lines = ref 1 in
    let cols = List.map (fun col ->
      let cw = col.col_width-colmarg*2 in
      let l = String.length col.col_cont in
      if l >= cw then
      begin
        let tokens = Str.split (Str.regexp " ") col.col_cont in
        let line' = ref "" in
        let lines' = ref [] in
        List.iter (fun token ->
          let l' = String.length !line' in
          let l'' = String.length token in
          if (l' + l'') >= cw then
          begin
            lines' := !lines' @ [col_str col !line'];
            line' := token;
          end
          else
            line' := if !line' = "" then token else sprintf "%s %s" !line' token; 
          ) tokens;
        if !line' <> "" then lines' := !lines' @ [col_str col !line'];  
        max_lines := max !max_lines (List.length !lines');        
        !lines';
      end
      else
      begin
        [col_str col col.col_cont]
      end;
      ) tr.row_cols in
    let lines = Array.create !max_lines "" in
    let cur_col = ref 0 in
    List.iter (fun coll ->
      let col = List.nth tr.row_cols !cur_col in
      let cw = col.col_width-colmarg*2 in
      let n = List.length coll in
      for i = 0 to n-1 do
        lines.(i) <- sprintf "%s%s" lines.(i) (List.nth coll i) 
      done;
      if n < !max_lines then
        for i = n to !max_lines-1 do
          lines.(i) <- sprintf "%s%s" lines.(i) (col_str col (cn cw ' '))
        done;
      ) cols;
    let lines = Array.to_list lines in
    if tab.tab_outline then List.map (fun l -> sprintf "|%s|" l) lines
    else lines in
    
  if tab.tab_title <> "" then
  begin
    let l = String.length tab.tab_title in
    let n' = tw-l-2 in
    let s1 = n' / 2 in
    let s2 = n' - s1 in
    pr (sprintf "%s%s %s %s%s"
                (if tab.tab_outline then "+" else "")
                (cn s1 '-')
                tab.tab_title
                (cn s2 '-')
                (if tab.tab_outline then "+" else ""));
  end
  else 
    pr (sprintf "-%s-" (cn tw '-'));  
  if tab.tab_header <> [] && tab.tab_title <> "" then
    pr (sprintf "+%s+" (cn tw '-'));  
  List.iter (fun tr -> lines := !lines @ (fr tr)) tab.tab_header;
  if tab.tab_header <> [] then
    pr (sprintf "+%s+" (cn tw '-'));  
  List.iter (fun tr -> lines := !lines @ (fr tr)) tab.tab_rows;
  
  if tab.tab_outline then
    pr (sprintf "+%s+" (cn tw '-'));  
  !lines
