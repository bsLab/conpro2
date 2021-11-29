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
**    $CREATED:     10.10.2007
**    $VERSION:     2.25
**
**    $INFO:
**
** Data management (microcode,vhdl): Transformation and conversion rules. 
** VHDL type conversion tables, splitted for LHS and RHS objects.
**
**
**    $ENDOFINFO
**
*)

open Unix
open Printf

open Cp_syntax
open Cp_types
open Cp_common
open Cp_symbol
open Cp_utils
open Cp_data_core
open Cp_print
open Cp_printtypes
open Cp_data_trans

           
(*
** RHS type conversion rules: DT PT ET ST NT CT -> 
**  (fun signal string -> vhdl conversion descriptor) 
*)
let _vhdl_rhs_rules dt pt et st nt ct range index to_ot = 
  let desc_dt dt = Some (type_name dt,dt) in
  let sdesc_dt range index dt = 
    match range with
    | Some (a,b) -> 
      Some ((sprintf "%s_R%d_%d" (type_name dt) a b),dt);
    | None ->
    begin
      (*
      ** Dynamic bit selector
      *)
      match index with
      | Some (UA_data da) ->
      begin
        match dt with
        | DT_logic n -> 
          let sel = 
            match da with
            | UC_reg uo -> uo.uo_name 
            | UC_sig uo -> uo.uo_name 
            | UC_temp ut -> ut.ut_name 
            | _ -> "?" 
            in
          Some ((sprintf "%s_S%s" (type_name dt) sel),dt)
        | _ -> error 0 "Bitselection type not supported.";
      end;
      | _ -> Some ((type_name dt),dt)
    end in
  let is_expr str =
    let expr = ref false in
    String.iter (fun c ->
      match c with
      | 'a' .. 'z' 
      | 'A' .. 'Z' 
      | '0' .. '9' 
      | '_' -> expr := !expr;
      | _ -> expr := true;) str;
    !expr in
[
   (*
   ** DT,ET=SOME
   ** PT,ST,CT=NONE
   *)
   (T_bool,None,T_bool,None,false,None),(fun s -> {vdc_sig=s;vdc_mask="";vdc_desc=None});
   (T_logic,None,T_logic,None,false,None),(fun s -> 
                                      if (size_of_dt dt) <> (size_of_dt et) then
                                      begin
                                        let s',_ = vhdl_conv_dt s dt et in
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None} 
                                      end else {vdc_sig=s;vdc_mask="";vdc_desc=None});
   (T_logic,None,T_logic,None,true,None),(fun s -> 
                                      if dt <> et then
                                      begin
                                        let s',_ = vhdl_conv_dt s dt et in 
                                        {vdc_sig=vhdl_conv_neg dt nt s';vdc_mask="";vdc_desc=None} 
                                      end else {vdc_sig=vhdl_conv_neg dt nt s;vdc_mask="";vdc_desc=None});
   (T_logic,None,T_int,None,false,None),(fun s -> 
                                      let s',_ = vhdl_conv_dt s dt et in 
                                      {vdc_sig=s';vdc_mask="";vdc_desc=None} );
   (T_logic,None,T_natural,None,false,None),(fun s -> 
                                      let s',_ = vhdl_conv_dt s dt et in 
                                      {vdc_sig=s';vdc_mask="";vdc_desc=None});
   (T_int,None,T_natural,None,false,None),(fun s -> 
                                      let s',_ = vhdl_conv_dt s dt et in 
                                      let cn = desc_dt et in 
                                      {vdc_sig=s';vdc_mask="";vdc_desc=cn});
   (T_int,None,T_int,None,false,None),(fun s -> 
                                      if dt <> et then
                                      begin
                                        let s',_ = vhdl_conv_dt s dt et in 
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None} 
                                      end else {vdc_sig=s;vdc_mask="";vdc_desc=None});
   (T_int,None,T_int,None,true,None),(fun s -> 
                                      if dt <> et then
                                      begin
                                        let s',_ = vhdl_conv_dt s dt et in 
                                        {vdc_sig=vhdl_conv_neg dt nt s';vdc_mask="";vdc_desc=None} 
                                      end else {vdc_sig=vhdl_conv_neg dt nt s;vdc_mask="";vdc_desc=None});
   (T_int,None,T_logic,None,false,None),(fun s ->  
                                      let s',_ = vhdl_conv_dt s dt et in 
                                      {vdc_sig=s';vdc_mask="";vdc_desc=None});

   (*
   ** DT,PT,ET=SOME
   ** ST,CT=NONE
   *)
   (T_bool,Some T_logic,T_logic,None,false,None),(fun s ->
                                      if (size_of_dt dt) <> (size_of_dt et) then
                                      begin
                                        let s',_ = vhdl_conv_dt s dt et in
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None} 
                                      end else {vdc_sig=s;vdc_mask="";vdc_desc=None});
   (T_bool,Some T_logic,T_bool,None,false,None),(fun s ->
                                      let pt = get_some pt in
                                      let ct = if dt <> pt then dt else et in 
                                      if pt <> ct then
                                      begin
                                        let s',_ = vhdl_conv_dt s pt ct in
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None}
                                      end 
                                      else {vdc_sig=s;vdc_mask="";vdc_desc=None});
   (T_bool,Some T_bool,T_bool,None,false,None),(fun s ->
                                      {vdc_sig=s;vdc_mask="";vdc_desc=None});
   (T_bool,Some T_logic,T_bool,None,true,None),(fun s ->
                                      {vdc_sig=vhdl_conv_neg dt nt s;vdc_mask="";vdc_desc=None});
   (T_logic,Some T_logic,T_logic,None,false,None),(fun s -> 
                                      let pt = get_some pt in
                                      let ct =  et in 
                                      if pt <> ct then
                                      begin
                                        let s',_ = vhdl_conv_dt s pt ct in
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None}
                                      end 
                                      else {vdc_sig=s;vdc_mask="";vdc_desc=None});
   (T_int,Some T_int,T_int,None,false,None),(fun s -> 
                                      let pt = get_some pt in
                                      let ct = if dt <> pt then dt else et in 
                                      if pt <> ct then
                                      begin
                                        let s',_ = vhdl_conv_dt s pt ct in 
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None} 
                                      end 
                                      else {vdc_sig=s;vdc_mask="";vdc_desc=None});
   (T_int,Some T_int,T_logic,None,false,None),(fun s -> 
                                      let pt = get_some pt in
                                      let ct = if dt <> pt then dt else et in 
                                      if pt <> ct then
                                      begin
                                        let s',_ = vhdl_conv_dt s pt ct in 
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None} 
                                      end 
                                      else {vdc_sig=s;vdc_mask="";vdc_desc=None});
   (T_int,Some T_int,T_int,None,true,None),(fun s -> 
                                      if dt <> et then
                                      begin
                                        let s',_ = vhdl_conv_dt s dt et in 
                                        {vdc_sig=vhdl_conv_neg dt nt s';vdc_mask="";vdc_desc=None} 
                                      end else {vdc_sig=vhdl_conv_neg dt nt s;vdc_mask="";vdc_desc=None});
   (T_int,Some T_logic,T_int,None,false,None),(fun s ->  
                                      let pt = get_some pt in
                                      let s',_ = vhdl_conv_dt s pt et in
                                      let cn = desc_dt et in 
                                      {vdc_sig=s';vdc_mask="";vdc_desc=cn});
   (T_char,Some T_logic,T_logic,None,false,None),(fun s ->
                                      let pt = get_some pt in
                                      let ct = if dt <> pt then dt else et in 
                                      if pt <> ct then
                                      begin
                                        let s',_ = vhdl_conv_dt s pt ct in
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None} 
                                      end 
                                      else {vdc_sig=s;vdc_mask="";vdc_desc=None});
   (T_char,Some T_char,T_logic,None,false,None),(fun s ->
                                      let pt = get_some pt in
                                      let ct = if dt <> pt then dt else et in 
                                      if pt <> ct then
                                      begin
                                        let s',_ = vhdl_conv_dt s pt ct in
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None} 
                                      end 
                                      else {vdc_sig=s;vdc_mask="";vdc_desc=None});
   (T_char,Some T_logic,T_char,None,false,None),(fun s ->
                                      let pt = get_some pt in
                                      let ct = if dt <> pt then dt else et in 
                                      if pt <> ct then
                                      begin
                                        let s',_ = vhdl_conv_dt s pt ct in
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None}
                                      end 
                                      else {vdc_sig=s;vdc_mask="";vdc_desc=None});
                                       
   (T_char,Some T_char,T_char,None,false,None),(fun s -> 
                                      let pt = get_some pt in
                                      let ct =  et in 
                                      if pt <> ct then
                                      begin
                                        let s',_ = vhdl_conv_dt s pt ct in
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None}
                                      end 
                                      else {vdc_sig=s;vdc_mask="";vdc_desc=None});
   (T_int,Some T_logic,T_int,None,true,None),(fun s -> 
                                      let pt = get_some pt in
                                      let s',_ = vhdl_conv_dt s pt et in 
                                      {vdc_sig=vhdl_conv_neg dt nt s';vdc_mask="";vdc_desc=None});
   (*
   ** DT,ET,CT=SOME
   ** ST,PT=NONE
   *)
   (T_logic,None,T_char,None,false,Some T_char),(fun s ->
                                      let ct = 
                                        let ct = get_some ct in 
                                        if (size_of_dt ct) > 0 then ct else et in
                                      if (size_of_dt dt) <> (size_of_dt ct) then
                                      begin
                                        let s',_ = vhdl_conv_dt s dt ct in
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None}
                                      end else {vdc_sig=s;vdc_mask="";vdc_desc=None});
   (T_logic,None,T_logic,None,false,Some T_char),(fun s ->
                                      let ct = 
                                        let ct = get_some ct in 
                                        if (size_of_dt ct) > 0 then ct else et in
                                      if (size_of_dt dt) <> (size_of_dt ct) then
                                      begin
                                        let s',_ = vhdl_conv_dt s dt ct in
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None}
                                      end else {vdc_sig=s;vdc_mask="";vdc_desc=None});
   (T_logic,None,T_char,None,false,Some T_logic),(fun s ->
                                      let ct = 
                                        let ct = get_some ct in 
                                        if (size_of_dt ct) > 0 then ct else et in
                                      if (size_of_dt dt) <> (size_of_dt ct) then
                                      begin
                                        let s',_ = vhdl_conv_dt s dt ct in
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None}
                                      end else {vdc_sig=s;vdc_mask="";vdc_desc=None});
   (T_logic,None,T_logic,None,false,Some T_logic),(fun s -> 
                                      let ct = 
                                        let ct = get_some ct in 
                                        if (size_of_dt ct) > 0 then ct else et in
                                      if dt <> ct then
                                      begin
                                        let s',_ = vhdl_conv_dt s dt ct in
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None} 
                                      end 
                                      else {vdc_sig=s;vdc_mask="";vdc_desc=None});
   (T_logic,None,T_int,None,false,Some T_logic),(fun s -> 
                                      let ct = 
                                        let ct = get_some ct in 
                                        if (size_of_dt ct) > 0 then ct else et in
                                      if dt <> ct then
                                      begin
                                        let s',_ = vhdl_conv_dt s dt ct in
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None} 
                                      end
                                      else {vdc_sig=s;vdc_mask="";vdc_desc=None});
   (T_logic,None,T_int,None,false,Some T_int),(fun s -> 
                                      let ct = 
                                        let ct = get_some ct in 
                                        if (size_of_dt ct) > 0 then ct else et in
                                      if dt <> ct then
                                      begin
                                        let s',_ = vhdl_conv_dt s dt ct in
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None} 
                                      end
                                      else {vdc_sig=s;vdc_mask="";vdc_desc=None});
   (T_char,None,T_char,None,false,Some T_logic),(fun s ->
                                      let ct = 
                                        let ct = get_some ct in 
                                        if (size_of_dt ct) > 0 then ct else et in
                                      if (size_of_dt dt) <> (size_of_dt ct) then
                                      begin
                                        let s',_ = vhdl_conv_dt s dt ct in
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None}
                                      end else {vdc_sig=s;vdc_mask="";vdc_desc=None});

   (T_int,None,T_int,None,false,Some T_int),(fun s -> 
                                      let ct = 
                                        let ct = get_some ct in 
                                        if (size_of_dt ct) > 0 then ct else et in
                                      if dt <> ct then
                                      begin
                                        let s',_ = vhdl_conv_dt s dt ct in
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None} 
                                      end else {vdc_sig=s;vdc_mask="";vdc_desc=None});
   (T_int,None,T_int,None,false,Some T_natural),(fun s -> 
                                      let ct = get_some ct in
                                      if dt <> ct then
                                      begin
                                        let s',_ = vhdl_conv_dt s dt ct in
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None} 
                                      end else {vdc_sig=s;vdc_mask="";vdc_desc=None});
   (T_int,None,T_int,None,false,Some T_logic),(fun s -> 
                                      let ct = get_some ct in 
                                      let s',_ = vhdl_conv_dt s dt ct in 
                                      {vdc_sig=s';vdc_mask="";vdc_desc=None} );
   (T_int,None,T_logic,None,false,Some T_logic),(fun s -> 
                                      let ct = 
                                        let ct = get_some ct in 
                                        if (size_of_dt ct) > 0 then ct else et in
                                      if dt <> ct then
                                      begin
                                        let s',_ = vhdl_conv_dt s dt ct in
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None} 
                                      end 
                                      else {vdc_sig=s;vdc_mask="";vdc_desc=None});
   (T_int,None,T_logic,None,true,Some T_logic),(fun s -> 
                                      let ct = 
                                        let ct = get_some ct in 
                                        if (size_of_dt ct) > 0 then ct else et in
                                      let s' = vhdl_conv_neg dt nt s in
                                      if dt <> ct then
                                      begin
                                        let s'',_ = vhdl_conv_dt s' dt ct in
                                        {vdc_sig=s'';vdc_mask="";vdc_desc=None} 
                                      end 
                                      else {vdc_sig=s';vdc_mask="";vdc_desc=None});
   
   (*
   ** DT,PT,ET,CT=SOME
   ** ST=NONE
   *)
  
   (T_bool,Some T_logic,T_logic,None,false,Some T_logic),(fun s ->
                                      let ct = get_some ct in 
                                      let s',_ = vhdl_conv_dt s dt ct in
                                      {vdc_sig=s';vdc_mask="";vdc_desc=None}); 
   (T_bool,Some T_logic,T_bool,None,false,Some T_logic),(fun s ->
                                      let ct = get_some ct in 
                                      let s',_ = vhdl_conv_dt s dt ct in
                                      {vdc_sig=s';vdc_mask="";vdc_desc=None}); 
   (T_bool,Some T_logic,T_bool,None,false,Some T_bool),(fun s ->
                                      (*
                                      ** Special case: forced boolean evaluation used in boolean expressions, for example
                                      ** s <= b1 or b2 
                                      **  =>
                                      ** s <= (b1 = '1') or (b2 = '1')
                                      *)
                                      {vdc_sig=sprintf "(%s = '1')" s;vdc_mask="";vdc_desc=None}); 
   (T_logic,Some T_logic,T_int,None,false,Some T_int),(fun s -> 
                                      let ct = 
                                        let ct = get_some ct in 
                                        if (size_of_dt ct) > 0 then ct else et in
                                      if dt <> ct then
                                      begin
                                        let s',_ = vhdl_conv_dt s dt ct in 
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None} 
                                      end 
                                      else {vdc_sig=s;vdc_mask="";vdc_desc=None});
   (T_logic,Some T_logic,T_logic,None,false,Some T_int),(fun s -> 
                                      let ct = 
                                        let ct = get_some ct in 
                                        if (size_of_dt ct) > 0 then ct else et in
                                      if dt <> ct then
                                      begin
                                        let s',_ = vhdl_conv_dt s dt ct in
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None} 
                                      end 
                                      else {vdc_sig=s;vdc_mask="";vdc_desc=None});
   (T_char,Some T_logic,T_char,None,false,Some T_logic),(fun s ->
                                      let ct = 
                                        let ct = get_some ct in 
                                        if (size_of_dt ct) > 0 then ct else et in
                                      if (size_of_dt dt) <> (size_of_dt ct) then
                                      begin
                                        let s',_ = vhdl_conv_dt s dt ct in
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None} 
                                      end else                                       
                                        {vdc_sig=s;vdc_mask="";vdc_desc=None});
   (T_char,Some T_char,T_logic,None,false,Some T_logic),(fun s ->
                                      let ct = 
                                        let ct = get_some ct in 
                                        if (size_of_dt ct) > 0 then ct else et in
                                      if (size_of_dt dt) <> (size_of_dt ct) then
                                      begin
                                        let s',_ = vhdl_conv_dt s dt ct in
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None} 
                                      end else                                       
                                        {vdc_sig=s;vdc_mask="";vdc_desc=None});
   (T_char,Some T_logic,T_logic,None,false,Some T_logic),(fun s ->
                                      let ct = 
                                        let ct = get_some ct in 
                                        if (size_of_dt ct) > 0 then ct else et in
                                      if (size_of_dt dt) <> (size_of_dt ct) then
                                      begin
                                        let s',_ = vhdl_conv_dt s dt ct in
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None} 
                                      end else {vdc_sig=s;vdc_mask="";vdc_desc=None});
                                     
   (T_char,Some T_logic,T_int,None,false,Some T_int),(fun s -> 
                                      let ct = 
                                        let ct = get_some ct in 
                                        if (size_of_dt ct) > 0 then ct else et in
                                      if dt <> ct then
                                      begin
                                        let s',_ = vhdl_conv_dt s dt ct in 
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None} 
                                      end 
                                      else {vdc_sig=s;vdc_mask="";vdc_desc=None});
   (T_int,Some T_int,T_int,None,false,Some T_logic),(fun s -> 
                                      let ct = 
                                        let ct = get_some ct in 
                                        if (size_of_dt ct) > 0 then ct else et in
                                      if dt <> ct then
                                      begin
                                        let s',_ = vhdl_conv_dt s dt ct in
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None} 
                                      end 
                                      else {vdc_sig=s;vdc_mask="";vdc_desc=None});
   (T_int,Some T_logic,T_logic,None,false,Some T_logic),(fun s -> 
                                      let ct = 
                                        let ct = get_some ct in 
                                        if (size_of_dt ct) > 0 then ct else et in
                                      let pt = get_some pt in
                                      if pt <> ct then
                                      begin
                                        let s',_ = vhdl_conv_dt s pt ct in
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None} 
                                      end 
                                      else {vdc_sig=s;vdc_mask="";vdc_desc=None});
                                      
   (*
   ** ST=SOME
   *)                                    
   (T_logic,None,T_logic,Some T_logic,false,None),(fun s -> 
                                      let s',dt' = vhdl_conv_range range index s dt in
                                      if dt' <> et then
                                      begin
                                        if (size_of_dt dt') > (size_of_dt et) then
                                          error 0 (sprintf "Range width larger than expression width!");
                                        let s'',dt'' =  vhdl_conv_dt s' dt' et in
                                        {vdc_sig=s'';vdc_mask="";vdc_desc=None}
                                      end
                                      else
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None});
   (T_logic,None,T_logic,Some T_logic,true,None),(fun s -> 
                                      let s',dt' = vhdl_conv_range range index s dt in
                                      let s' = vhdl_conv_neg dt nt s' in
                                      if dt' <> et then
                                      begin
                                        if (size_of_dt dt') > (size_of_dt et) then
                                          error 0 (sprintf "Range width larger than expression width!");
                                        let s'',dt'' =  vhdl_conv_dt s' dt' et in
                                        {vdc_sig=s'';vdc_mask="";vdc_desc=None}
                                      end
                                      else
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None});
   (T_logic,Some T_logic,T_logic,Some T_logic,false,None),(fun s -> 
                                      let s',dt' = vhdl_conv_range range index s dt in
                                      if dt' <> et then
                                      begin
                                        if (size_of_dt dt') > (size_of_dt et) then
                                          error 0 (sprintf "Range width larger than expression width!");
                                        let s'',dt'' =  vhdl_conv_dt s' dt' et in
                                        {vdc_sig=s'';vdc_mask="";vdc_desc=None}
                                      end
                                      else
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None});
   (T_logic,None,T_logic,Some T_logic,false,Some T_logic),(fun s -> 
                                      let ct = 
                                        let ct = get_some ct in 
                                        if (size_of_dt ct) > 0 then ct else et in
                                      let s',dt' = vhdl_conv_range range index s dt in
                                      if dt' <> ct then
                                      begin
                                        if (size_of_dt dt') > (size_of_dt ct) then
                                          error 0 (sprintf "Range width larger than expression width!");
                                        let s'',dt'' =  vhdl_conv_dt s' dt' ct in
                                        {vdc_sig=s'';vdc_mask="";vdc_desc=None}
                                      end
                                      else
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None});
   (T_logic,None,T_bool,Some T_logic,false,Some T_logic),(fun s -> 
                                      let ct = 
                                        let ct = get_some ct in 
                                        if (size_of_dt ct) > 0 then ct else et in
                                      let s',dt' = vhdl_conv_range range index s dt in
                                      if dt' <> ct then
                                      begin
                                        if (size_of_dt dt') > (size_of_dt ct) then
                                          error 0 (sprintf "Range width larger than expression width!");
                                        let s'',dt'' =  vhdl_conv_dt s' dt' ct in
                                        {vdc_sig=s'';vdc_mask="";vdc_desc=None}
                                      end
                                      else
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None});
   (T_logic,None,T_int,Some T_logic,false,Some T_int),(fun s -> 
                                      let s',dt' = vhdl_conv_range range index s dt in
                                      if dt' <> et then
                                      begin
                                        let s'',dt'' =  vhdl_conv_dt s' dt' et in
                                        {vdc_sig=s'';vdc_mask="";vdc_desc=None}
                                      end
                                      else
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None});
 
   (T_logic,Some T_logic,T_int,Some T_logic,false,Some T_int),(fun s -> 
                                      let ct = 
                                        let ct = get_some ct in 
                                        if (size_of_dt ct) > (size_of_dt et) then ct else et in
                                      let s',dt' = vhdl_conv_range range index s dt in
                                      if dt' <> et then
                                      begin
                                        if (size_of_dt dt') > (size_of_dt ct) then
                                          error 0 (sprintf "Range width (%d) larger than expression width (%d)!"
                                                            (size_of_dt dt')  (size_of_dt ct));
                                        let s'',dt'' =  vhdl_conv_dt s' dt' ct in
                                        {vdc_sig=s'';vdc_mask="";vdc_desc=None}
                                      end
                                      else
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None});
 
    (T_int,None,T_int,Some T_int,false,None),(fun s -> 
                                      let s',dt' = vhdl_conv_range range index s dt in
                                      if dt' <> et then
                                      begin
                                        if (size_of_dt dt') > (size_of_dt et) then
                                          error 0 (sprintf "Range width larger than expression width!");
                                        let s'',dt'' =  vhdl_conv_dt s' dt' et in
                                        {vdc_sig=s'';vdc_mask="";vdc_desc=None}
                                      end
                                      else
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None});
   (T_int,None,T_logic,Some T_int,false,None),(fun s -> 
                                      let s',dt' = vhdl_conv_range range index s dt in 
                                      let s'',dt'' = vhdl_conv_dt s' dt et in
                                      {vdc_sig=s'';vdc_mask="";vdc_desc=None});
   (T_int,None,T_logic,Some T_int,false,Some T_logic),(fun s -> 
                                      let ct = 
                                        let ct = get_some ct in 
                                        if (size_of_dt ct) > 0 then ct else et in
                                      let s',dt' = vhdl_conv_range range index s dt in 
                                      if dt' <> ct then
                                      begin
                                        let s'',dt'' = vhdl_conv_dt s' dt' ct in
                                        {vdc_sig=s'';vdc_mask="";vdc_desc=None}
                                      end
                                      else {vdc_sig=s';vdc_mask="";vdc_desc=None});
   (T_logic,None,T_int,Some T_logic,false,Some T_logic),(fun s -> 
                                      let ct = 
                                        let ct = get_some ct in 
                                        if (size_of_dt ct) > 0 then ct else et in
     
                                      let s',dt' = vhdl_conv_range range index s dt in
                                      if dt' <> ct then
                                      begin
                                        let s'',dt'' =  vhdl_conv_dt s' dt' et in
                                        let s''',dt''' = vhdl_conv_dt s'' dt'' ct in
                                        {vdc_sig=s''';vdc_mask="";vdc_desc=None}
                                      end
                                      else
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None});

   (T_logic,None,T_bool,Some T_logic,false,Some T_bool),(fun s -> 
                                      let s',dt' = vhdl_conv_range range index s dt in
                                      if dt' <> et then
                                      begin
                                        if (size_of_dt dt') > (size_of_dt et) then
                                          error 0 (sprintf "Range width larger than expression width!");
                                        let s'',dt'' =  vhdl_conv_dt s' dt' et in
                                        {vdc_sig=s'';vdc_mask="";vdc_desc=None}
                                      end
                                      else
                                        {vdc_sig=s';vdc_mask="";vdc_desc=None});
  (*
  ** Special objects....
  *)
  (T_object,None,T_object,None,false,None),(fun s -> 
    {vdc_sig=s;vdc_mask="%s";vdc_desc=None});
  ] 
  
(*
** LHS type conversion rules: DT PT ET ST NT CT -> 
**  (fun signal string -> vhdl conversion descriptor) 
*)
let _vhdl_lhs_rules dt pt et st nt ct range index to_ot = 
  let resize_dt dt1 dtn =
    let td1 = td_of_dt dt1 in
    let size = size_of_dt dtn in
    dt_of_td td1 size in
    
[
   (*
   ** DT,ET=SOME
   ** PT,ST,CT=NONE
   *)
   (T_logic,None,T_logic,None,false,None),(fun s -> 
                                      if to_ot = O_reg && dt <> et then
                                      begin
                                        let n = size_of_dt et in
                                        let n' = size_of_dt dt in
                                        let vdc_mask =
                                            if n' > n && n > 1 
                                            then sprintf "Ln_to_Lm((%%s),%d,%d)" n n'
                                            else if n' > n 
                                            then sprintf "Ln_1_to_Lm((%%s),%d,%d)" n n'
                                            else if n > n' && n' > 1 
                                            then sprintf "Lm_to_Ln((%%s),%d,%d)" n n'
                                            else if n > n' 
                                            then sprintf "Lm_to_Ln_1((%%s),%d,%d)" n n'
                                            else "%s" in
                                        {vdc_sig=s;vdc_mask=vdc_mask;vdc_desc=None}
                                      end 
                                      else
                                        {vdc_sig=s;vdc_mask="%s";vdc_desc=None});
   (T_logic,None,T_int,None,false,None),(fun s -> 
                                      {vdc_sig=s;vdc_mask="%s";vdc_desc=None});

   (T_int,None,T_int,None,false,None),(fun s -> 
                                      let n = size_of_dt et in
                                      let n' = size_of_dt dt in
                                      let vdc_mask =
                                          if n' > n 
                                          then sprintf "In_to_Im((%%s),%d,%d)" n n'
                                          else if n > n'
                                          then sprintf "Im_to_In((%%s),%d,%d)" n n'
                                          else "%s" in
                                      {vdc_sig=s;vdc_mask=vdc_mask;vdc_desc=None});
   (T_int,None,T_logic,None,false,None),(fun s ->  
                                      {vdc_sig=s;vdc_mask="%s";vdc_desc=None});
                                      
   (*
   ** DT,PT,ET=SOME
   ** ST,CT=NONE
   *)
   (T_bool,Some T_logic ,T_bool,None,false,None),(fun s -> 
        let pt = get_some pt in
        let n' = size_of_dt pt in
        let vdc_mask = 
          if n' <> 1 then sprintf "(%s & %%s)"  (val_str (DT_logic (n'-1)) (V_int Int64.zero))
          else "%s" in
        {vdc_sig=s;vdc_mask=vdc_mask;vdc_desc=None});
   (T_logic,Some T_logic,T_logic,None,false,None),(fun s -> 
                                      let pt = get_some pt in
                                      let n = size_of_dt et in
                                      let n' = size_of_dt pt in
                                      let vdc_mask =
                                          if n' > n && n > 1 
                                          then sprintf "Ln_to_Lm((%%s),%d,%d)" n n'
                                          else if n' > n 
                                          then sprintf "Ln_1_to_Lm((%%s),%d,%d)" n n'
                                          else if n > n' && n' > 1 
                                          then sprintf "Lm_to_Ln((%%s),%d,%d)" n n'
                                          else if n > n' 
                                          then sprintf "Lm_to_Ln_1((%%s),%d,%d)" n n'
                                          else "%s" in
                                      {vdc_sig=s;vdc_mask=vdc_mask;vdc_desc=None});
   (T_int,Some T_int,T_int,None,false,None),(fun s -> 
                                      let pt = get_some pt in
                                      let s'' =
                                        if pt <> et then
                                        begin
                                            let s'',_ = vhdl_conv_dt "%s" et pt in 
                                            s''
                                        end else "%s" in
                                     {vdc_sig=s;vdc_mask=s'';vdc_desc=None});
   (T_int,Some T_logic,T_int,None,false,None),(fun s -> 
                                      let pt = get_some pt in
                                      let n = size_of_dt et in
                                      let n' = size_of_dt pt in
                                      let vdc_mask =
                                          if n' > n && n > 1 
                                          then sprintf "In_to_Lm((%%s),%d,%d)" n n'
                                          else if n' > n 
                                          then sprintf "In_1_to_Lm((%%s),%d,%d)" n n'
                                          else if n > n' && n' > 1 
                                          then sprintf "Im_to_Ln((%%s),%d,%d)" n n'
                                          else if n > n' 
                                          then sprintf "Im_to_Ln_1((%%s),%d,%d)" n n'
                                          else "I_to_L(%s)" in
                                      {vdc_sig=s;vdc_mask=vdc_mask;vdc_desc=None});
   (T_int,Some T_logic,T_logic,None,false,None),(fun s -> 
                                      {vdc_sig=s;vdc_mask="%s";vdc_desc=None});
   (T_char,Some T_logic,T_char,None,false,None),(fun s -> 
                                       {vdc_sig=s;vdc_mask="%s";vdc_desc=None});
   (T_char,Some T_logic,T_logic,None,false,None),(fun s -> 
                                       {vdc_sig=s;vdc_mask="%s";vdc_desc=None});

   (*
   ** DT,ET,CT=SOME
   ** ST,PT=NONE
   *)
   (T_logic,None,T_logic,None,false,Some T_logic),(fun s -> 
                                      let ct = resize_dt (get_some ct) et in 
                                      let s'',_ = vhdl_conv_dt "%s" ct dt in 
                                      {vdc_sig=s;vdc_mask=s'';vdc_desc=None});
   (T_logic,None,T_int,None,false,Some T_logic),(fun s -> 
                                      let ct = resize_dt (get_some ct) et in 
                                      let s'',_ = vhdl_conv_dt "%s" ct dt in 
                                      {vdc_sig=s;vdc_mask=s'';vdc_desc=None});
   (T_logic,None,T_int,None,false,Some T_int),(fun s -> 
                                      let ct = resize_dt (get_some ct) et in 
                                      let s'',_ = vhdl_conv_dt "%s" ct dt in 
                                      {vdc_sig=s;vdc_mask=s'';vdc_desc=None});
   (T_int,None,T_logic,None,false,Some T_logic),(fun s -> 
                                      let ct = resize_dt (get_some ct) et in 
                                      let s'',_ = vhdl_conv_dt "%s" ct dt in 
                                      {vdc_sig=s;vdc_mask=s'';vdc_desc=None});

   (*
   ** DT,PT,ET,CT=SOME
   ** ST=NONE
   *)

   (T_bool,Some T_logic,T_bool,None,false,Some T_logic),(fun s -> 
        {vdc_sig=s;
         vdc_mask= sprintf "if (%%s) then %s <= '1'; else %s <= '0'; end if;" s s;
         vdc_desc=None});
   (T_logic,Some T_logic,T_logic,None,false,Some T_logic),(fun s -> 
                                      let pt = get_some pt in 
                                      let ct = resize_dt (get_some ct) et in 
                                      let s'',_ = 
                                        if dt <> ct then vhdl_conv_dt "%s" dt ct else
                                        if et <> ct then vhdl_conv_dt "%s" et ct else 
                                        if pt <> ct then vhdl_conv_dt "%s" pt ct else 
                                        "%s",DT_bool in 
                                      {vdc_sig=s;vdc_mask=s'';vdc_desc=None});
   (T_int,Some T_logic,T_int,None,false,Some T_logic),(fun s -> 
                                      (* let ct = resize_dt (get_some ct) et in *)
                                      let ct = get_some ct in
                                      let s'',_ = vhdl_conv_dt "%s" dt ct in 
                                      {vdc_sig=s;vdc_mask=s'';vdc_desc=None});
   (T_char,Some T_logic,T_char,None,false,Some T_logic),(fun s -> 
                                      let ct = resize_dt (get_some ct) et in 
                                      let s'',_ = vhdl_conv_dt "%s" dt ct in 
                                      {vdc_sig=s;vdc_mask=s'';vdc_desc=None});
                                      
   (*
   ** ST
   *)                                
   (T_logic,None,T_logic,Some T_logic,false,None),(fun s -> 
        match to_ot with
        | O_signal ->
        begin
          let s',dt' = vhdl_conv_range range index s dt in
          {vdc_sig=s';vdc_mask="%s";vdc_desc=None};
        end;
        | O_reg ->
        begin
          let s',dt' = vhdl_conv_lhs_range range index s dt in
          {vdc_sig=s;vdc_mask=s';vdc_desc=None};
        end;
        | _ ->
          {vdc_sig=s;vdc_mask="%s";vdc_desc=None});
   (T_int,None,T_logic,Some T_logic,false,None),(fun s -> 
        match to_ot with
        | O_reg ->
        begin
          let s',dt' = vhdl_conv_lhs_range range index s dt in
          {vdc_sig=s;vdc_mask=s';vdc_desc=None};
        end;
        | _ ->
          {vdc_sig=s;vdc_mask="%s";vdc_desc=None});
   (T_int,None,T_int,Some T_int,false,None),(fun s -> 
        match to_ot with
        | O_reg ->
        begin
          let s',dt' = vhdl_conv_lhs_range range index s dt in
          {vdc_sig=s;vdc_mask=s';vdc_desc=None};
        end;
        | _ ->
          {vdc_sig=s;vdc_mask="%s";vdc_desc=None});
                                      

  (*
  ** Special objects....
  *)
  (T_object,None,T_object,None,false,None),(fun s -> 
    {vdc_sig=s;vdc_mask="%s";vdc_desc=None});
  ] 
  
let init () =
  info ("Initializing VHDL transformation tables...");
  vhdl_rhs_rules := _vhdl_rhs_rules;
  vhdl_lhs_rules := _vhdl_lhs_rules;
    
