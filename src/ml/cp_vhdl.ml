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
**    $INITIAL:     (C) 2006-2008 BSSLAB
**    $CREATED:     15.10.2008
**    $VERSION:     2.04
**
**    $INFO:
**
**  CONPRO VHDL synthesis tool support. 
**         VHDL mapping.
**
**    $ENDOFINFO
**
*)

open Cp_version
open Cp_common
open Cp_syntax
open Cp_symbol
open Cp_types

(*
** VHDL operator and type mapping, depends on VHDL synthesis tool and user settings
*)
let vhdl_map name =
  try
  begin
    let n,v = List.find (fun (n,v) -> n = name) compiler.t_synth_tool.syn_vhdl_map in
    v
  end
  with Not_found -> name
