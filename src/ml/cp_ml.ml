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
**    $CREATED:     21.9.2009
**    $VERSION:     2.15
**
**    $INFO:
**
**  ML backend: high-level synthesis of ConPro module, process, and object model to 
**              ML functional language model
**
**    Different abstraction levels are available: high (default), mid, low (RTL level)
**
**  ===============
**  Synthesis RULES
**  ===============
**
**  1. Data types:
**
**      dt' = {dt: logic[n],int[n],char -> int | Int64.t}
**            {dt: bool -> bool}
**  
**  2. Register
**
**    ConPro: 
**      reg|var|sig r: dt
**
**    ML[high]: 
**      let r = ref def_val_of_dt 
**    ML[mid]: 
**      type 'a storage = { dt: data_type; mutable v: a'}
**      let r = { dt = dt; v = def_val_of_dt }
**
**      r.v <- r.v + 1
**  3. Structures
**
**    ConPro: 
**        type s: { e1: dt1; e2: dt2; ...}
**    ML[mid]: 
**        type s = { mutable e1': dt1'; ... }
**                 with e' = s_e
**
**  4. Arrays
**    ConPro:
**       array a: ot[N] of dt;
**    ML[mid]: 
**        type 'a storage = { dt: datatype; mutable v: a'}
**        let a = { dt = dt; v = Array.create N def_val_of_dt }
**
**  5. Structure Arrays
**    ConPro: 
**        type s: ....
**        array as: ot[N] of s
**    ML[mid]: 
**        type 'a storage = { dt: datatype; mutable v: a'}
**        let a = { dt = dt; v = Array.create N def_val_of_s }
**  
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

let ml_emit modu =
  let level = ML_level_high in
  match level with
  | ML_level_high -> Cp_ml_high.ml_emit modu
  | _ -> error 0 (sprintf "ml_emit: level not supported.")
  
