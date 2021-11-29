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
**  C backend: high-level synthesis of ConPro module, process, and object model to 
**              C imperative language model
**
**    Different abstraction levels are available: high (default), mid, low (RTL level)
**
**  ===============
**  Synthesis RULES
**  ===============
**
**  1. Data types:
**
**      dt' = {dt: logic[n] -> unsigned long long,
**                 int[n] -> long int,
**                 char -> char }
**            {dt: bool -> int}
**  
**  2. Register
**
**    ConPro: 
**      reg|var|sig r: dt
**      r <- r + 1;
**
**    C[high]: 
**      dt' r = def_val_of_dt; 
**      r = r + 1;
**
**  3. Structures
**
**    ConPro: 
**        type s: { e1: dt1; e2: dt2; ...}
**        s.e1 <- s.e2;
**    C[high]: 
**        struct s_s { dt1': e1'; ... }; typedef struct s_s s;
**        s.e1 = s.e2;
**
**  4. Arrays
**    ConPro:
**        array a: ot[N] of dt;
**        a.[n] <- a.[n+1];
**    C[high]: 
**        dt' a[N];
**        a[n] = a[n+1];
**
**  5. Structure Arrays
**    ConPro: 
**        type s: ....
**        array as: ot[N] of s
**        a.[n].e1 <- a.[n+1].e1;
**    C[high]: 
**        s as[N];
**        a[n].e1 = a[n+1].e1;
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
open Cp_c_types

let c_emit modu =
  let level = C_level_high in
  match level with
  | C_level_high -> Cp_c_high.c_emit modu
  | _ -> error 0 (sprintf "c_emit: level not supported.")
