open Core;
open Process;
open Mutex;
open Uart;
open Timer;

-- open Queue;

include "arith";
include "const";
include "common";
include "top";
include "interp";


process main:
begin
  tty_link.init ();
  -- tty_link.baud (38400);
  tty_link.start ();
  tty_tx.start ();
  tty_rx.start ();
  interp.start ();
end;
