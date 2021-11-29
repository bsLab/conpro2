#include "com.h"
#include "com_sys_status_proc.h"
#include "com_com_tmo.h"
#include "com_com_rx.h"
#include "com_com_tx.h"
#include "com_interpreter.h"
#include "com_FUN_request.h"
#include "com_main.h"
event_t *_exit;
int conpro(){
  process_init();
  _exit=event_new("");
  _exit->init(_exit);
  stat_led=0;
  sys_status=STATUS_OK;
  sys_status_next=STATUS_OK;
  sys = system_new("");
  watch_timer = timer_new("");
  com_timeout=false;
  com = uart_new("");
  rx_q = queue_new("");
  LOCK_FUN_request = mutex_new("");
  tx_q = queue_new("");
  dev_io.RX=0;
  dev_io.TX=0;
  rep = event_new("");
  rep_d=0;
  sys_status_proc=process_new("sys_status_proc",PRO_sys_status_proc,0);
  com_tmo=process_new("com_tmo",PRO_com_tmo,0);
  com_rx=process_new("com_rx",PRO_com_rx,0);
  com_tx=process_new("com_tx",PRO_com_tx,0);
  interpreter=process_new("interpreter",PRO_interpreter,0);
  Main=process_new("Main",PRO_Main,0);
  sys->clock(sys,18500000);
  sys->target(sys,"xc3s1000-ft256-5");
  sys->target(sys,"xc18v04");
  sys->reset_level(sys,0);
  watch_timer->time(watch_timer,50000);
  com->baud(com,115200);
  Main->call(Main);
}
