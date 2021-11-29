#include "com_objs.h"
int stat_led;
status_types sys_status;
status_types sys_status_next;
system_t* sys;
timer_t* watch_timer;
bool com_timeout;
uart_t* com;
queue_t* rx_q;
mutex_t* LOCK_FUN_request;
queue_t* tx_q;
struct dev_io_t dev_io;
event_t* rep;
int rep_d;
