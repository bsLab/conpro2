#ifndef __com_objs
#define __com_objs
#include "com_types.h"
#include "com_const.h"
#include "com_models.h"
extern int stat_led;
extern status_types sys_status;
extern status_types sys_status_next;
extern system_t* sys;
extern timer_t* watch_timer;
extern bool com_timeout;
extern uart_t* com;
extern queue_t* rx_q;
extern mutex_t* LOCK_FUN_request;
extern queue_t* tx_q;
extern struct dev_io_t dev_io;
extern event_t* rep;
extern int rep_d;
#endif /* !__com_objs */
