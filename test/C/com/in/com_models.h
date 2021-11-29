#ifndef __com_models
#define __com_models
#include "com_const.h"
#include "com_types.h"

#ifndef __CONPROH
#define __CONPROH
/*
******************************************
** CONPRO
******************************************
*/

#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <ucontext.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <termios.h>
#include <string.h>
#include <unistd.h>

extern int get_param(char* params,char *name);
extern int get_params(char* params,char *value, char *name);
extern void conpro_err(char *msg);
extern void mem_check(void *p,const char *msg);

typedef int bool;
#ifndef false
#define false 0
#endif
#ifndef true
#define true 1
#endif

#define int_of_char(x)  (int)(x)
#define char_of_int(x)  (char)(x)

#ifndef MAX
#define MAX(a,b)  (a>b?a:b)
#endif
#ifndef MIN
#define MIN(a,b)  (a<b?a:b)
#endif

#define ARCH_INT64_TYPE int64_t
#define ARCH_UINT64_TYPE uint64_t
#define ARCH_INT64_PRINTF_FORMAT "ll"

#ifdef ARCH_INT64_TYPE
/*
******************************************
** Int64
******************************************
*/

typedef ARCH_INT64_TYPE int64;
typedef ARCH_UINT64_TYPE uint64;

struct Int64_s {
  int64 zero;
  int64 one;
  int64 (*neg) ();
  int64 (*add) ();
  int64 (*sub) ();
  int64 (*mul) ();
  int64 (*div) ();
  int64 (*mod) ();
  int64 (*logand) ();
  int64 (*logor) ();
  int64 (*logxor) ();
  int64 (*lognot) ();
  int64 (*shift_left) ();
  int64 (*shift_right) ();
  int64 (*of_int) ();
  int (*to_int) ();
  char* (*to_string) ();
  int64 (*of_string) ();
};

typedef struct Int64_s Int64_t; 
extern Int64_t Int64;
extern int64 bit64(int a,int b,int64 x);
extern int64 bitw64(int a,int b,int64 x, int64 e);

#endif /* ARCH_INT64_TYPE */

#if !defined (NULL)
  #define NULL ((void *)0)
#endif


extern void mem_check(void *p,const char *msg);
extern double timeofday(void);

/*
** Exception handler
**
**  try(id)
**  {
**
**      .. raise(E);
**  };
**  with(id)
**  {
**      case E: ... break;
**  };
**  with_end(id);  
**  
**
*/
#ifndef __SUNPRO_C
     
#define try(id) process_table[process_running].catcher=&&try_with_label##id;\
            process_table[process_running].catcher_end=&&try_with_label_end##id;
#define with(id)  goto *(process_table[process_running].catcher_end); \
              try_with_label##id: switch (process_table[process_running].exception)
#define with_end(id)  try_with_label_end##id: \
                  process_table[process_running].catcher=(void **)0;\
                  process_table[process_running].catcher_end=(void **)0;
#define raise(e) process_raise(e); goto *(process_table[process_running].catcher);

#else /*__SUNPRO_C */

#define try(id) process_table[process_running].catcher=&&try_with_label/**/id;\
            process_table[process_running].catcher_end=&&try_with_label_end/**/id;
#define with(id)  goto *(process_table[process_running].catcher_end); \
              try_with_label/**/id: switch (process_table[process_running].exception)
#define with_end(id)  try_with_label_end/**/id: \
                  process_table[process_running].catcher=(void **)0;\
                  process_table[process_running].catcher_end=(void **)0;
#define raise(e) process_raise(e); goto *(process_table[process_running].catcher);

#endif 


#ifndef X
  #define X(a,b) {printf("[%s=%d]\n",a,b);fflush(stdout);}
#endif
#ifndef Xn
  #define Xn(a,s,b) {printf("[%s[%d]=%d]\n",a,s,b);fflush(stdout);}
#endif
#endif /* __CONPROH */

#ifndef __PROCH
#define __PROCH


/*
******************************************
** PROCESS
******************************************
*/

#define MAX_PROC  64
#define PROC_SS   1024*64

#define NILPID -1
#define NILFD -1
#define NILEXC -1

#define PRODBG  0
#define QDBG    0


enum process_states {
  PROC_FREE,
  PROC_READY,
  PROC_RUN,
  PROC_AWAIT,
  PROC_EXCEP,
  PROC_START,
  PROC_STOP  
};

struct process {
  /*
  ** Unique process identifer PID
  */
  int id;
  /*
  ** Descriptive name of process
  */
  char name[100];
  /*
  ** Actual process state (type process_states)
  */
  int state;  
  
  /*
  ** Pending exception signal?
  */
  int exception;
  /*
  ** try-with blocks
  **  catcher: start of with block (exception raised)
  **  catcher_leaver: point after with block (no exception raised)
  */
  
  void **catcher;
  void **catcher_end;
  
  /*
  ** Blockes processes (PROC_AWAIT) must
  ** have an interrupt handler which cleans
  ** up the blocked resource
  */
  void (*handler) (struct process *p,void *arg);
  /*
  ** Blocked resources passed to handler
  */
  void *arg;
  /*
  ** PROC_AWAIT on IO: read,write,execeptio
  */
  int read;
  int write;
  int excep;
  /*
  ** Process context, process function and optional argument 
  */
  ucontext_t context;
  void (*fun)(int arg);
  int fun_arg;
  void *stack;
  
  /*
  ** Linked list (queues)
  */
  struct process *next;
  /*
  ** PROC_AWAIT with timeout? [abs. time, sec]
  */
  double timeout;
  
  /*
  ** Methods
  */
  int (*start)(struct process*);
  int (*stop)(struct process*);
  int (*call)(struct process*);
};

typedef struct process process_t;

extern process_t process_table[];
extern int process_running;

extern void process_schedule();
extern void process_delay(int timeout);

extern process_t* process_new (char *name, void (*f)(int arg),int arg);
extern int process_raise (int exc);
extern process_t* process_self();
extern int process_end();
extern void process_await(process_t* p,int timeout,void (*handler)(process_t*,void *arg),void *arg );
extern void process_await_io(process_t* p,int timeout,char kind,int fd);
extern void process_wakeup(process_t *p);

#define NILPROC (process_t *)0
#define NILTMO  (double)(0)

#define PRINT(q) {\
  process_t *qe=q;\
  printf("[");\
  while (qe != NILPROC)\
  {\
    printf("%d:%s%s%s%s%s",qe->id,qe->name,\
      (qe->read!=NILFD?"|R":""),\
      (qe->write!=NILFD?"|W":""),\
      (qe->excep!=NILFD?"|X":""),\
      (qe->timeout!=NILTMO?"|T":""));\
    if (qe->next!= NILPROC) printf(",");\
    qe=qe->next;\
  };\
  printf("]");\
}

#define ADD(q,p) {\
  if (q == NILPROC) q = p;\
  else\
  {\
    process_t *qe=q;\
    while (qe->next != NILPROC) qe = qe->next;\
    qe->next = p;\
  };\
  if (QDBG==1) printf("[ADD]: adding process %d to Q%x:", (p)->id,(unsigned int)&q);\
  if (QDBG==1) {PRINT(q);printf("\n");};\
}
#define EMPTY(q) (q==NILPROC)

#define HEAD(q,p) {\
  process_t *head=q;\
  q=q->next;\
  head->next=NILPROC;\
  p=head;\
  if (QDBG==1) printf("[HEAD]: removing head process %d from Q%x:", (p)->id,(unsigned int)&q);\
  if (QDBG==1) {PRINT(q);printf("\n");};\
} 

#define MEM(q,p,found) {\
  process_t *qe=q;\
  while (qe != NILPROC && qe != p) qe = qe->next;\
  if (qe == NILPROC) found=0; else found=1;\
}

#define REM(q,p) {\
  process_t *qe=q;\
  if (q==p) q=q->next; else {\
  while (qe != NILPROC && qe->next != NILPROC && qe->next != p) qe = qe->next;\
  if (qe == NILPROC) conpro_err("[REM]: empty queue");\
  if (qe->next == NILPROC) conpro_err("[REM]: process not found");\
  qe->next=(p)->next;};\
  (p)->next=NILPROC;\
  if (QDBG==1) printf("[REM]: removing process %d from Q%x:", (p)->id,(unsigned int)&q);\
  if (QDBG==1) {PRINT(q);printf("\n");};\
} 


    
/*
** from timer.h, but required here for
** process scheduling
*/
struct timers {
  int qawait[MAX_PROC];
  int on;
  /*
  ** [abs. time, sec]
  */
  double timeout;
  /*
  ** [microsec]
  */
  int interval;
  int once;
  int waiting;
  struct timers *next;
  
  int (*init)(struct timers *);
  int (*start)(struct timers *);
  int (*stop)(struct timers *);
  int (*await)(struct timers *);
  int (*time)(struct timers *,int d);
  
};

typedef struct timers timers_t;

#define NILT  (timers_t *)0
#define timer_t timers_t

extern timers_t *q_timer;
#endif /* __PROCH */

#ifndef __EVENTH
#define __EVENTH

/*
******************************************
** EVENT
******************************************
*/
struct event {
  int qawait[MAX_PROC];
  int waiting;
  int (*init)(struct event *);
  int (*await)(struct event *);
  int (*wakeup) (struct event *);
};

typedef struct event event_t;

extern event_t *event_new(char *params);

#endif /* __EVENTH */

#ifndef __MUTEXH
#define __MUTEXH

/*
******************************************
** MUTEX
******************************************
*/
struct mutex {
  int qawait[MAX_PROC];
  int qawait_head,qawait_tail;
  int owner;
  int (*lock)(struct mutex*);
  int (*unlock)(struct mutex*);
  int (*init)(struct mutex*);
};

typedef struct mutex mutex_t;

extern mutex_t *mutex_new(char *params);

#endif /* __MUTEXH */

#ifndef __QUEUEH
#define __QUEUEH

/*
******************************************
** QUEUE
******************************************
*/
struct queue {
  /*
  ** Queue management
  */
  int size;
  int head,tail;
  int full;
  int empty;
  int *data;
  /*
  ** Process management
  */
  int qawait[MAX_PROC];
  /*
  ** Timeout [microsec]
  */
  int timeout[MAX_PROC];
  int qawait_head;
  int qawait_tail;
  
  int (*write)(struct queue *,int d);
  int (*read)(struct queue *);
  int (*unlock)(struct queue *);
};

typedef struct queue queue_t;
extern queue_t *queue_new(char *params);

#endif /* __QUEUEH */

#ifndef __UARTH
#define __UARTH

/*
******************************************
** UART Serial Interface
******************************************
*/
struct terminal_io {
     int c_ignbrk;
     int c_brkint;
     int c_ignpar;
     int c_parmrk;
     int c_inpck;
     int c_istrip;
     int c_inlcr;
     int c_igncr;
     int c_icrnl;
     int c_ixon;
     int c_ixoff;
     int c_opost;
     int c_obaud;
     int c_ibaud;
     int c_csize;
     int c_cstopb;
     int c_cread;
     int c_parenb;
     int c_parodd;
     int c_hupcl;
     int c_clocal;
     int c_isig;
     int c_icanon;
     int c_noflsh;
     int c_echo;
     int c_echoe;
     int c_echok;
     int c_echonl;
     int c_vintr;
     int c_vquit;
     int c_verase;
     int c_vkill;
     int c_veof;
     int c_veol;
     int c_vmin;
     int c_vtime;
     int c_vstart;
     int c_vstop;
};

struct uart {
  char dev[80];
  int Baud;
  int fd;
  struct terminal_io *tio;
  int (*init)(struct uart *);
  int (*baud)(struct uart *, int d);
  int (*start)(struct uart *);
  int (*stop)(struct uart *);
  int (*read)(struct uart *, int *d, int *err);
  int (*write)(struct uart *, int d, int *err);
};

typedef struct uart uart_t;

extern uart_t * uart_new(char *params);

#endif /* __UARTH */

#ifndef __SYSTEMH
#define __SYSTEMH

/*
******************************************
** SYSTEM
******************************************
*/
struct system {
  int Clock;
  int (*clock)(struct system *,int d);
  int (*target)(struct system *,char *str);
  int (*reset_level)(struct system *,int d);
  int (*simu_cycles)(struct system *,int d);
  
};

typedef struct system system_t;

extern system_t *system_new(char *params);

#endif /* __SYSTEMH */

#ifndef __TIMERH
#define __TIMERH

/*
******************************************
** TIMER
******************************************
*/

extern timers_t *timer_new(char *params);

#endif /* __TIMERH */
#endif /* !__com_models */
