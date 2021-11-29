#include "conpro.h"
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
  ** Pending interrupt signal?
  */
  int signal;
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
  ** PROC_AWAIT on process termination: call
  */
  int join;
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
