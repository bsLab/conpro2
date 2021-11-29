
/*
******************************************
** PROCESS
******************************************
*/
#include "process.h"
#define VERSION_PROCESS "1.4"

process_t process_table[MAX_PROC];
timers_t *q_timer;

int process_running=0;
static int process_top=0;

static process_t *q_process_ready=NILPROC;
static process_t *q_process_await=NILPROC;
static process_t *q_process_start=NILPROC;
static process_t *q_process_stop=NILPROC;
 
static int process_start (process_t *);
static int process_stop (process_t *);
static int process_call (process_t *);
   
static void _handler(process_t *p,void *arg)
{
  return;
}

static void process_dump()
{
  double date = timeofday();
  printf("q_process_ready:");
  PRINT(q_process_ready);
  printf("\nq_process_await:");
  PRINT(q_process_await);
  printf("\nq_process_start:");
  PRINT(q_process_start);
  printf("\nq_process_stop:");
  PRINT(q_process_stop);
  printf("\ntimer: (date=%f)\n",date);
  if (q_timer != NILT)
  {
      int i;
      timers_t *t=q_timer;
      while (t != NILT)
      {
        printf("[interval=%d,once=%d,timeout=%f,waiting=%d,on=%d,qawait=(",
                t->interval,t->once,t->timeout,t->waiting,t->on);
        for(i=0;i<t->waiting;i++)
        {
          int pid=t->qawait[i];
          if (pid != NILPID)
          {
            process_t * p=&process_table[pid];
            printf("%d:%s",p->id,p->name);  
          };
        };
        printf(")]\n");
        t=t->next;
      };
  };
  return;
}

static unsigned long fds_to_int(fd_set *fds)
{
  unsigned long x=0L;
  unsigned long mask=1L;
  
  int i;
  for (i=0;i<32;i++) {
    if (FD_ISSET(i,fds)) {
      x = x | mask; 
    };
    mask = mask << 1;
  };
  return x;
}

/*
** Process event, IO, and timeout handler
*/
static int process_io()
{
  double date = timeofday();
  double timeout=NILTMO;
  int need_select, need_wait;
  int retcode;
  int ready;
  int pid;
  struct timeval delay_tv, * delay_ptr;
  
  fd_set readfds, writefds, exceptfds;
  
  
again:
  date=timeofday();
  timeout=NILTMO;
  
  if (PRODBG==1) printf("[process_io] @ %f\n",date);
  /*
  ** Check for IO and timeout events
  */
  pid=0;
  ready=0;
  
  if (! EMPTY(q_process_await))
  {
    process_t *p=q_process_await;
    while (p!=NILPROC)
    {
        /*
        ** Check timeout event
        */
        if (p->timeout != NILTMO && date > p->timeout) 
        {
            p->timeout=NILTMO;
            process_wakeup(p);
            /*
            ** Start again from beginning of queue
            */
            p=q_process_await;
            ready++;
        };
        /*
        ** Check IO event
        */
        if (p->read != NILFD)
        {
          if (FD_ISSET(p->read,&readfds)) 
          {
            p->timeout=NILTMO;
            p->read=NILFD;
            process_wakeup(p);
            ready++;              
          };
        };
        if (p->write != NILFD)
        {
          if (FD_ISSET(p->write,&writefds)) 
          {
            p->timeout=NILTMO;
            p->write=NILFD;
            process_wakeup(p);
            ready++;              
          };
        };
        if (p->excep != NILFD)
        {
          if (FD_ISSET(p->excep,&exceptfds)) 
          {
            p->timeout=NILTMO;
            p->excep=NILFD;
            process_wakeup(p);
            ready++;              
          };
        };
        p=p->next;
    };
  };
  /*
  ** Check timers
  */
  if (q_timer != NILT)
  {
      int i;
      timers_t *t=q_timer;
      while (t != NILT)
      {
          if (t->timeout != NILTMO && date > t->timeout)
          {
              if (t->once == 0) t->timeout=date+((double)t->interval*1e-6);
              else 
              {
                t->timeout=NILTMO;
                t->on=0;
              };
              for(i=0;i<t->waiting;i++)
              {
                int pid=t->qawait[i];
                if (pid != NILPID)
                {
                    /*
                    ** Wakeup
                    */
                    process_t * p=&process_table[pid];
                    if (PRODBG==1) printf("[process_schedule]: timer wakeup of process %d\n",pid);
                    process_wakeup(p);
                    t->qawait[i] = NILPID;
                    ready++;
                };
              };
              t->waiting=0;
          };
          t=t->next;
      };
  };
  if (PRODBG==1) printf("[process_io] ready=%d\n",ready);
  
  if (ready>0) return ready;

  FD_ZERO(&readfds);
  FD_ZERO(&writefds);
  FD_ZERO(&exceptfds);
  need_select=0;
  need_wait=0;
  
  /*
  ** Check timers
  */
  if (q_timer != NILT)
  {
      int i;
      timers_t *t=q_timer;
      while (t != NILT)
      {
          if (t->timeout != NILTMO && t->timeout > date)
          {
            need_wait++;
            if (t->timeout < timeout || timeout==NILTMO) 
            {
              timeout=t->timeout;
            };
          };
          t=t->next;
      };
  };
  /*
  ** Check awaiting processes...
  */
  if (q_process_await != NILPROC)
  {
    process_t *p=q_process_await;
    while (p!=NILPROC)
    {
        /*
        ** Check timeout event
        */
        if (p->timeout != NILTMO && p->timeout > date ) 
        {
            need_wait++;
            if (p->timeout < timeout || timeout==NILTMO) 
            {
              timeout=p->timeout;
            };
        };
        /*
        ** Check IO event
        */
        if (p->read != NILFD)
        {
          FD_SET(p->read,&readfds);
          need_select=MAX(need_select,p->read);
        };
        if (p->write != NILFD)
        {
          FD_SET(p->write,&writefds);
          need_select=MAX(need_select,p->write);
        };
        if (p->excep != NILFD)
        {
          FD_SET(p->excep,&exceptfds);
          need_select=MAX(need_select,p->excep);
        }; 
        p=p->next;
    };
  };

  if (PRODBG==1) printf("[process_io] need_select=%d need_wait=%d\n",need_select,need_wait);
  
  if (need_select > 0 || need_wait > 0)
  {
    if (need_wait > 0) 
    {
      double delay=timeout-date;
      if (delay < 0.0) delay = 0.0;
      delay_tv.tv_sec=(int)delay;
      delay_tv.tv_usec=(delay - (double) delay_tv.tv_sec) * 1E6;
      delay_ptr = &delay_tv;
      if (PRODBG==1) printf("[process_io]: waiting %f - %f = %f = %d.%d s...\n",
          timeout,date,delay,(int)delay_tv.tv_sec,(int)delay_tv.tv_usec);
    }
    else
      delay_ptr=(struct timeval *)0;
    if (PRODBG==1) printf("[process_io] readfds=%x writefds=%x exceptfds=%x\n",
        (int)fds_to_int(&readfds), (int)fds_to_int(&writefds),(int)fds_to_int(&exceptfds));    
    retcode = select(FD_SETSIZE, &readfds, &writefds, &exceptfds, delay_ptr);
    if (PRODBG==1) printf("[process_io] select->retcode=%d\n",retcode);

    if (retcode >= 0) goto again;
    return 0;    
  }
  else
    return 0;  
}

/*
** Initializte process table, create root process posthum.
*/
void process_init ()
{
  int i;
  for (i=0;i<MAX_PROC;i++)
  {
    process_table[i].id=i;
    if (i == 0) 
    {
      process_table[i].state=PROC_RUN;    
      getcontext(&process_table[i].context);
    }
    else process_table[i].state=PROC_FREE; 
    process_table[i].handler = _handler;
    process_table[i].arg = (void *)NULL;
    process_table[i].exception=NILEXC;
    process_table[i].signal=0;
    process_table[i].next = NILPROC;
    process_table[i].timeout=NILTMO;
    process_table[i].read=NILFD;
    process_table[i].write=NILFD;
    process_table[i].excep=NILFD;
  }; 
  /*
  ** Root/main process
  */
  process_top=1;
  q_process_ready=NILPROC;
  q_process_start=NILPROC;
  q_process_stop=NILPROC;
  q_process_await=NILPROC;
  q_timer=NILT;
}

/*
** Create process
*/
process_t* process_new (char *name, void (*f)(),int arg)
{
  int i=0;
  while (i < MAX_PROC && process_table[i].state != PROC_FREE)
    i++;
  if (i==MAX_PROC) conpro_err("Process table full");
  process_table[i].state = PROC_STOP;
  process_table[i].exception=NILEXC;
  process_table[i].next = NILPROC;
  process_table[i].timeout=NILTMO;
  process_table[i].read=NILFD;
  process_table[i].write=NILFD;
  process_table[i].excep=NILFD;
  process_table[i].join=NILPID;
  /*
  ** Set to default resource handler (dummy)
  */
  process_table[i].handler=_handler;
  
  /*
  ** Methods
  */
  process_table[i].start=process_start;
  process_table[i].stop=process_stop;
  process_table[i].call=process_call;

  strcpy(process_table[i].name,name);
  
  getcontext(&process_table[i].context);
  process_table[i].fun=f;
  process_table[i].fun_arg=arg;
  process_table[i].stack=(void *)malloc(PROC_SS);
  mem_check("process_new",process_table[i].stack);
  process_table[i].context.uc_link=0;
  process_table[i].context.uc_stack.ss_size=PROC_SS;
  process_table[i].context.uc_stack.ss_sp=process_table[i].stack;
  process_table[i].context.uc_stack.ss_flags=0;
  makecontext(&process_table[i].context,f,1,arg); 
  
  process_top=i+1;
  
  return &process_table[i];
}

/*
** Start process
*/
static int process_start (process_t *p)
{
  int found;
  p->state = PROC_START;
  p->timeout=NILTMO;
  MEM(q_process_stop,p,found);
  if (found==1) REM(q_process_stop,p);
  ADD(q_process_start,p);
  process_schedule();
  return 0;
}

/*
** Stop process
*/
static int process_stop (process_t *p)
{
  int found;
  p->state = PROC_STOP;
  /*
  ** If blocked, cleanup blocked resource
  */
  p->handler(p,p->arg);
  
  /*
  ** Process is either ready or awaiting.
  */
  MEM(q_process_ready,p,found);
  if (found==1) REM(q_process_ready,p);
  MEM(q_process_await,p,found);
  if (found==1) REM(q_process_await,p);
  MEM(q_process_stop,p,found);
  if (found==1) REM(q_process_stop,p);
  ADD(q_process_stop,p);
  
  if (p->join != NILPID)
  {
    /*
    ** There is a calling process waiting
    ** for this process termination
    */
    int i=p->join;
    process_t *pj=&process_table[i];
    MEM(q_process_await,pj,found);
    if (found==1)
    { 
      REM(q_process_await,pj);
      ADD(q_process_ready,pj);
    };
    p->join=NILPID;
  };
  process_schedule();
  return 0;
}

/*
** Call process, await termination
*/
static int process_call (process_t *p)
{
  process_t *pr=process_self();
  int found;
  p->state = PROC_START;
  p->timeout=NILTMO;
  p->join=pr->id;
  MEM(q_process_stop,p,found);
  if (found==1) REM(q_process_stop,p);
  ADD(q_process_start,p);
  
  pr->state = PROC_AWAIT;
  pr->handler=_handler;
  pr->timeout=NILTMO;
  
  ADD(q_process_await,pr);
  process_schedule();
  return 0;
}

int process_end()
{
  process_t *p=process_self();
  p->stop(p);
  return 0;  
}

/*
** Raise an exception
*/
int process_raise (int exc)
{
  int pid=process_running;
  int found;
  void *addr;
  process_table[pid].exception = exc;
  if (process_table[process_running].catcher==(void**)0)
  {
    process_table[pid].state = PROC_STOP;
    /*
    ** If blocked, cleanup blocked resource
    */
    process_table[pid].handler(&process_table[pid],process_table[pid].arg);
  
    /*
    ** Process is either ready or awaiting.
    */
    MEM(q_process_ready,&process_table[pid],found);
    if (found==1) REM(q_process_ready,&process_table[pid]);
    MEM(q_process_await,&process_table[pid],found);
    if (found==1) REM(q_process_await,&process_table[pid]);
    ADD(q_process_stop,&process_table[pid]);
  
    process_schedule();
    /* never reached - exception signal propagation */
  };
  /* we are in try-with environment */
  return 0;
}
 


/*
** Change state of process
** Timeout: [microsec]
*/
void process_await(process_t *p,int timeout,void (*handler)(process_t*p,void *arg),void *arg )
{
  
  double date = timeofday();
  p->state = PROC_AWAIT;
  p->handler=handler;
  p->arg=arg;
  if (timeout > 0) p->timeout=date+((double)timeout*1e-6);
  else p->timeout=NILTMO;
  
  ADD(q_process_await,p);
  return;
}

void process_await_io(process_t *p,int timeout,char kind, int fd)
{
  
  double date = timeofday();
  
  if (PRODBG==1) printf("[process_await_io]: %d\n",process_running);
  p->state = PROC_AWAIT;
  p->handler=_handler;
  if (timeout > 0) p->timeout=date+((double)timeout*1e-6);
  else p->timeout=NILTMO;
  switch (kind) {
    case 'r': p->read=fd; break;
    case 'w': p->write=fd; break;
    case 'x': p->excep=fd; break;
    default: conpro_err("process_await_io");
  };
  ADD(q_process_await,p);
  process_schedule();
  return;
}

void process_wakeup(process_t *p)
{
  p->state = PROC_READY;
  p->handler(p,p->arg);
  /*
  ** Reset to default handler
  */
  p->handler=_handler;
  p->timeout=NILTMO;
  REM(q_process_await,p);
  ADD(q_process_ready,p);
  return;
}



/*
** Return actual process identifier
*/ 
process_t* process_self()
{
  return &process_table[process_running];
}


/*
** Delay process (timeout: micro seconds)
*/
void process_delay(int timeout)
{
  process_t *p=process_self();
  double date = timeofday();
  p->state = PROC_AWAIT;
  p->handler=_handler;
  if (timeout > 0) p->timeout=date+((double)timeout*1e-6);
  else p->timeout=NILTMO;
  
  ADD(q_process_await,p);
  process_schedule();
  return;  
}


/*
** The process scheduler
*/
void process_schedule()
{
  int pid=0;
  int pid_run=process_running;
  double date;
 
  if (PRODBG==1) 
  {
    process_t *q=q_process_ready;
    printf("[process_schedule] %d, QREADY=",process_running);
    PRINT(q_process_ready);
    printf("\n");
  };
again:
  date = timeofday();
  /*
  ** Check for starting processes first...
  */
  if (! EMPTY(q_process_start))
  {
    int pid;
    process_t *p;
    HEAD(q_process_start,p);
    pid=p->id;
    if (PRODBG==1) printf("[process_schedule]: %d:%s: starting process %d:%s...\n",pid_run, process_table[pid_run].name,pid,process_table[pid].name);
    process_running=pid;
    process_table[pid].state = PROC_RUN;
    process_table[pid].context.uc_link=0;
    process_table[pid].context.uc_stack.ss_size=PROC_SS;
    process_table[pid].context.uc_stack.ss_sp=process_table[pid].stack;
    process_table[pid].context.uc_stack.ss_flags=0;
    makecontext(&process_table[pid].context,process_table[pid].fun,1,process_table[pid].fun_arg); 

    if (process_table[pid_run].state == PROC_RUN)
    {
       process_table[pid_run].state = PROC_READY;
       ADD(q_process_ready,&process_table[pid_run]); 
    };
    swapcontext(&process_table[pid_run].context,
                &process_table[pid].context);
    process_running=pid_run;
    if (process_table[pid].state == PROC_RUN) 
    {
      process_table[pid].state = PROC_READY;
      ADD(q_process_ready,&process_table[pid]); 
    };
    if (PRODBG==1) printf("[process_schedule]: %d:%s: after starting process %d:%s...\n",pid_run,process_table[pid_run].name,pid,process_table[pid].name);
    return;
  };
  /*
  ** Check for ready processes, schedule oldest first (FIFO order)...
  */
  pid=0;
  if (! EMPTY(q_process_ready))
  {
    int pid;
    process_t *p;
    HEAD(q_process_ready,p);
    pid=p->id;
    if (PRODBG==1) printf("[process_schedule]: %d:%s: resuming process %d:%s...\n", pid_run, process_table[pid_run].name,pid,process_table[pid].name);
    process_running=pid;
    process_table[pid].state = PROC_RUN;
    if (pid!=pid_run)
    {
      if (process_table[pid_run].state == PROC_RUN)
      {
         process_table[pid_run].state = PROC_READY;
         ADD(q_process_ready,&process_table[pid_run]); 

      };
      swapcontext(&process_table[pid_run].context,
                  &process_table[pid].context);
      process_running=pid_run;
      if (process_table[pid].state == PROC_RUN) 
      { 
        process_table[pid].state = PROC_READY;
        ADD(q_process_ready,&process_table[pid]); 
      };
    };
    if (PRODBG==1) printf("[process_schedule]: %d:%s: after scheduling process %d:%s...\n",pid_run,process_table[pid_run].name,pid,process_table[pid].name);
    return;
  };
  
  if (process_table[process_running].state == PROC_RUN) return;
  else 
  {
    int ready = process_io ();
    if (ready == 0)  
    {
      process_dump();
      conpro_err ("[process_schedule]: no processes ready");
    };
    goto again;
  }
}

