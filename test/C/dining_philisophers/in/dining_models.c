#include "dining_models.h"
/*
******************************************
** SYSTEM
******************************************
*/


static int sys_clock(struct system *,int d);
static int sys_target(struct system *,char *str);
static int sys_reset_level(struct system *,int d);
static int sys_simu_cycles(struct system *,int d);
 
system_t *system_new(char *params)
{
  int i;
  system_t *sys;

  sys=(system_t *)malloc(sizeof(system_t));
  mem_check((void*)sys,"system_new");

    i=get_param(params,"clock");
  if (i != -1) sys->Clock=i; else sys->Clock=10000000;
  
  sys->clock=sys_clock;
  sys->target=sys_target;
  sys->reset_level=sys_reset_level;
  sys->simu_cycles=sys_simu_cycles;
  return sys;
}

static int sys_clock(struct system *s,int d)
{
  s->Clock=d;
  return 0;  
}
static int sys_reset_level(struct system *s,int d)
{
  return 0;  
}
static int sys_target(struct system *s,char *str)
{
  return 0;  
}
static int sys_simu_cycles(struct system *s,int d)
{
  return 0;  
}
/*
******************************************
** EVENT
******************************************
*/
static int event_init();
static int event_await();
static int event_wakeup();

event_t *event_new(char *params)
{
  event_t *ev;
  int i;
  ev=(event_t *)malloc(sizeof(event_t));
  mem_check((void*)ev,"event_new");
  
  ev->waiting=0;
  for(i=0;i<MAX_PROC;i++) ev->qawait[i]=NILPID;
  ev->init=event_init;
  ev->await=event_await;
  ev->wakeup=event_wakeup;
  return ev;
}

static void event_handler(process_t *p,void *arg)
{
  event_t *ev=(event_t *)arg;
  
  int i;
  for (i=0;i<MAX_PROC;i++)
  {
    if (ev->qawait[i] == p->id)
    {
        ev->qawait[i] = NILPID;
        i=MAX_PROC;
    };
  };
}

static int event_init(event_t *ev)
{
  process_t *p=process_self();
  int i;
  
  for(i=0;i<MAX_PROC;i++) ev->qawait[i]=NILPID;
  ev->waiting=0;
  return 0;
}

static int event_await(event_t *ev)
{
  process_t *p=process_self();
  
  process_await(p,0,event_handler,(void *) ev);
      
  ev->qawait[ev->waiting] = p->id;
  ev->waiting++;
  process_schedule();
  return 0;
}

static int event_wakeup(event_t *ev)
{
  int i;
  int pid;
  for(i=0;i<ev->waiting;i++)
  {
    pid=ev->qawait[i];
    if (pid != -1) process_wakeup(&process_table[pid]);
  };
  ev->waiting=0;
  process_schedule();
  return 0;
}
/*
******************************************
** SEMAPHORE
******************************************
*/


static int semaphore_init(semaphore_t *ev,int v);
static int semaphore_down(semaphore_t *ev);
static int semaphore_up(semaphore_t *ev);

semaphore_t *semaphore_new(char *params)
{
  int i;
  int init=1;
  semaphore_t *s;

  i=get_param(params,"init");
  if (i != -1) init=i;
  
  s=(semaphore_t *)malloc(sizeof(semaphore_t));
  mem_check((void*)s,"sema_new");

  
  s->qawait_head=s->qawait_tail=0;
  s->count=init;
  s->init=semaphore_init;
  s->down=semaphore_down;
  s->up=semaphore_up;
  
  for(i=0;i<MAX_PROC;i++) s->qawait[i]=NILPID;
  return s;
}

static void sema_handler(process_t *p,void *arg)
{
  semaphore_t *s=(semaphore_t *)arg;
  
  int i;
  for (i=0;i<MAX_PROC;i++)
  {
    if (s->qawait[i] == p->id)
    {
        s->qawait[i] = NILPID;
        i=MAX_PROC;
    };
  };
}

static int semaphore_init(semaphore_t *s,int v)
{
  process_t *p=process_self();
  s->qawait_head=s->qawait_tail=0;
  s->count=v;
  return 0;
}

static int semaphore_down(semaphore_t *s)
{
  process_t *p=process_self();
again:
    
  if (s->count == 0)
  {
    process_await(p,0,sema_handler,(void *) s);
      
    s->qawait[s->qawait_head] = p->id;
    s->qawait_head++; if (s->qawait_head == MAX_PROC) s->qawait_head=0;
    process_schedule();
    goto again;
  }
  else
      s->count--;
  return 0;
}

static int semaphore_up(semaphore_t *s)
{
  int i;
  int pid;
  
  /*
  ** Find next waiting process in queue, if any. Processes can be already
  ** removed from await queue!
  */
  s->count++;
  pid=s->qawait[s->qawait_tail];
  while (pid == NILPID && s->qawait_tail != s->qawait_head) 
  {
    s->qawait_tail++; if (s->qawait_tail == MAX_PROC) s->qawait_tail=0;       
  };
  if (s->qawait_tail != s->qawait_head) 
  {
    process_wakeup(&process_table[s->qawait[s->qawait_tail]]);
    s->qawait_tail++; if (s->qawait_tail == MAX_PROC) s->qawait_tail=0;
    process_schedule();
  };
  
  return 0;
}

/*
******************************************
** MUTEX
******************************************
*/

static int mutex_lock(mutex_t *m);
static int mutex_init(mutex_t *m);
static int mutex_unlock(mutex_t *m);

mutex_t *mutex_new(char *params)
{
  int i;
  mutex_t *m;
  m=(mutex_t *)malloc(sizeof(mutex_t));
  mem_check((void*)m,"mutex_new");
  
  m->qawait_head=m->qawait_tail=0;
  m->owner=NILPID;
  m->lock=mutex_lock;
  m->unlock=mutex_unlock;
  m->init=mutex_init;
  for(i=0;i<MAX_PROC;i++) m->qawait[i]=NILPID;
  return m;
}

static void mutex_handler(process_t *p,void *arg)
{
  mutex_t *m=(mutex_t *)arg;
  
  int i;
  for (i=0;i<MAX_PROC;i++)
  {
    if (m->qawait[i] == p->id)
    {
        m->qawait[i] = NILPID;
        i=MAX_PROC;
    };
  };
}

static int mutex_init(mutex_t *m)
{
  process_t *p=process_self();
  m->owner=NILPID;
  m->qawait_head=m->qawait_tail=0;
  return 0;
}

static int mutex_lock(mutex_t *m)
{
  process_t *p=process_self();
again:
    
  if (m->owner != NILPID)
  {
    process_await(p,0,mutex_handler,(void *) m);
      
    m->qawait[m->qawait_head] = p->id;
    m->qawait_head++; if (m->qawait_head == MAX_PROC) m->qawait_head=0;
    process_schedule();
    goto again;
  }
  else
      m->owner=p->id;
  return 0;
}

static int mutex_unlock(mutex_t *m)
{
  int i;
  int pid;
  
  /*
  ** Find next waiting process in queue, if any. Processes can be already
  ** removed from await queue!
  */
  m->owner=NILPID;
  pid=m->qawait[m->qawait_tail];
  while (pid == NILPID && m->qawait_tail != m->qawait_head) 
  {
    m->qawait_tail++; if (m->qawait_tail == MAX_PROC) m->qawait_tail=0;       
  };
  if (m->qawait_tail != m->qawait_head) 
  {
    process_wakeup(&process_table[m->qawait[m->qawait_tail]]);
    m->qawait_tail++; if (m->qawait_tail == MAX_PROC) m->qawait_tail=0;
    process_schedule();
  };
  return 0;
}

/*
******************************************
** CONPRO API layer
******************************************
*/


static char * parse_sign_and_base(char * p,
                                  /*out*/ int * base,
                                  /*out*/ int * sign)
{
  *sign = 1;
  if (*p == '-') {
    *sign = -1;
    p++;
  }
  *base = 10;
  if (*p == '0') {
    switch (p[1]) {
    case 'x': case 'X':
      *base = 16; p += 2; break;
    case 'o': case 'O':
      *base = 8; p += 2; break;
    case 'b': case 'B':
      *base = 2; p += 2; break;
    }
  }
  return p;
}

static int parse_digit(char * p)
{
  int c = *p;
  if (c >= '0' && c <= '9')
    return c - '0';
  else if (c >= 'A' && c <= 'F')
    return c - 'A' + 10;
  else if (c >= 'a' && c <= 'f')
    return c - 'a' + 10;
  else
    return -1;
}

static long parse_long(char * p)
{
  unsigned long res;
  int sign, base, d;

  p = parse_sign_and_base(p, &base, &sign);
  d = parse_digit(p);
  if (d < 0 || d >= base) conpro_err("int_of_string");
  for (p++, res = d; /*nothing*/; p++) {
    d = parse_digit(p);
    if (d < 0 || d >= base) break;
    res = base * res + d;
  }
  if (*p != 0) conpro_err("int_of_string");
  return sign < 0 ? -((long) res) : (long) res;
}

int int_of_string(char  *s)
{
  return parse_long(s);
}

#define FORMAT_BUFFER_SIZE 32

static char * parse_format(char  *fmt,
                           char * suffix,
                           char format_string[],
                           char default_format_buffer[])
{
  int prec, lastletter;
  char * p;
  int len, len_suffix;

  /* Copy Caml format fmt to format_string,
     adding the suffix before the last letter of the format */
  len = strlen(fmt);
  len_suffix = strlen(suffix);
  if (len + len_suffix + 1 >= FORMAT_BUFFER_SIZE)
    conpro_err("format_int: format too long");
  memmove(format_string, fmt, len);
  p = format_string + len - 1;
  lastletter = *p;
  /* Compress two-letter formats, ignoring the [lnL] annotation */
  if (p[-1] == 'l' || p[-1] == 'n' || p[-1] == 'L') p--;
  memmove(p, suffix, len_suffix);  p += len_suffix;
  *p++ = lastletter;
  *p = 0;
  /* Determine space needed for result and allocate it dynamically if needed */
  prec = 22 + 5; /* 22 digits for 64-bit number in octal + 5 extra */
  for (p = fmt; *p != 0; p++) {
    if (*p >= '0' && *p <= '9') {
      prec = atoi(p) + 5;
      break;
    }
  }
  if (prec < FORMAT_BUFFER_SIZE)
    return default_format_buffer;
  else
  {
    p=(char *)malloc(prec + 1);
    mem_check(p,"parse_format");
    return p;
  }
}



void mem_check(void *p,const char *msg)
{
  static char _line[100];
  if (p==NULL)
  {  
   sprintf("%s: Out of memory",_line,msg);
   conpro_err(_line);
  };
  return;
}

int get_param(char* params,char *name)
{
  int v=0;
  char *p=params;
  char *n=name;
  char *e;
  char value[80];
  int found;
  while (*p != 0)
  {
    found=1;
    while (*n != 0 && *p != '=')
    {
      if (*n!=*p) {found=0;break;}
      else {n++;p++;};
    };
    if (found==1 && *n==0 && *p=='=')
    {
      e=value; p++;
      while (*p != 0 && *p != ':') 
      {
        *e=*p;
        e++;p++;
      }
      *e=0;
      return (int_of_string(value));
    }
    else
    {
      n=name;
      while (*p!=0 && *p != ':') p++;
      if (*p != 0) p++;
    };
  };
  return (-1);
}

int get_params(char* params,char *value,char *name)
{
  int v=0;
  char *p=params;
  char *n=name;
  char *e;
  int found;
  while (*p != 0)
  {
    found=1;
    while (*n != 0 && *p != '=')
    {
      if (*n!=*p) {found=0;break;}
      else {n++;p++;};
    };
    if (found==1 && *n==0 && *p=='=')
    {
      e=value; p++;
      while (*p != 0 && *p != ':') 
      {
        *e=*p;
        e++;p++;
      }
      *e=0;
      
      return 0;
    }
    else
    {
      n=name;
      while (*p!=0 && *p != ':') p++;
      if (*p != 0) p++;
    };
  };
  return (-1);
}

/*
******************************************
** Int64
******************************************
*/

#ifdef ARCH_INT64_TYPE

int64 int64_neg(int64 x)
{
  return (-x);
}

int64 int64_add(int64 a,int64 b)
{
  return (a+b);
}

int64 int64_sub(int64 a,int64 b)
{
  return (a-b);
}

int64 int64_mul(int64 a,int64 b)
{
  return (a*b);
}

int64 int64_div(int64 a,int64 b)
{
  if (b==0) conpro_err ("Divisor is zero"); 
  return (a/b);
}

int64 int64_mod(int64 a,int64 b)
{
  if (b==0) conpro_err ("Divisor is zero"); 
  return (a%b);
}

int64 int64_and(int64 a,int64 b)
{
  return (a & b);
}

int64 int64_or(int64 a,int64 b)
{
  return (a | b);
}

int64 int64_xor(int64 a,int64 b)
{
  return (a ^ b);
}

int64 int64_shift_left(int64 a,int b)
{
  return (a << b);
}

int64 int64_shift_right(int64 a,int b)
{
  return (a >> b);
}

int64 int64_of_int(int x)
{
  return ((int64)x);
}

int int64_to_int(int64 x)
{
  return ((int)x);
}

char *int64_format(char* fmt, int64 arg)
{
  char format_string[FORMAT_BUFFER_SIZE];
  char default_format_buffer[FORMAT_BUFFER_SIZE];
  char * buffer;
  char *res;

  buffer = parse_format(fmt, ARCH_INT64_PRINTF_FORMAT,
                        format_string, default_format_buffer);
  sprintf(buffer, format_string, arg);
  res=(char *)malloc(strlen(buffer));
  mem_check(res,"int64_format");
  res = strcpy(res,buffer);
  if (buffer != default_format_buffer) free(buffer);
  return res;
}

char *int64_to_string (int64 x)
{
  return (int64_format("%d",x));
}

int64 int64_of_string(char *s)
{
  char * p;
  uint64 res;
  int sign, base, d;

  p = parse_sign_and_base(s, &base, &sign);
  d = parse_digit(p);
  if (d < 0 || d >= base) conpro_err("int_of_string");
  for (p++, res = d; /*nothing*/; p++) {
    d = parse_digit(p);
    if (d < 0 || d >= base) break;
    res = base * res + d;
  }
  if (*p != 0) conpro_err("int_of_string");
  return (sign < 0 ? -((int64) res) : (int64) res);
}

Int64_t Int64 = {
  (int64) 0,
  (int64)1,
  int64_neg,
  int64_add,
  int64_sub,
  int64_mul,
  int64_div,
  int64_mod,
  int64_and,  
  int64_or,  
  int64_xor,   
  int64_shift_left,   
  int64_shift_right, 
  int64_of_int,  
  int64_to_int, 
  int64_to_string,
  int64_of_string, 
};

int64 bit64(int a,int b,int64 x)
{
  return x;
}

int64 bit64w(int a,int b,int64 x,int64 e)
{
  return x;
}

#endif /* ARCH_INT64_TYPE */


int bit(int a,int b,int x)
{
  return x;
}

int bitw(int a,int b,int x,int e)
{
  return x;
}


/*
** Return the current time as a floating-point number [sec]
*/

double timeofday(void)
{
  struct timeval tv;
  gettimeofday(&tv, NULL);
  return (double) tv.tv_sec + (double) tv.tv_usec * 1e-6;
}

void conpro_err(char *msg)
{
  printf("[CONPRO]: Error: %s\n",msg);
  fflush(stdout);
  exit(1);
};

/*
******************************************
** PROCESS
******************************************
*/

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


/*
** IO process and timeout handler
*/
static int process_io()
{
  double date = timeofday();
  double timeout=NILTMO;
  int need_select, need_wait;
  int retcode;
  struct timeval delay_tv, * delay_ptr;
  
  fd_set readfds, writefds, exceptfds;
  FD_ZERO(&readfds);
  FD_ZERO(&writefds);
  FD_ZERO(&exceptfds);
  need_select=0;
  need_wait=0;
  
  if (PRODBG==1) printf("[process_io] @ %f\n",date);
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
  ** Check awaitint processes...
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
        };
        if (p->write != NILFD)
        {
        };
        if (p->excep != NILFD)
        {
        };
        p=p->next;
    };
  };
  
  if (need_select > 0 || need_wait > 0)
  {
    if (need_wait > 0) 
    {
      double delay=timeout-date;
      delay_tv.tv_sec=(int)delay;
      delay_tv.tv_usec=(delay - (double) delay_tv.tv_sec) * 1E6;
      delay_ptr = &delay_tv;
      if (PRODBG==1) printf("[process_io]: waiting %d.%d s...\n",(int)delay_tv.tv_sec,(int)delay_tv.tv_usec);
    }
    else
      delay_ptr=(struct timeval *)0;
    
    retcode = select(FD_SETSIZE, &readfds, &writefds, &exceptfds, delay_ptr);
    return (need_select+need_wait); 
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
  process_table[i].context.uc_link=0;
  process_table[i].context.uc_stack.ss_size=PROC_SS;
  process_table[i].context.uc_stack.ss_sp=(void *)malloc(PROC_SS);
  if (process_table[i].context.uc_stack.ss_sp == NULL) conpro_err("Can't create stack for new process");
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
  ADD(q_process_stop,p);
  
  process_schedule();
  return 0;
}

/*
** Call process, await termination
*/
static int process_call (process_t *p)
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
  if (timeout > 0) p->timeout=date+(double)timeout;
  else p->timeout=NILTMO;
  
  ADD(q_process_await,p);
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
    if (PRODBG==1) printf("[process_schedule]: %d: starting process %d...\n",pid_run, pid);
    process_running=pid;
    process_table[pid].state = PROC_RUN;
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
    if (PRODBG==1) printf("[process_schedule]: %d: after starting process %d...\n",pid_run,pid);
    return;
  };
  /*
  ** Check for IO and timeout events
  */
  pid=0;
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
        };
        /*
        ** Check IO event
        */
        if (p->read != NILFD)
        {
        };
        if (p->write != NILFD)
        {
        };
        if (p->excep != NILFD)
        {
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
                };
              };
              t->waiting=0;
          };
          t=t->next;
      };
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
    if (PRODBG==1) printf("[process_schedule]: %d: resuming process %d...\n", pid_run, pid);
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
    if (PRODBG==1) printf("[process_schedule]: %d: after scheduling process %d...\n",pid_run,pid);
    return;
  };
  
  if (process_table[process_running].state == PROC_RUN) return;
  else 
  {
    int ready = process_io ();
    if (ready == 0) conpro_err ("[process_schedule]: no processes ready");
    goto again;
  }
}

