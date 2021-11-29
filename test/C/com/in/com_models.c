#include "com_models.h"
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

int64 int64_not(int64 x)
{
  return (!x);
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
  int64_not,  
  int64_or,  
  int64_xor,   
  int64_shift_left,   
  int64_shift_right, 
  int64_of_int,  
  int64_to_int, 
  int64_to_string,
  int64_of_string, 
};

int64 bit64(int a,int b,int64 o)
{
  char m[80];
  int i;
  strcpy(m,"0b");
  m[b+1+2]=0;
  for(i=0;i<b+1;i++) m[i+2]='0';
  
  for(i=a;i<=b;i++) 
    m[b-i+2] <- '1';
  return Int64.shift_right (Int64.logand(o,Int64.of_string(m)),a);
}

int64 bit64w(int a,int b,int64 o,int64 e)
{
  char m[80],m2[80];
  int64 d1,d2;
  int i;
  strcpy(m,"0b");
  strcpy(m2,"0b");
  m[b+1+2]=0;
  m2[b-a+1+2]=0;
  for(i=0;i<b+1;i++) m[i+2]='0';
  for(i=0;i<b-a+1;i++) m2[i+2]='1';
  
  for(i=a;i<=b;i++) 
    m[b-i+2] <- '1';
  d1=Int64.of_string(m);
  d2=Int64.of_string(m2);
  return (Int64.logor (Int64.logand(o,Int64.lognot(m)),(Int64.shift_left (Int64.logand(e,m2),a))));
}

#endif /* ARCH_INT64_TYPE */


int bit(int a,int b,int o)
{
  char m[80];
  int i;
  strcpy(m,"0b");
  m[b+1+2]=0;
  for(i=0;i<b+1;i++) m[i+2]='0';
  
  for(i=a;i<=b;i++) 
    m[b-i+2] <- '1';
  return ((o  && int_of_string(m)) >> a);
}

int bitw(int a,int b,int o,int e)
{
  char m[80],m2[80];
  int d1,d2;
  int i;
  strcpy(m,"0b");
  strcpy(m2,"0b");
  m[b+1+2]=0;
  m2[b-a+1+2]=0;
  for(i=0;i<b+1;i++) m[i+2]='0';
  for(i=0;i<b-a+1;i++) m2[i+2]='1';
  
  for(i=a;i<=b;i++) 
    m[b-i+2] <- '1';
  d1=int_of_string(m);
  d2=int_of_string(m2);
  return ((o && (!m)) || ((e && m2) << a));
}

int sign(int n,int e)
{
  if (bit((n-1),(n-1),e) == 1) 
  {
    int v = bit(0,(n-2),e);
    return -(bit(0,(n-2),(!(v-1))));
  }
  else
    return e;   
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
}

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
      delay_tv.tv_sec=(int)delay;
      delay_tv.tv_usec=(delay - (double) delay_tv.tv_sec) * 1E6;
      delay_ptr = &delay_tv;
      if (PRODBG==1) printf("[process_io]: waiting %d.%d s...\n",(int)delay_tv.tv_sec,(int)delay_tv.tv_usec);
    }
    else
      delay_ptr=(struct timeval *)0;
    if (PRODBG==1) printf("[process_io] readfds=%x writefds=%x exceptfds=%x\n",
        (int)readfds.fds_bits[0], (int)writefds.fds_bits[0],(int)exceptfds.fds_bits[0]);    
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
    if (PRODBG==1) printf("[process_schedule]: %d: starting process %d...\n",pid_run, pid);
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
    if (PRODBG==1) printf("[process_schedule]: %d: after starting process %d...\n",pid_run,pid);
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
    if (ready == 0)  
    {
      process_dump();
      conpro_err ("[process_schedule]: no processes ready");
    };
    goto again;
  }
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
** QUEUE
******************************************
*/
static int queue_write (queue_t *q,int d);
static int queue_read (queue_t *q);
static int queue_unlock (queue_t *q);

static void queue_handler(process_t *p,void *arg)
{
  queue_t *q=(queue_t *)arg;
  
  int i;
  for (i=0;i<MAX_PROC;i++)
  {
    if (q->qawait[i] == p->id)
    {
        q->qawait[i] = NILPID;
        i=MAX_PROC;
    };
  };
  
}

queue_t *queue_new(char *params)
{
    int size=16;
    queue_t *q;
    int i;
    i=get_param(params,"size");
    if (i != -1) size=i;
    
    q=(queue_t *) malloc(sizeof(queue_t));
    mem_check((void *)q,"queue_new");
    q->data=(int *)malloc(sizeof(int)*size);
    mem_check(q->data,"queue_new");
    
    q->size=size;
    q->head=q->tail=0;
    q->qawait_head=q->qawait_tail=0;
    q->full=0;q->empty=1;
    q->read=queue_read;
    q->write=queue_write;
    q->unlock=queue_unlock;
    for(i=0;i<MAX_PROC;i++) q->qawait[i]=NILPID;
    return q;
}

static int queue_write (queue_t *q,int d)
{
  
again:
  if (q->full == 0) 
  {
    if (q->empty)
    {
      /*
      ** Find next waiting process in queue, if any. Processes can be already
      ** removed from await queue!
      */
      while (q->qawait[q->qawait_tail] == NILPID && q->qawait_tail != q->qawait_head) 
      {
        q->qawait_tail++; if (q->qawait_tail == MAX_PROC) q->qawait_tail=0;       
      };
      if (q->qawait_tail != q->qawait_head) 
      {
        process_wakeup(&process_table[q->qawait[q->qawait_tail]]);
        q->qawait_tail++; if (q->qawait_tail == MAX_PROC) q->qawait_tail=0;
      };
    };
    q->empty=0;
    
    q->data[q->head] = d;
    q->head++;
    if (q->head == q->size) q->head=0;
    if (q->head == q->tail) q->full=1;
  }
  else
  {
    process_t *p=process_self();

    process_await(p,0,queue_handler,(void *) q);

    q->qawait[q->qawait_head] = p->id;
    q->timeout[q->qawait_head] = 0;
    q->qawait_head++; if (q->qawait_head == MAX_PROC) q->qawait_head=0;
    process_schedule();
    goto again;
  };
  return 0;
}

static int queue_read (queue_t *q)
{
  int d;
  
again:
  if (q->empty == 0) 
  {
    if (q->full == 1)
    {
      /*
      ** Find next waiting process in queue, if any. Processes can be already
      ** removed from await queue!
      */
      while (q->qawait[q->qawait_tail] == NILPID && q->qawait_tail != q->qawait_head) 
      {
        q->qawait_tail++; if (q->qawait_tail == MAX_PROC) q->qawait_tail=0;       
      };
      if (q->qawait_tail != q->qawait_head) 
      {
        process_wakeup(&process_table[q->qawait[q->qawait_tail]]);
        q->qawait_tail++; if (q->qawait_tail == MAX_PROC) q->qawait_tail=0;
      };
    };
    q->full=0;
                
    d=q->data[q->tail];
    q->tail++;
    if (q->tail == q->size) q->tail=0; 
    if (q->head == q->tail) q->empty=1;
    return d;
  }
  else
  {
    process_t *p=process_self();

    process_await(p,0,queue_handler,(void *) q);

    q->qawait[q->qawait_head] = p->id;
    q->timeout[q->qawait_head] = 0;
    q->qawait_head++; if (q->qawait_head == MAX_PROC) q->qawait_head=0;    
    process_schedule();
    goto again;
  };
}

static int queue_unlock(queue_t *q)
{
  int i;
  int pid;
  while (q->qawait_tail != q->qawait_head) 
  {
    pid=q->qawait[q->qawait_tail];
    if (pid != NILPID) process_wakeup(&process_table[pid]);
    q->qawait_tail++; if (q->qawait_tail == MAX_PROC) q->qawait_tail=0;       
  };
  process_schedule();
  return 0;
}
/*
******************************************
** UART Serial Interface
******************************************
*/


static int uart_init(uart_t *);
static int uart_baud(uart_t *, int d);
static int uart_start(uart_t *);
static int uart_stop(uart_t *);
static int uart_read(uart_t *, int *d,int *err);
static int uart_write(uart_t *,int d, int *err);

static struct {
  speed_t speed;
  int baud;
} speedtable[] = {
  {B50,      50},
  {B75,      75},
  {B110,     110},
  {B134,     134},
  {B150,     150},
  {B300,     300},
  {B600,     600},
  {B1200,    1200},
  {B1800,    1800},
  {B2400,    2400},
  {B4800,    4800},
  {B9600,    9600},
  {B19200,   19200},
  {B38400,   38400},
#ifdef B57600
  {B57600,   57600},
#endif
#ifdef B115200
  {B115200,  115200},
#endif
#ifdef B230400
  {B230400,  230400},
#endif
  {B0,       0}
};
#define NSPEEDS (sizeof(speedtable) / sizeof(speedtable[0]))
enum { Bool, Enum, Speed, Char, End };

enum { Input, Output };

static struct termios terminal_status;

#define iflags ((long)(&terminal_status.c_iflag))
#define oflags ((long)(&terminal_status.c_oflag))
#define cflags ((long)(&terminal_status.c_cflag))
#define lflags ((long)(&terminal_status.c_lflag))

/* Number of fields in the terminal_io record field. Cf. unix.mli */

#define NFIELDS 38

/* Structure of the terminal_io record. Cf. unix.mli */
static long terminal_io_descr[] = {
  /* Input modes */
  Bool, iflags, IGNBRK,
  Bool, iflags, BRKINT,
  Bool, iflags, IGNPAR,
  Bool, iflags, PARMRK,
  Bool, iflags, INPCK,
  Bool, iflags, ISTRIP,
  Bool, iflags, INLCR,
  Bool, iflags, IGNCR,
  Bool, iflags, ICRNL,
  Bool, iflags, IXON,
  Bool, iflags, IXOFF,
  /* Output modes */
  Bool, oflags, OPOST,
  /* Control modes */
  Speed, Output,
  Speed, Input,
  Enum, cflags, 5, 4, CSIZE, CS5, CS6, CS7, CS8,
  Enum, cflags, 1, 2, CSTOPB, 0, CSTOPB,
  Bool, cflags, CREAD,
  Bool, cflags, PARENB,
  Bool, cflags, PARODD,
  Bool, cflags, HUPCL,
  Bool, cflags, CLOCAL,
  /* Local modes */
  Bool, lflags, ISIG,
  Bool, lflags, ICANON,
  Bool, lflags, NOFLSH,
  Bool, lflags, ECHO,
  Bool, lflags, ECHOE,
  Bool, lflags, ECHOK,
  Bool, lflags, ECHONL,
  /* Control characters */
  Char, VINTR,
  Char, VQUIT,
  Char, VERASE,
  Char, VKILL,
  Char, VEOF,
  Char, VEOL,
  Char, VMIN,
  Char, VTIME,
  Char, VSTART,
  Char, VSTOP,
  End
};

#undef iflags
#undef oflags
#undef cflags
#undef lflags




static void encode_terminal_status(int *dst)
{
  long * pc;
  int i;

  for(pc = terminal_io_descr; *pc != End; dst++) {
    switch(*pc++) {
    case Bool:
      { int * src = (int *) (*pc++);
        int msk = *pc++;
        *dst = ((*src & msk)==0?0:1);
        break; }
    case Enum:
      { int * src = (int *) (*pc++);
        int ofs = *pc++;
        int num = *pc++;
        int msk = *pc++;
        for (i = 0; i < num; i++) {
          if ((*src & msk) == pc[i]) {
            *dst = (i + ofs);
            break;
          }
        }
        pc += num;
        break; }
    case Speed:
      { int which = *pc++;
        speed_t speed = 0;
        *dst = (9600);   /* in case no speed in speedtable matches */
        switch (which) {
        case Output:
          speed = cfgetospeed(&terminal_status); break;
        case Input:
          speed = cfgetispeed(&terminal_status); break;
        }
        for (i = 0; i < NSPEEDS; i++) {
          if (speed == speedtable[i].speed) {
            *dst = (speedtable[i].baud);
            break;
          }
        }
        break; }
    case Char:
      { int which = *pc++;
        *dst = (int)(terminal_status.c_cc[which]);
        break; }
    }
  }
}

static void decode_terminal_status(int *src)
{
  long * pc;
  int i;

  for (pc = terminal_io_descr; *pc != End; src++) {
    switch(*pc++) {
    case Bool:
      { int * dst = (int *) (*pc++);
        int msk = *pc++;
        if (*src == 1)
          *dst |= msk;
        else
          *dst &= ~msk;
        break; }
    case Enum:
      { int * dst = (int *) (*pc++);
        int ofs = *pc++;
        int num = *pc++;
        int msk = *pc++;
        i = (*src) - ofs;
        if (i >= 0 && i < num) {
          *dst = (*dst & ~msk) | pc[i];
        } else {
          conpro_err("tcsetattr");
        }
        pc += num;
        break; }
    case Speed:
      { int which = *pc++;
        int baud = (*src);
        int res = 0;
        for (i = 0; i < NSPEEDS; i++) {
          if (baud == speedtable[i].baud) {
            switch (which) {
            case Output:
              res = cfsetospeed(&terminal_status, speedtable[i].speed); break;
            case Input:
              res = cfsetispeed(&terminal_status, speedtable[i].speed); break;
            }
            if (res == -1) conpro_err("tcsetattr");
            goto ok;
          }
        }
        conpro_err("tcsetattr");
      ok:
        break; }
    case Char:
      { int which = *pc++;
        terminal_status.c_cc[which] = (*src);
        break; }
    }
  }
}



uart_t * uart_new(char *params) 
{
  uart_t *u;
  int res;
  int Baud=115200;
  int i;

  u=(uart_t*) malloc(sizeof(uart_t));
  mem_check((void*)u,"uart_new");
  u->tio=(struct terminal_io*)malloc(sizeof(struct terminal_io));
  mem_check((void *)u->tio,"uart_new");
  
  i=get_param(params,"baud");
  if (i != -1) Baud=i;
  i=get_params(params,u->dev,"dev");
  if (i== -1) strcpy(u->dev,"/dev/cua/a");
  
  
  u->init=uart_init;
  u->baud=uart_baud;
  u->start=uart_start;
  u->stop=uart_stop;
  u->read=uart_read;
  u->write=uart_write;
  u->Baud=Baud;
  
  return u;    
}
static int uart_start(uart_t *u)
{
  return 0;
}
static int uart_stop(uart_t *u)
{
  return 0;
}
static int uart_baud(uart_t *u, int d)
{
  u->Baud=d;
  return 0;
}

static int uart_init(uart_t *u)
{
  int res;
  u->fd=open(u->dev,(O_RDWR|O_NONBLOCK));
  if (u->fd<=0) conpro_err("[UART]: can't open device");
  res=tcgetattr(u->fd,&terminal_status);  
  if (res!=0) conpro_err ("[UART]: can't get terminal io structure");
  encode_terminal_status((int*)u->tio);  
  u->tio->c_ibaud=u->Baud;
  u->tio->c_obaud=u->Baud;
  u->tio->c_icanon = 0;
  u->tio->c_echo   = 0;
  u->tio->c_echoe  = 0;
  u->tio->c_echok  = 0;
  u->tio->c_echonl = 0;
  u->tio->c_noflsh = 0; 
  u->tio->c_vmin = 1; 

  u->tio->c_ignbrk = 1;
  u->tio->c_brkint = 0;
  u->tio->c_igncr = 1;
  u->tio->c_ixon = 0;
  u->tio->c_ixoff = 0;
  u->tio->c_opost = 0;
  u->tio->c_clocal = 1;
  u->tio->c_isig = 0;
  u->tio->c_istrip = 0;
  u->tio->c_hupcl = 0;   

  u->tio->c_vintr = 0;
  u->tio->c_vquit = 0;
  u->tio->c_verase = 0;
  u->tio->c_vkill = 0;
  u->tio->c_veof = 0;
  u->tio->c_veol = 0;
  
  decode_terminal_status((int*)u->tio);
  res=tcsetattr(u->fd,TCSANOW,&terminal_status);
  if (res!=0) conpro_err ("[UART]: can't set terminal io structure");
  
  return 0;
}

static int uart_read(uart_t *u, int * d, int *err)
{
  char buf[8];
  int res;
again:
  res=read(u->fd,buf,1);
  if (res == -1 && errno == EINTR) goto again;
  if (res == -1 && (errno == EAGAIN || errno == EWOULDBLOCK))
  {
    process_await_io(process_self(),0,'r',u->fd);
    goto again;
  };
  if (res == 1) 
  {
    *d=(int)((unsigned int)buf[0]);
    *err=0;
  }
  else
    *err=1;
  return 1;
}

static int uart_write(uart_t *u,int d, int *err)
{
  char buf[8];
  int res;
again:
  buf[0]=(char)((unsigned int)d);
  res=write(u->fd,buf,1);
  if (res == -1 && errno == EINTR) goto again;
  if (res == -1 && (errno == EAGAIN || errno == EWOULDBLOCK))
  {
    process_await_io(process_self(),0,'w',u->fd);
    goto again;
  };
  if (res==1) *err=0; else *err=1;
  return res;
}
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
** TIMER
******************************************
*/

static int timer_await(timers_t *t);
static int timer_start(timers_t *t);
static int timer_stop(timers_t *t);
static int timer_init(timers_t *t);
static int timer_time(timers_t *t,int d);


static void timer_handler(process_t *p,void *arg)
{
  timers_t *t=(timers_t *)arg;
  
  int i;
  for (i=0;i<MAX_PROC;i++)
  {
    if (t->qawait[i] == p->id)
    {
        t->qawait[i] = NILPID;
        i=MAX_PROC;
    };
  };
}

/*
** Timeout: [microsec]
*/
timers_t *timer_new(char *params)
{
  timers_t *t;
  int i;
  int tmo=100000; 
  int once=0;

  i=get_param(params,"time");
  if (i != -1) tmo=i;
  i=get_param(params,"mode");
  if (i != -1) once=i;
      
  t=(timers_t *)malloc(sizeof(timers_t));
  mem_check((void*)t,"timer_new");
  
  t->waiting=0;
  t->timeout=NILTMO;
  t->interval=tmo;
  t->once=once;
  t->on=0;
  t->next=NILT;
  
  t->await=timer_await;
  t->start=timer_start;
  t->stop=timer_stop;
  t->init=timer_init; 
  t->time=timer_time; 
  
  if (q_timer==NILT) q_timer=t;
  else 
  {
    /*
    ** Put on top of the queue
    */
    t->next=q_timer;
    q_timer=t;
  };
  for(i=0;i<MAX_PROC;i++) t->qawait[i]=NILPID;
    
  return t;
}

static int timer_start(timers_t *t)
{
  double date = timeofday();
  t->on=1;
  t->timeout=date+((double)t->interval*1e-6);
  return 0;
}

static int timer_stop(timers_t *t)
{
  t->on=0;
  t->timeout=NILTMO;
  return 0;
}

static int timer_init(timers_t *t)
{
  return 0;
}

static int timer_time(timers_t *t, int tmo)
{
  t->interval=tmo;
  return 0;
}

static int timer_await(timers_t *t)
{
  process_t *p=process_self();
  
  process_await(p,0,timer_handler,(void *) t);
      
  t->qawait[t->waiting] = p->id;
  t->waiting++;
  process_schedule();
  return 0;
}

