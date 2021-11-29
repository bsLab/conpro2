

/*
******************************************
** TIMER
******************************************
*/
#include "timer.h"
#define VERSION_TIMER "1.1"

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

