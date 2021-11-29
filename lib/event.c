/*
******************************************
** EVENT
******************************************
*/
#include "event.h"
#define VERSION_EVENT "1.1"

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
  ev->latch=0;
  i=get_param(params,"latch");
  if (i != -1) ev->latched=i;
  
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
  
  if ((ev->latched == 0) || (ev->latched == 1 && ev->latch == 0))
  {
    process_await(p,0,event_handler,(void *) ev);
      
    ev->qawait[ev->waiting] = p->id;
    ev->waiting++;
    process_schedule();
  }
  else ev->latch=0;
  return 0;
}

static int event_wakeup(event_t *ev)
{
  int i;
  int pid;
  if (ev->waiting == 0 && ev->latched==1) ev->latch=1;
  else 
  {
    for(i=0;i<ev->waiting;i++)
    {
     pid=ev->qawait[i];
     if (pid != -1) process_wakeup(&process_table[pid]);
    };
    ev->waiting=0;
  };
  process_schedule();
  return 0;
}
