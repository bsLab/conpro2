/*
******************************************
** SEMAPHORE
******************************************
*/

#include "semaphore.h"
#define VERSION_SEMAPHORE "1.1"

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

