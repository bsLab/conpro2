/*
******************************************
** MUTEX
******************************************
*/

#include "mutex.h"
#define VERSION_MUTEX "1.1"

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
  
  /*
  ** Find next waiting process in queue, if any. Processes can be already
  ** removed from await queue!
  */
  m->owner=NILPID;
  while (m->qawait[m->qawait_tail] == NILPID && m->qawait_tail != m->qawait_head) 
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

