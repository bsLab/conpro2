
/*
******************************************
** QUEUE
******************************************
*/
#include "queue.h"
#define VERSION_QUEUE "1.2"

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
    if (p->signal == 0) goto again; else { p->signal=0; return 0;};
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
    if (p->signal == 0) goto again; else { p->signal=0; return 0;};
  };
}

static int queue_unlock(queue_t *q)
{
  int i;
  int pid;
  while (q->qawait_tail != q->qawait_head) 
  {
    pid=q->qawait[q->qawait_tail];
    if (pid != NILPID) 
    {
      process_table[pid].signal++;
      process_wakeup(&process_table[pid]);
    };
    q->qawait_tail++; if (q->qawait_tail == MAX_PROC) q->qawait_tail=0;       
  };
  process_schedule();
  return 0;
}
