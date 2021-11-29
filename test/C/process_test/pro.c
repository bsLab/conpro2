#include "conpro.h"
#include "process.h"
#include "event.h"
#include "queue.h"
#include "timer.h"
#include "mutex.h"
#include "semaphore.h"
event_t *evm,*ev12;
queue_t *q;
timers_t *t;
mutex_t *m;
process_t *P1,*P2;


void ep1()
{
  int i;
  for(i=0;i<10;i++)
  {
      ev12->await(ev12);
      printf("EP1\n");
  };
  process_end();
};

void ep2()
{
  int i;
  for(i=0;i<10;i++)
  {
    printf("EP2\n");
    ev12->wakeup(ev12);
  };
  evm->wakeup(evm);
  process_end();
};

void qp1()
{
  int i,x;
  for(i=0;i<10;i++)
  {
      printf("QP1:read...\n",x);
      x=q->read(q);
      printf("QP1:%d\n",x);
  };
  evm->wakeup(evm);
  process_end();
};

void qp2()
{
  int i;
  for(i=0;i<10;i++)
  {
    printf("QP2\n");
    q->write(q,i);
  };
  process_end();
};

void tp1()
{
  int i,x;
  for(i=0;i<10;i++)
  {
      printf("TP1:wait...\n",x);
      t->await(t);
      printf("TP1:%d\n",i);
  };
  process_end();
};

void tp2()
{
  int i;
  for(i=0;i<10;i++)
  {
    printf("TP2:wait...\n");
    t->await(t);
    printf("TP2:%d\n",i);
    if (i==4) P1->stop(P1);
  };
  evm->wakeup(evm);
  process_end();
};

int xg=0;

void mp1(int arg)
{
  int i,x;
  printf("MP1:# %d\n",arg);
  for(i=0;i<10;i++)
  {
      printf("MP1:%d:lock...\n",i);
      m->lock(m); 
      printf("MP1:%d:got lock...\n",i);
      x=xg; process_schedule();
      x=x+1; process_schedule();
      xg=x; process_schedule();
      printf("MP1:%d:unlock xg=%d\n",i,xg);
      m->unlock(m); 
  };
  process_end();
};

void mp2(int arg)
{
  int i,x;
  printf("MP2:# %d\n",arg);
  for(i=0;i<10;i++)
  {
      printf("MP2:%d:lock...\n",i);
      m->lock(m); 
      printf("MP2:%d:got lock...\n",i);
      x=xg; process_schedule();
      x=x-1; process_schedule();
      xg=x; process_schedule();
      printf("MP2:%d:unlock xg=%d\n",i,xg);
      m->unlock(m); 
  };
  evm->wakeup(evm);
  process_end();
};
#define E1 1
#define E2 2

void exc()
{
  int x;
  x=0;
  try(1)
  {
    x=x+1;
    raise(E1);
    x=x+1;
  };
  with(1)
  {
    case E1: x=99; break;
  };
  with_end(1);
  printf("exc: after try-with: %d\n",x);
};

int bit_test()
{
  int x;
  int64 y;
  
  y=Int64.of_string("0x1234567890abcdef");
  x=0xaaaa;
  printf("bit(1,5,x)=%x\n",bit(1,5,x));
  printf("bit(11,11,x)=%x\n",bit(11,11,x));
  printf("bitw(6,6,x,1)=%x\n",bitw(6,6,x,1));
  printf("bit64(0,48,y)=%s\n",
      Int64.format("%x",bit64(0,48,y)));
  exit(0);
};
int main()
{
  process_init ();
  bit_test();
  
  evm=event_new("");
  ev12=event_new("");
  q=queue_new("size=4");
  t=timer_new("time=100000:mode=0");
  t->start(t);
  m=mutex_new("");

  
  exc();
    
  printf("PM 1\n");
  P1=process_new("p1",mp1,1);
  P2=process_new("p2",mp2,2);
  P1->start(P1);
  P2->start(P2);
  printf("PM 2\n");
  
  evm->await(evm);

  printf("PM 3\n");
  return 0; 
  
};
  
