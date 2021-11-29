#include "conpro.h"
#include "event.h"
#include "test_types.h"
#include "test_const.h"
#include "test_models.h"
#include "test_objs.h"
#include "test_FUN_f.h"
#include "test_p1.h"
#include "test_p2.h"
#include "test_pa.h"
#include "test_main.h"
#include "mutex.h"
#include "semaphore.h"
#include "process.h"
event_t *_exit;
int conpro(){
  process_init();
  _exit=event_new("");
  _exit->init(_exit);
  x=0;
  c1.real=0;
  c1.imag=0;
  c2.real=0;
  c2.imag=0;
  LOCK_FUN_f = mutex_new("");
  ca=(struct complex *)malloc(100*sizeof(struct complex));
  mu = mutex_new("");
  sm = semaphore_new("init=1");
  a=(int *)malloc(100*sizeof(int));
  p1=process_new("p1",PRO_p1,0);
  p2=process_new("p2",PRO_p2,0);
  pa[0]=process_new("pa_0",PRO_pa,0);
  pa[1]=process_new("pa_1",PRO_pa,1);
  pa[2]=process_new("pa_2",PRO_pa,2);
  pa[3]=process_new("pa_3",PRO_pa,3);
  main=process_new("main",PRO_main,0);
  main->call(main);
  _exit->await(_exit);
}
