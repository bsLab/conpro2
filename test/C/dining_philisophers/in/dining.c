#include "dining_types.h"
#include "dining_const.h"
#include "dining_models.h"
#include "dining_objs.h"
#include "dining_init.h"
#include "dining_FUN_eat.h"
#include "dining_philosopher.h"
#include "dining_main.h"
event_t *_exit;
int conpro(){
  process_init();
  _exit=event_new("");
  _exit->init(_exit);
  sys = system_new("");
  ev = event_new("");
  thinking=(int *)malloc(5*sizeof(int));
  status=' ';
  fork[0] = semaphore_new("depth=8:scheduler=fifo");
  fork[1] = semaphore_new("depth=8:scheduler=fifo");
  fork[2] = semaphore_new("depth=8:scheduler=fifo");
  fork[3] = semaphore_new("depth=8:scheduler=fifo");
  fork[4] = semaphore_new("depth=8:scheduler=fifo");
  eating=(int *)malloc(5*sizeof(int));
  init=process_new("init",PRO_init,0);
  philosopher[0]=process_new("philosopher_0",PRO_philosopher,0);
  philosopher[1]=process_new("philosopher_1",PRO_philosopher,1);
  philosopher[2]=process_new("philosopher_2",PRO_philosopher,2);
  philosopher[3]=process_new("philosopher_3",PRO_philosopher,3);
  philosopher[4]=process_new("philosopher_4",PRO_philosopher,4);
  Main=process_new("Main",PRO_Main,0);
  sys->simu_cycles(sys,500);
  Main->call(Main);
  _exit->await(_exit);
}
