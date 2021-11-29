#include "dining_main.h"
void PRO_Main()
{
  int i;
  status = 'I'; X("status",(int)status);
  init->call(init);
  status = 'S'; X("status",(int)status);
  for(i=0;i<=4;i=i+1){
    philosopher[i]->start(philosopher[i]);
  };
  status = 'W'; X("status",(int)status);
  ev->wakeup(ev);
  process_end();
}
process_t* Main;
