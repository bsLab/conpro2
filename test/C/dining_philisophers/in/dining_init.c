#include "dining_init.h"
void PRO_init()
{
  int i;
  for(i=0;i<=4;i=i+1){
    fork[i]->init(fork[i],1);
  };
  ev->init(ev);
  process_end();
}
process_t* init;
