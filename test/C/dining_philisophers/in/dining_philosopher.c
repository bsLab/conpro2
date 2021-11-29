#include "dining_philosopher.h"
void PRO_philosopher(int process_id)
{
  if (process_id < 4)
  {
    ev->await(ev);
    while(true){
      process_delay(50000);
      fork[process_id]->down(fork[process_id]);
      fork[process_id + 1]->down(fork[process_id + 1]);
      eat(process_id);
      fork[process_id]->up(fork[process_id]);
      fork[process_id + 1]->up(fork[process_id + 1]);
    };
  }
  else
  {
    while(true){
      process_delay(50000);
      fork[4]->down(fork[4]);
      fork[0]->down(fork[0]);
      eat(process_id);
      fork[4]->up(fork[4]);
      fork[0]->up(fork[0]);
    };
  };
  process_end();
}
process_t *philosopher[5];
