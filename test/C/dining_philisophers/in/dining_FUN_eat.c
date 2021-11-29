#include "dining_FUN_eat.h"
void eat(int n)
{
  eating[n] = 1; printf("eating=[%d,%d,%d,%d,%d] fork=[%d,%d,%d,%d,%d]\n",
                    eating[0],eating[1],eating[2],eating[3],eating[4],
                    fork[0]->count,fork[1]->count,fork[2]->count,fork[3]->count,fork[4]->count);
  thinking[n] = 0; 
  process_delay(50000);
  eating[n] = 0; 
  thinking[n] = 1; 
}
