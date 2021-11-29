#include "conpro.h"
#include "process.h"

#ifndef __SYSTEMH
#define __SYSTEMH

/*
******************************************
** SYSTEM
******************************************
*/
struct system {
  int Clock;
  int (*clock)(struct system *,int d);
  int (*target)(struct system *,char *str);
  int (*reset_level)(struct system *,int d);
  int (*simu_cycles)(struct system *,int d);
  
};

typedef struct system system_t;

extern system_t *system_new(char *params);

#endif /* __SYSTEMH */
