#include "conpro.h"
#include "process.h"

#ifndef __CLOCKH
#define __CLOCKH

/*
******************************************
** CLOCK
******************************************
*/
struct cp_clock {
  int Clock;
  int (*action)(struct cp_clock *,int d);
  int (*clock)(struct cp_clock *,int d);
  int (*source)(struct cp_clock *,char *str);
  int (*add)(struct cp_clock *,int *d);
  
};

typedef struct cp_clock cp_clock_t;

extern cp_clock_t *clock_new(char *params);

#endif /* __CLOCKH */
