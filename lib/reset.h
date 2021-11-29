#include "conpro.h"
#include "process.h"

#ifndef __RESETH
#define __RESETH

/*
******************************************
** RESET
******************************************
*/
struct reset {
  int Action;
  int (*action)(struct reset *,int d);
  int (*source)(struct reset *,char *str);
  int (*add)(struct reset *,int *d);
  
};

typedef struct reset reset_t;

extern reset_t *reset_new(char *params);

#endif /* __RESETH */
