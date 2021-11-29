#include "conpro.h"
#include "process.h"

#ifndef __MUTEXH
#define __MUTEXH

/*
******************************************
** MUTEX
******************************************
*/
struct mutex {
  int qawait[MAX_PROC];
  int qawait_head,qawait_tail;
  int owner;
  int (*lock)(struct mutex*);
  int (*unlock)(struct mutex*);
  int (*init)(struct mutex*);
};

typedef struct mutex mutex_t;

extern mutex_t *mutex_new(char *params);

#endif /* __MUTEXH */
