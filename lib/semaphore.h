#include "conpro.h"
#include "process.h"

#ifndef __SEMAH
#define __SEMAH

/*
******************************************
** SEMAPHORE
******************************************
*/
struct semaphore {
  int qawait[MAX_PROC];
  int qawait_head,qawait_tail;
  int count;
  int (*init)(struct semaphore *,int d);
  int (*down)(struct semaphore *);
  int (*up)(struct semaphore *);
};

typedef struct semaphore semaphore_t;

extern semaphore_t *semaphore_new(char *params);

#endif /* __SEMH */
