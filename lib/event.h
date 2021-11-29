#include "conpro.h"
#include "process.h"

#ifndef __EVENTH
#define __EVENTH

/*
******************************************
** EVENT
******************************************
*/
struct event {
  int qawait[MAX_PROC];
  int waiting;
  int latched;
  int latch;
  int (*init)(struct event *);
  int (*await)(struct event *);
  int (*wakeup) (struct event *);
};

typedef struct event event_t;

extern event_t *event_new(char *params);

#endif /* __EVENTH */
