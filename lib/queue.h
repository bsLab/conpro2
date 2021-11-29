#include "conpro.h"
#include "process.h"

#ifndef __QUEUEH
#define __QUEUEH

/*
******************************************
** QUEUE
******************************************
*/
struct queue {
  /*
  ** Queue management
  */
  int size;
  int head,tail;
  int full;
  int empty;
  int *data;
  /*
  ** Process management
  */
  int qawait[MAX_PROC];
  /*
  ** Timeout [microsec]
  */
  int timeout[MAX_PROC];
  int qawait_head;
  int qawait_tail;
  
  int (*write)(struct queue *,int d);
  int (*read)(struct queue *);
  int (*unlock)(struct queue *);
};

typedef struct queue queue_t;
extern queue_t *queue_new(char *params);

#endif /* __QUEUEH */
