#include "conpro.h"
#include "process.h"

#ifndef __IOPORTH
#define __IOPORTH

/*
******************************************
** IOPORT
******************************************
*/
struct ioport {
  int width;
  int port_reg;
  int port_dir;
  int (*init)(struct ioport *);
  int (*dir)(struct ioport *, int d);
  int (*read) (struct ioport *, int *d);
  int (*write) (struct ioport *, int d);
};

typedef struct ioport ioport_t;

extern ioport_t *ioport_new(char *params);

#endif /* __IOPORTH */
