#include "conpro.h"
#include "process.h"

#ifndef __UARTH
#define __UARTH

/*
******************************************
** UART Serial Interface
******************************************
*/
struct terminal_io {
     int c_ignbrk;
     int c_brkint;
     int c_ignpar;
     int c_parmrk;
     int c_inpck;
     int c_istrip;
     int c_inlcr;
     int c_igncr;
     int c_icrnl;
     int c_ixon;
     int c_ixoff;
     int c_opost;
     int c_obaud;
     int c_ibaud;
     int c_csize;
     int c_cstopb;
     int c_cread;
     int c_parenb;
     int c_parodd;
     int c_hupcl;
     int c_clocal;
     int c_isig;
     int c_icanon;
     int c_noflsh;
     int c_echo;
     int c_echoe;
     int c_echok;
     int c_echonl;
     int c_vintr;
     int c_vquit;
     int c_verase;
     int c_vkill;
     int c_veof;
     int c_veol;
     int c_vmin;
     int c_vtime;
     int c_vstart;
     int c_vstop;
};

struct uart {
  char dev[80];
  int Baud;
  int fd;
  struct terminal_io *tio;
  int (*init)(struct uart *);
  int (*baud)(struct uart *, int d);
  int (*start)(struct uart *);
  int (*stop)(struct uart *);
  int (*read)(struct uart *, int *d, int *err);
  int (*write)(struct uart *, int d, int *err);
};

typedef struct uart uart_t;

extern uart_t * uart_new(char *params);

#endif /* __UARTH */
