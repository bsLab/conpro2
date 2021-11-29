/*
******************************************
** UART Serial Interface
******************************************
*/

#include "uart.h"
#define VERSION_UART "1.1"

static int uart_init(uart_t *);
static int uart_baud(uart_t *, int d);
static int uart_start(uart_t *);
static int uart_stop(uart_t *);
static int uart_read(uart_t *, int *d,int *err);
static int uart_write(uart_t *,int d, int *err);

static struct {
  speed_t speed;
  int baud;
} speedtable[] = {
  {B50,      50},
  {B75,      75},
  {B110,     110},
  {B134,     134},
  {B150,     150},
  {B300,     300},
  {B600,     600},
  {B1200,    1200},
  {B1800,    1800},
  {B2400,    2400},
  {B4800,    4800},
  {B9600,    9600},
  {B19200,   19200},
  {B38400,   38400},
#ifdef B57600
  {B57600,   57600},
#endif
#ifdef B115200
  {B115200,  115200},
#endif
#ifdef B230400
  {B230400,  230400},
#endif
  {B0,       0}
};
#define NSPEEDS (sizeof(speedtable) / sizeof(speedtable[0]))
enum { Bool, Enum, Speed, Char, End };

enum { Input, Output };

static struct termios terminal_status;

#define iflags ((long)(&terminal_status.c_iflag))
#define oflags ((long)(&terminal_status.c_oflag))
#define cflags ((long)(&terminal_status.c_cflag))
#define lflags ((long)(&terminal_status.c_lflag))

/* Number of fields in the terminal_io record field. Cf. unix.mli */

#define NFIELDS 38

/* Structure of the terminal_io record. Cf. unix.mli */
static long terminal_io_descr[] = {
  /* Input modes */
  Bool, iflags, IGNBRK,
  Bool, iflags, BRKINT,
  Bool, iflags, IGNPAR,
  Bool, iflags, PARMRK,
  Bool, iflags, INPCK,
  Bool, iflags, ISTRIP,
  Bool, iflags, INLCR,
  Bool, iflags, IGNCR,
  Bool, iflags, ICRNL,
  Bool, iflags, IXON,
  Bool, iflags, IXOFF,
  /* Output modes */
  Bool, oflags, OPOST,
  /* Control modes */
  Speed, Output,
  Speed, Input,
  Enum, cflags, 5, 4, CSIZE, CS5, CS6, CS7, CS8,
  Enum, cflags, 1, 2, CSTOPB, 0, CSTOPB,
  Bool, cflags, CREAD,
  Bool, cflags, PARENB,
  Bool, cflags, PARODD,
  Bool, cflags, HUPCL,
  Bool, cflags, CLOCAL,
  /* Local modes */
  Bool, lflags, ISIG,
  Bool, lflags, ICANON,
  Bool, lflags, NOFLSH,
  Bool, lflags, ECHO,
  Bool, lflags, ECHOE,
  Bool, lflags, ECHOK,
  Bool, lflags, ECHONL,
  /* Control characters */
  Char, VINTR,
  Char, VQUIT,
  Char, VERASE,
  Char, VKILL,
  Char, VEOF,
  Char, VEOL,
  Char, VMIN,
  Char, VTIME,
  Char, VSTART,
  Char, VSTOP,
  End
};

#undef iflags
#undef oflags
#undef cflags
#undef lflags




static void encode_terminal_status(int *dst)
{
  long * pc;
  int i;

  for(pc = terminal_io_descr; *pc != End; dst++) {
    switch(*pc++) {
    case Bool:
      { int * src = (int *) (*pc++);
        int msk = *pc++;
        *dst = ((*src & msk)==0?0:1);
        break; }
    case Enum:
      { int * src = (int *) (*pc++);
        int ofs = *pc++;
        int num = *pc++;
        int msk = *pc++;
        for (i = 0; i < num; i++) {
          if ((*src & msk) == pc[i]) {
            *dst = (i + ofs);
            break;
          }
        }
        pc += num;
        break; }
    case Speed:
      { int which = *pc++;
        speed_t speed = 0;
        *dst = (9600);   /* in case no speed in speedtable matches */
        switch (which) {
        case Output:
          speed = cfgetospeed(&terminal_status); break;
        case Input:
          speed = cfgetispeed(&terminal_status); break;
        }
        for (i = 0; i < NSPEEDS; i++) {
          if (speed == speedtable[i].speed) {
            *dst = (speedtable[i].baud);
            break;
          }
        }
        break; }
    case Char:
      { int which = *pc++;
        *dst = (int)(terminal_status.c_cc[which]);
        break; }
    }
  }
}

static void decode_terminal_status(int *src)
{
  long * pc;
  int i;

  for (pc = terminal_io_descr; *pc != End; src++) {
    switch(*pc++) {
    case Bool:
      { int * dst = (int *) (*pc++);
        int msk = *pc++;
        if (*src == 1)
          *dst |= msk;
        else
          *dst &= ~msk;
        break; }
    case Enum:
      { int * dst = (int *) (*pc++);
        int ofs = *pc++;
        int num = *pc++;
        int msk = *pc++;
        i = (*src) - ofs;
        if (i >= 0 && i < num) {
          *dst = (*dst & ~msk) | pc[i];
        } else {
          conpro_err("tcsetattr");
        }
        pc += num;
        break; }
    case Speed:
      { int which = *pc++;
        int baud = (*src);
        int res = 0;
        for (i = 0; i < NSPEEDS; i++) {
          if (baud == speedtable[i].baud) {
            switch (which) {
            case Output:
              res = cfsetospeed(&terminal_status, speedtable[i].speed); break;
            case Input:
              res = cfsetispeed(&terminal_status, speedtable[i].speed); break;
            }
            if (res == -1) conpro_err("tcsetattr");
            goto ok;
          }
        }
        conpro_err("tcsetattr");
      ok:
        break; }
    case Char:
      { int which = *pc++;
        terminal_status.c_cc[which] = (*src);
        break; }
    }
  }
}



uart_t * uart_new(char *params) 
{
  uart_t *u;
  int res;
  int Baud=115200;
  int i;

  u=(uart_t*) malloc(sizeof(uart_t));
  mem_check((void*)u,"uart_new");
  u->tio=(struct terminal_io*)malloc(sizeof(struct terminal_io));
  mem_check((void *)u->tio,"uart_new");
  
  i=get_param(params,"baud");
  if (i != -1) Baud=i;
  i=get_params(params,u->dev,"dev");
  if (i== -1) strcpy(u->dev,"/dev/cua/a");
  
  
  u->init=uart_init;
  u->baud=uart_baud;
  u->start=uart_start;
  u->stop=uart_stop;
  u->read=uart_read;
  u->write=uart_write;
  u->Baud=Baud;
  
  return u;    
}
static int uart_start(uart_t *u)
{
  return 0;
}
static int uart_stop(uart_t *u)
{
  return 0;
}
static int uart_baud(uart_t *u, int d)
{
  u->Baud=d;
  return 0;
}

static int uart_init(uart_t *u)
{
  int res;
  u->fd=open(u->dev,(O_RDWR|O_NONBLOCK));
  if (u->fd<=0) conpro_err("[UART]: can't open device");
  res=tcgetattr(u->fd,&terminal_status);  
  if (res!=0) conpro_err ("[UART]: can't get terminal io structure");
  encode_terminal_status((int*)u->tio);  
  u->tio->c_ibaud=u->Baud;
  u->tio->c_obaud=u->Baud;
  u->tio->c_icanon = 0;
  u->tio->c_echo   = 0;
  u->tio->c_echoe  = 0;
  u->tio->c_echok  = 0;
  u->tio->c_echonl = 0;
  u->tio->c_noflsh = 0; 
  u->tio->c_vmin = 1; 

  u->tio->c_ignbrk = 1;
  u->tio->c_brkint = 0;
  u->tio->c_igncr = 1;
  u->tio->c_ixon = 0;
  u->tio->c_ixoff = 0;
  u->tio->c_opost = 0;
  u->tio->c_clocal = 1;
  u->tio->c_isig = 0;
  u->tio->c_istrip = 0;
  u->tio->c_hupcl = 0;   

  u->tio->c_vintr = 0;
  u->tio->c_vquit = 0;
  u->tio->c_verase = 0;
  u->tio->c_vkill = 0;
  u->tio->c_veof = 0;
  u->tio->c_veol = 0;
  
  decode_terminal_status((int*)u->tio);
  res=tcsetattr(u->fd,TCSANOW,&terminal_status);
  if (res!=0) conpro_err ("[UART]: can't set terminal io structure");
  
  return 0;
}

static int uart_read(uart_t *u, int * d, int *err)
{
  char buf[8];
  int res;
again:
  res=read(u->fd,buf,1);
  if (res == -1 && errno == EINTR) goto again;
  if (res == -1 && (errno == EAGAIN || errno == EWOULDBLOCK))
  {
    process_await_io(process_self(),0,'r',u->fd);
    goto again;
  };
  if (res == 1) 
  {
    *d=(int)((unsigned int)buf[0]);
    *err=0;
  }
  else
    *err=1;
  return 1;
}

static int uart_write(uart_t *u,int d, int *err)
{
  char buf[8];
  int res;
again:
  buf[0]=(char)((unsigned int)d);
  res=write(u->fd,buf,1);
  if (res == -1 && errno == EINTR) goto again;
  if (res == -1 && (errno == EAGAIN || errno == EWOULDBLOCK))
  {
    process_await_io(process_self(),0,'w',u->fd);
    goto again;
  };
  if (res==1) *err=0; else *err=1;
  return res;
}
