/*
******************************************
** IOPORT
******************************************
*/

#include "ioport.h"
#define VERSION_IOPORT "1.1"

static int ioport_init(ioport_t *io);
static int ioport_dir(ioport_t *io,int d);
static int ioport_read(ioport_t *io,int *d);
static int ioport_write(ioport_t *io,int d);

ioport_t *ioport_new(char *params)
{
  int i;
  int width=8;
  ioport_t *io;

  i=get_param(params,"datawidth");
  if (i != -1) width=i;
  
  io=(ioport_t *)malloc(sizeof(ioport_t));
  mem_check((void*)io,"ioport_new");

  io->width=width;
  io->port_dir=0;
  io->port_reg=0;
  io->init=ioport_init;
  io->dir=ioport_dir;
  io->read=ioport_read;
  io->write=ioport_write;
   
  return io;
}


static int ioport_init(ioport_t *io)
{
  process_t *p=process_self();
  io->port_dir=0;
  io->port_reg=0;
  return 0;
}

static int ioport_dir(ioport_t *io,int d)
{
  process_t *p=process_self();
  io->port_dir=d;
  return 0;
}

static int ioport_read(ioport_t *io,int *d)
{
  process_t *p=process_self();
  *d=io->port_reg;
  return 0;
}

static int ioport_write(ioport_t *io,int d)
{
  process_t *p=process_self();
  io->port_reg=d;
  return 0;
}


