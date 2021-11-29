#ifndef __com_types
#define __com_types
struct dev_io_t {
  int RX;
  int TX;
};
typedef enum { 
  STATUS_OK=1,
  STATUS_ERR=2,
  STATUS_ACT=3,
  STATUS_EV=4,
  STATUS_DOWN=5
} status_types;
#endif /* !__com_types */
