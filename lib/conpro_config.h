/*
** Targets:
**  LINUX,SOL9,SOL10
*/


#ifndef __CONFIG_H
#define __CONFIG_H

#ifdef LINUX
#define H_STDIO
#define H_ERRNO
#define H_STDLIB
#define H_UCONTEXT
#define H_SYS_TIME
#define H_SYS_TYPES
#define H_SYS_STAT
#define H_FCNTL
#define H_SYS_FCNTL
#define H_TERMIOS
#define H_STRING
#define H_UNISTD
#define H_STDINT
#define H_SYS_SOCKET
#define H_SYS_UN
#endif

#ifdef LINUX64
#define H_STDIO
#define H_ERRNO
#define H_STDLIB
#define H_UCONTEXT
#define H_SYS_TIME
#define H_SYS_TYPES
#define H_SYS_STAT
#define H_FCNTL
#define H_SYS_FCNTL
#define H_TERMIOS
#define H_STRING
#define H_UNISTD
#define H_STDINT
#define H_SYS_SOCKET
#define H_SYS_UN
#endif

#ifdef SOL9
#define H_STDIO
#define H_ERRNO
#define H_STDLIB
#define H_UCONTEXT
#define H_SYS_TIME
#define H_SYS_TYPES
#define H_SYS_STAT
#define H_FCNTL
#define H_SYS_FCNTL
#define H_TERMIOS
#define H_STRING
#define H_UNISTD
#undef H_STDINT
#define H_SYS_SOCKET
#define H_SYS_UN
#define _SYS_STREAM_H
#endif

#ifdef SOL10
#define H_STDIO
#define H_ERRNO
#define H_STDLIB
#define H_UCONTEXT
#define H_SYS_TIME
#define H_SYS_TYPES
#define H_SYS_STAT
#define H_FCNTL
#define H_SYS_FCNTL
#define H_TERMIOS
#define H_STRING
#define H_UNISTD
#undef H_STDINT
#define H_SYS_SOCKET
#define H_SYS_UN
#define _SYS_STREAM_H
#endif

#endif /* !__CONFIG_H */
