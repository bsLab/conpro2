PROJ=com
CC=cc
INCL=-I. -I/opt/Conpro2/lib
%.o: %.c
	$(CC) -c $(INCL) $*.c
help:
	@echo "Targets: [core impl clean all]"
clean:
	rm -f *.o *.a
core: com_const.o com_models.o com_objs.o
impl: \
	com_sys_status_proc.o \
	com_com_tmo.o \
	com_com_rx.o \
	com_com_tx.o \
	com_interpreter.o \
	com_FUN_request.o \
	com_main.o \
	com.o
libcom.a: core impl
	ar rcv libcom.a \
		com_const.o \
		com_models.o \
		com_objs.o \
		com_sys_status_proc.o \
		com_com_tmo.o \
		com_com_rx.o \
		com_com_tx.o \
		com_interpreter.o \
		com_FUN_request.o \
		com_main.o \
	com.o
all: libcom.a
com: main.o libcom.a
	$(CC) -o com main.c libcom.a
