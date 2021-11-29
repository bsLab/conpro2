PROJ=dining
CC=cc
INCL=-I. -I/opt/Conpro2/lib
%.o: %.c
	$(CC) -c $(INCL) $*.c
help:
	@echo "Targets: [core impl clean all]"
clean:
	rm -f *.o *.a
core: dining_const.o dining_models.o dining_objs.o
impl: \
	dining_init.o \
	dining_FUN_eat.o \
	dining_philosopher.o \
	dining_main.o \
	dining.o
libdining.a: core impl
	ar rcv libdining.a \
		dining_const.o \
		dining_models.o \
		dining_objs.o \
		dining_init.o \
		dining_FUN_eat.o \
		dining_philosopher.o \
		dining_main.o \
	dining.o
all: libdining.a
dining: main.o libdining.a
	$(CC) -o dining main.c libdining.a
