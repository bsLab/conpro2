PROJ=test
CC=cc
INCL=-I. -I/opt/Conpro2/lib
%.o: %.c
	$(CC) -c $(INCL) $*.c
help:
	@echo "Targets: [core impl clean]"
clean:
	rm -f *.o *.a
core: test_const.o test_models.o test_objs.o
impl: \
	test_FUN_f.o \
	test_p1.o \
	test_p2.o \
	test_pa.o \
	test_main.o \
	test.o
libtest.a: core impl
	ar cv libtest.a \
		test_const.o \
		test_models.o \
		test_objs.o \
		test_FUN_f.o \
		test_p1.o \
		test_p2.o \
		test_pa.o \
		test_main.o \
	test.o
