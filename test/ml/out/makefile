PROJ=dining
CC=vumc
INCL=-I .
%.cmo: %.ml
	$(CC) -c $(INCL) $*.ml
%.cmi: %.mli
	$(CC) -c $(INCL) $*.mli
help:
	@echo "Targets: [core impl clean all]"
clean:
	rm -f *.cmi *.cmo
core: dining_types.cmi dining_const.cmi dining_models.cmi dining_objs.cmi \
	dining_types.cmo dining_const.cmo dining_models.cmo dining_objs.cmo
int: \
	dining_init.cmi\
	dining_FUN_eat.cmi\
	dining_philosopher.cmi\
	dining_main.cmi
impl: \
	dining_init.cmo \
	dining_FUN_eat.cmo \
	dining_philosopher.cmo \
	dining_main.cmo \
	dining.cmo
dining: core int impl
	vumc -o dining \
		dining_types.cmo \
		dining_const.cmo \
		dining_models.cmo \
		dining_objs.cmo \
		dining_init.cmo \
		dining_FUN_eat.cmo \
		dining_philosopher.cmo \
		dining_main.cmo \
		dining.cmo
