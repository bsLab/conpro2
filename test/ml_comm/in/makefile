PROJ=com2
CC=vumc
INCL=-I .
%.cmo: %.ml
	$(CC) -c $(INCL) $*.ml
%.cmi: %.mli
	$(CC) -c $(INCL) $*.mli
clean:
	rm *.cmi *.cmo
core: com2_types.cmo com2_const.cmo com2_models.cmo com2_objs.cmo
int: \
	com2_send.cmi\
	com2_recv.cmi\
	com2_main.cmi
impl: \
	com2_send.cmo \
	com2_recv.cmo \
	com2_main.cmo \
	com2.cmo
com2: core int impl
	vumc -o com2 \
		com2_types.cmo \
		com2_const.cmo \
		com2_models.cmo \
		com2_objs.cmo \
		com2_send.cmo \
		com2_recv.cmo \
		com2_main.cmo \
		com2.cmo
