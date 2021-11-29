#
# TOP path
#

TOP=/home/sbosse/publications/conpro2_book

#
# Utils
#
CP=cp
MKDIR=mkdir
TAR=tar
RM=rm
VUM=vum
REV=revision

help:
	@echo "Targets: [backup,diff,sync]"
        
sync:
	$(VUM) $(REV) -freeze
	touch diff

backup:
	$(VUM) $(REV) -backup
	touch diff

diff: 
	$(VUM) $(REV) -compare
	touch diff
        
    
    
    
