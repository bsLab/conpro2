#
# TOP path
#
PROJ=conpro2
TOP=/home/sbosse/proj/$(PROJ)
LOG=$(TOP)/update.log

#
# Utils
#

CP=cp
MKDIR=mkdir
TAR=tar
RM=rm
VUM=
REV=/usr/bin/revision
DATE := $(shell date)
SHELL = /bin/bash
ARCH := /Cloud/Revision/conpro2/$(PROJ).diff*.tgz
STATE := /home/sbosse/revision/$(PROJ).state
DATE := $(shell perl -MPOSIX=strftime -le 'print strftime("%y%m%d%H%M", localtime((stat shift)[9]))' $(STATE))
DATES := $(shell perl -MPOSIX=strftime -le 'print strftime("%y%m%d@%H%M", localtime((stat shift)[9]))' $(STATE))

help:
	@echo "Targets: [backup,diff,sync]"
        
check: $(ARCH)
	$(RM) -f update
	@cd $(TOP)/..;echo "Last sync: $(DATES)";for file in $(ARCH);\
        do\
          F=$${file##*/};\
          F1=$${F%.*};\
          F2=$${F1##*.};\
          DATE2=$${F2/@/};\
          if [[ $$DATE2 > $(DATE) ]];\
          then\
            echo "Apply update $$file ? [y/n]";\
            read answer;\
            if [ "X$$answer" = "Xn" ];\
            then\
              echo "Skipping $$file"; \
            else\
	      touch $(TOP)/update;\
              echo "Applying update $$file!";\
              gtar -U -zxvf $$file;\
            fi;\
          fi;\
        done;\
        cd $(TOP);\
	if test -f update;\
        then\
	  $(VUM) $(REV) -freeze;\
	  touch diff;\
	  touch update;\
	fi

all:
	cd $(TOP)/doc; build all install
	cd $(TOP)/lib; build all install
	cd $(TOP)/toolset; build all install
	cd $(TOP)/build/ml; build all install
	cd $(TOP)/scripts; build all install
	cd $(TOP)/build/bin; build all install
clean:
	cd $(TOP)/lib; build clean
	cd $(TOP)/toolset; build clean
	cd $(TOP)/build/ml; build clean
	
sync:
	$(VUM) $(REV) -freeze
	touch diff
	touch update

backup:
	$(VUM) $(REV) -backup
	touch diff
	touch update

diff: 
	$(VUM) $(REV) -compare
	touch diff
	touch update
        
