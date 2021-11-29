##
##      ==================================
##      OOOO   OOOO OOOO  O      O   OOOO
##      O   O  O    O     O     O O  O   O
##      O   O  O    O     O     O O  O   O
##      OOOO   OOOO OOOO  O     OOO  OOOO
##      O   O     O    O  O    O   O O   O
##      O   O     O    O  O    O   O O   O
##      OOOO   OOOO OOOO  OOOO O   O OOOO
##      ================================== 
##      BSSLAB, Dr. Stefan Bosse sci@bsslab.de
##
##    PROTECTED BY AND DISTRIBUTED UNDER THE TERMS OF: 
##    Free Software Foundation-Europe, GNU GPL License, Version 2
##
##    $MODIFIEDBY:  BSSLAB
##    $AUTHORS:     Stefan Bosse
##    $INITIAL:     (C) 2003 BSSLAB
##    $CREATED:     
##    $MODIFIED:    
##    $VERSION:     1.05
##
##    $INFO:
##
##  VUM Bytecode Program Amakefile template
##
##  Don't forget to create in the current directory the 'interface'
##  directory!
##
##    $ENDOFINFO
##



TOP = /opt/Vum-2.4;

%include $TOP/Amakefile.sys;
%include $TOP/Amakefile.common;


#
# Src list file
#
%include Amake.srclist;


#
# Include directories
#

INCLUDES = {
    -I,
    $VUMDIR/interface,
};


DEFINES = {
    -g,
};

VUM_PP='';
VUM_OPT = '';

#
# First build a library containg program code except
# the main module!
#
SRC = {
    $CONPRO_ML_SRC,
};

LIB = conpro.cma;

%include $VUMDIR/toolset/ocaml.lib;
%include $VUMDIR/toolset/ocaml.sys;

%instance libcluster($LIB,$SRC);


