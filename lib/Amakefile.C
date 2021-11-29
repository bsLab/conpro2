
# version 1.01

VUMDIR=/opt/Vum-2.4;
%include $VUMDIR/Amakefile.sys;
%include $VUMDIR/Amakefile.common;
%include $VUMDIR/toolset/ocaml.lib;


#
# Src list file
#
%include Amake.srclist;

CINCLUDES = {
    -I./,
    -I$VUMDIR/src/ocaml/byterun,
    -include,conpro_config.h,
    -DSOL9,
};

CSRC = {
    $CONPRO_C_SRC
};

OCAML_STD_INC ={};
VUM_STD_INC = {};
VUM_CC_STDINC = {};

CLIB = libconpro.a;

VMNAME = ocamlrun;

#
# Build a C library from the sources.
#

%generic clibcluster(clib,csrc)
{
    %default $clib;
    %cluster 
    {
        %targets    $clib;
        %sources    $csrc;
        %use        ar-cc();
    };
};


#
# And finally the byterun library and VM
#

%instance clibcluster ($CLIB,$CSRC);

