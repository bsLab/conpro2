FILES:

@ * arith.cp
@ * com1.cp
@ * interp.cp
@ * common.cp
@ * const.cp
@ * status.cp
@ * top.cp

@ bug-report


PROJECT:
  PROJ_PATH $HOME/proj/conpro2/test/com1
  SRC_PATH $PROJ_PATH/src
  VHDL_DIR vhdl
  CONPRO_DIR cp
  OBJ_PATH $PROJ_PATH/build
  CONPRO_PATH /opt/Conpro2
  VUM_PATH /opt/Vum-2.1

COMMANDS:
  OPEN ALL
  CLOSE ALL
  BUILD CONPRO
  BUILD SYNTH
  BUILD TECH
  BUILD ALL
  BUILD CLEAN
  BUILD INSTALL
  BUILD VERSION


