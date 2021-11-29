FILES:

@ * l2.cp
@ * link4.mod

@ bug-report


PROJECT:
  PROJ_PATH $HOME/proj/conpro2/test/link4
  SRC_PATH $PROJ_PATH
  VHDL_DIR src
  CONPRO_DIR src
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

