FILES:

@ * agent_node.cp
@ * agent_link.cp
@ * agent_manager.cp
@ * agent_smart_message.cp

@ build


PROJECT:
  PROJ_PATH $HOME/publications/spie2013/experiments
  SRC_PATH $PROJ_PATH
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


