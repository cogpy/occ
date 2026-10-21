
# The visualizer is stand-alone.
ADD_SUBDIRECTORY (gtk-visualizer)

# The dotty and tulip CogServer modules are written against an older
# AtomSpace and CogServer API (get_handleset_by_type, output-iterator
# getIncomingSet, a global `server()`), so they no longer compile.
OPTION(BUILD_COGSERVER_MODULES "Build the dotty and tulip CogServer modules" OFF)
IF (BUILD_COGSERVER_MODULES)
	ADD_SUBDIRECTORY (dotty)
	ADD_SUBDIRECTORY (tulip)
ENDIF (BUILD_COGSERVER_MODULES)
