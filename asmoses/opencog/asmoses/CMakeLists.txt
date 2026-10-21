# The atom_types.h file is written to the build directory)
INCLUDE_DIRECTORIES(
		..
		${CMAKE_CURRENT_BINARY_DIR}
		${CMAKE_BINARY_DIR}
)
DECLARE_GUILE_CONFIG_TARGET(SCM_CONFIG "opencog asmoses-config" "ASMOSES_TEST")

ADD_SUBDIRECTORY (combo)
ADD_SUBDIRECTORY (moses)
ADD_SUBDIRECTORY (feature-selection)
ADD_SUBDIRECTORY (atomese)
ADD_SUBDIRECTORY (data)
ADD_SUBDIRECTORY (reduct)
ADD_SUBDIRECTORY (utils)

# Currently, the pleasure code does not build, as it uses various
# obsolete combo types and routines and etc.  It needs to be ported
# over to the modern combo interfaces.
#
#ADD_SUBDIRECTORY (pleasure)

IF (HAVE_CYTHON)
	ADD_SUBDIRECTORY (cython)
ENDIF (HAVE_CYTHON)

IF (HAVE_ATOMSPACE AND HAVE_GUILE )
	ADD_SUBDIRECTORY (urereduct)
ENDIF (HAVE_ATOMSPACE AND HAVE_GUILE )

# Much of the tree includes the generated atomese/atom_types/atom_types.h,
# directly or through headers such as combo_atomese.h, so every library and
# program waits for it; otherwise parallel builds can compile them first.
FUNCTION(ASMOSES_DEPEND_ON_ATOM_TYPES dir)
	GET_PROPERTY(targets DIRECTORY ${dir} PROPERTY BUILDSYSTEM_TARGETS)
	FOREACH(target ${targets})
		GET_TARGET_PROPERTY(type ${target} TYPE)
		IF (type MATCHES "^(SHARED_LIBRARY|STATIC_LIBRARY|MODULE_LIBRARY|OBJECT_LIBRARY|EXECUTABLE)$")
			ADD_DEPENDENCIES(${target} asmoses_atom_types)
		ENDIF ()
	ENDFOREACH()
	GET_PROPERTY(subdirs DIRECTORY ${dir} PROPERTY SUBDIRECTORIES)
	FOREACH(subdir ${subdirs})
		ASMOSES_DEPEND_ON_ATOM_TYPES(${subdir})
	ENDFOREACH()
ENDFUNCTION()
ASMOSES_DEPEND_ON_ATOM_TYPES(${CMAKE_CURRENT_SOURCE_DIR})


WRITE_GUILE_CONFIG(${GUILE_BIN_DIR}/opencog/asmoses-config.scm SCM_CONFIG TRUE)
WRITE_GUILE_CONFIG(${GUILE_BIN_DIR}/opencog/asmoses-config-installable.scm SCM_CONFIG FALSE)
INSTALL(FILES ${GUILE_BIN_DIR}/opencog/asmoses-config-installable.scm DESTINATION ${GUILE_SITE_DIR}/opencog RENAME asmoses-config.scm)

