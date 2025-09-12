#
# OpenCog Collection (OCC) - Root CMakeLists.txt
# Monorepo integration for cognitive synergy
#
# This CMakeLists.txt coordinates building the core OpenCog components
# in the optimal order for integration.
#

CMAKE_MINIMUM_REQUIRED(VERSION 3.12)

PROJECT(opencog-collection)

# Set default build type
IF (CMAKE_BUILD_TYPE STREQUAL "")
	SET(CMAKE_BUILD_TYPE Release)
ENDIF (CMAKE_BUILD_TYPE STREQUAL "")

MESSAGE(STATUS "OpenCog Collection build type: ${CMAKE_BUILD_TYPE}")

# Global compilation flags
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# Options for selective building
OPTION(BUILD_COGUTIL "Build CogUtil library" ON)
OPTION(BUILD_ATOMSPACE "Build AtomSpace hypergraph database" ON)  
OPTION(BUILD_COGSERVER "Build CogServer networking" ON)
OPTION(BUILD_MATRIX "Build Matrix sparse vector support" ON)
OPTION(BUILD_LEARN "Build symbolic learning" ON)
OPTION(BUILD_AGENTS "Build interactive agents" ON)
OPTION(BUILD_SENSORY "Build sensory dataflow system" ON)

# External integration packages
OPTION(BUILD_GNUCASH "Build Gnucash cognitive accounting integration" OFF)
OPTION(BUILD_KOBOLDCPP "Build KoboldCpp story/world modeling integration" OFF)
OPTION(BUILD_APHRODITE "Build Aphrodite Engine LLM inference integration" OFF)

# Core components built in dependency order
IF(BUILD_COGUTIL)
    MESSAGE(STATUS "Building CogUtil...")
    add_subdirectory(cogutil)
ENDIF()

IF(BUILD_ATOMSPACE AND BUILD_COGUTIL)
    MESSAGE(STATUS "Building AtomSpace...")
    add_subdirectory(atomspace)
ENDIF()

IF(BUILD_COGSERVER AND BUILD_ATOMSPACE)
    MESSAGE(STATUS "Building CogServer...")
    add_subdirectory(cogserver)
ENDIF()

IF(BUILD_MATRIX AND BUILD_ATOMSPACE)
    MESSAGE(STATUS "Building Matrix...")
    add_subdirectory(matrix)
ENDIF()

IF(BUILD_LEARN AND BUILD_ATOMSPACE)
    MESSAGE(STATUS "Building Learn...")
    add_subdirectory(learn)
ENDIF()

IF(BUILD_AGENTS AND BUILD_ATOMSPACE)
    MESSAGE(STATUS "Building Agents...")
    add_subdirectory(agents)
ENDIF()

IF(BUILD_SENSORY AND BUILD_ATOMSPACE)
    MESSAGE(STATUS "Building Sensory...")
    add_subdirectory(sensory)
ENDIF()

# External integration packages
IF(BUILD_GNUCASH AND BUILD_ATOMSPACE)
    MESSAGE(STATUS "Building Gnucash cognitive accounting integration...")
    add_subdirectory(gnucash)
ENDIF()

IF(BUILD_KOBOLDCPP AND BUILD_ATOMSPACE)
    MESSAGE(STATUS "Building KoboldCpp story/world modeling integration...")
    add_subdirectory(koboldcpp)
ENDIF()

IF(BUILD_APHRODITE AND BUILD_ATOMSPACE)
    MESSAGE(STATUS "Building Aphrodite Engine LLM inference integration...")
    add_subdirectory(aphrodite-engine)
ENDIF()

# Optional: Additional AtomSpace extensions
OPTION(BUILD_ATOMSPACE_STORAGE "Build AtomSpace storage backends" OFF)
IF(BUILD_ATOMSPACE_STORAGE AND BUILD_ATOMSPACE)
    add_subdirectory(atomspace-storage)
    add_subdirectory(atomspace-rocks)
    add_subdirectory(atomspace-cog)
ENDIF()

OPTION(BUILD_ATOMSPACE_EXTENSIONS "Build AtomSpace language extensions" OFF)
IF(BUILD_ATOMSPACE_EXTENSIONS AND BUILD_ATOMSPACE)
    add_subdirectory(atomspace-metta)
    add_subdirectory(atomspace-bridge)
ENDIF()

# Print summary
MESSAGE(STATUS "")
MESSAGE(STATUS "OpenCog Collection Configuration Summary:")
MESSAGE(STATUS "========================================")
MESSAGE(STATUS "Build type: ${CMAKE_BUILD_TYPE}")
MESSAGE(STATUS "CogUtil: ${BUILD_COGUTIL}")
MESSAGE(STATUS "AtomSpace: ${BUILD_ATOMSPACE}")
MESSAGE(STATUS "CogServer: ${BUILD_COGSERVER}")
MESSAGE(STATUS "Matrix: ${BUILD_MATRIX}")
MESSAGE(STATUS "Learn: ${BUILD_LEARN}")
MESSAGE(STATUS "Agents: ${BUILD_AGENTS}")
MESSAGE(STATUS "Sensory: ${BUILD_SENSORY}")
MESSAGE(STATUS "AtomSpace Storage: ${BUILD_ATOMSPACE_STORAGE}")
MESSAGE(STATUS "AtomSpace Extensions: ${BUILD_ATOMSPACE_EXTENSIONS}")
MESSAGE(STATUS "")
MESSAGE(STATUS "External Integration Packages:")
MESSAGE(STATUS "Gnucash (cognitive accounting): ${BUILD_GNUCASH}")
MESSAGE(STATUS "KoboldCpp (story/world modeling): ${BUILD_KOBOLDCPP}")
MESSAGE(STATUS "Aphrodite Engine (LLM inference): ${BUILD_APHRODITE}")
MESSAGE(STATUS "")