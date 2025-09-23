cmake_minimum_required(VERSION 3.16)
project(agentzero-planning VERSION 0.1.0 DESCRIPTION "Agent-Zero Planning Module with Spatial-Temporal Integration")

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

message(STATUS "Configuring Agent-Zero Planning Module (AZ-SPATIAL-001)")

# Dependencies
find_package(PkgConfig REQUIRED)
find_package(CogUtil REQUIRED)
find_package(AtomSpace REQUIRED)

# Find spacetime - it should be available as it's in the cognitive layer
find_path(SPACETIME_INCLUDE_DIR
    NAMES opencog/spacetime/octomap/TimeOctomap.h
    PATHS
        /usr/local/include
        /usr/include
        ${CMAKE_PREFIX_PATH}/include
    NO_DEFAULT_PATH
)

find_library(SPACETIME_LIBRARY
    NAMES spacetime
    PATHS
        /usr/local/lib
        /usr/lib
        ${CMAKE_PREFIX_PATH}/lib
    NO_DEFAULT_PATH
)

if(SPACETIME_INCLUDE_DIR AND SPACETIME_LIBRARY)
    message(STATUS "Found spacetime library: ${SPACETIME_LIBRARY}")
    message(STATUS "Found spacetime includes: ${SPACETIME_INCLUDE_DIR}")
    set(SPACETIME_FOUND TRUE)
else()
    message(WARNING "Spacetime library not found - building without spacetime integration")
    set(SPACETIME_FOUND FALSE)
endif()

# Find octomap
find_package(PkgConfig QUIET)
if(PKG_CONFIG_FOUND)
    pkg_check_modules(OCTOMAP octomap)
endif()

if(NOT OCTOMAP_FOUND)
    find_path(OCTOMAP_INCLUDE_DIR 
        NAMES octomap/octomap.h
        PATHS /usr/local/include /usr/include
    )
    find_library(OCTOMAP_LIBRARY 
        NAMES octomap
        PATHS /usr/local/lib /usr/lib
    )
    if(OCTOMAP_INCLUDE_DIR AND OCTOMAP_LIBRARY)
        set(OCTOMAP_FOUND TRUE)
        set(OCTOMAP_INCLUDE_DIRS ${OCTOMAP_INCLUDE_DIR})
        set(OCTOMAP_LIBRARIES ${OCTOMAP_LIBRARY})
    endif()
endif()

if(NOT OCTOMAP_FOUND)
    message(FATAL_ERROR "Octomap library is required for spatial-temporal planning")
endif()

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    ${COGUTIL_INCLUDE_DIR}
    ${ATOMSPACE_INCLUDE_DIR}
    ${OCTOMAP_INCLUDE_DIRS}
)

if(SPACETIME_FOUND)
    include_directories(${SPACETIME_INCLUDE_DIR})
    add_definitions(-DHAVE_SPACETIME)
endif()

# Source files
set(SOURCES
    src/SpaceTimeIntegrator.cpp
)

set(HEADERS
    include/opencog/agentzero/SpaceTimeIntegrator.h
)

# Create the library
add_library(agentzero-planning SHARED ${SOURCES})

# Link libraries
target_link_libraries(agentzero-planning
    ${COGUTIL_LIBRARY}
    ${ATOMSPACE_LIBRARY}
    ${OCTOMAP_LIBRARIES}
)

if(SPACETIME_FOUND)
    target_link_libraries(agentzero-planning ${SPACETIME_LIBRARY})
endif()

# Install targets
install(TARGETS agentzero-planning
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    RUNTIME DESTINATION bin
)

install(DIRECTORY include/opencog
    DESTINATION include
    FILES_MATCHING PATTERN "*.h"
)

# Tests
if(BUILD_TESTING)
    add_subdirectory(tests)
endif()

# Examples
add_subdirectory(examples)

message(STATUS "Agent-Zero Planning Module configuration complete")