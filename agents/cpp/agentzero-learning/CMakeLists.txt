# Agent-Zero Learning Module CMakeLists.txt
# Implements ExperienceManager and learning functionality

cmake_minimum_required(VERSION 3.16)

project(AgentZeroLearning
    VERSION 0.1.0
    DESCRIPTION "Agent-Zero Learning & Adaptation Module for OpenCog Integration"
    LANGUAGES CXX
)

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find required OpenCog packages
find_package(PkgConfig REQUIRED)

# Find cogutil (required)
pkg_check_modules(COGUTIL REQUIRED cogutil)
if (COGUTIL_FOUND)
    message(STATUS "Found cogutil version ${COGUTIL_VERSION}")
    include_directories(${COGUTIL_INCLUDE_DIRS})
    link_directories(${COGUTIL_LIBRARY_DIRS})
endif()

# Find atomspace (required)
pkg_check_modules(ATOMSPACE REQUIRED atomspace)
if (ATOMSPACE_FOUND)
    message(STATUS "Found atomspace version ${ATOMSPACE_VERSION}")
    include_directories(${ATOMSPACE_INCLUDE_DIRS})
    link_directories(${ATOMSPACE_LIBRARY_DIRS})
endif()

# Find cogserver (optional)
pkg_check_modules(COGSERVER cogserver)
if (COGSERVER_FOUND)
    message(STATUS "Found cogserver version ${COGSERVER_VERSION}")
    include_directories(${COGSERVER_INCLUDE_DIRS})
    link_directories(${COGSERVER_LIBRARY_DIRS})
else()
    message(STATUS "CogServer not found - some features will be disabled")
    set(COGSERVER_FOUND FALSE)
endif()

# Find MOSES (required for learning module)
pkg_check_modules(MOSES moses)
if (MOSES_FOUND)
    message(STATUS "Found MOSES version ${MOSES_VERSION}")
    include_directories(${MOSES_INCLUDE_DIRS})
    link_directories(${MOSES_LIBRARY_DIRS})
else()
    message(STATUS "MOSES not found via pkg-config, looking for system includes")
    find_path(MOSES_INCLUDE_PATH
        NAMES opencog/moses/main/moses.h
        PATHS /usr/local/include /usr/include
        PATH_SUFFIXES opencog
    )
    if(MOSES_INCLUDE_PATH)
        message(STATUS "Found MOSES headers at ${MOSES_INCLUDE_PATH}")
        include_directories(${MOSES_INCLUDE_PATH})
        set(MOSES_FOUND TRUE)
    else()
        message(WARNING "MOSES not found - MOSES integration will be disabled")
        set(MOSES_FOUND FALSE)
    endif()
endif()

# Find ASMOSES (for AtomSpace evolution)
pkg_check_modules(ASMOSES asmoses)
if (ASMOSES_FOUND)
    message(STATUS "Found ASMOSES version ${ASMOSES_VERSION}")
    include_directories(${ASMOSES_INCLUDE_DIRS})
    link_directories(${ASMOSES_LIBRARY_DIRS})
else()
    message(STATUS "ASMOSES not found via pkg-config, looking for system includes")
    find_path(ASMOSES_INCLUDE_PATH
        NAMES opencog/asmoses/combo/combo.h
        PATHS /usr/local/include /usr/include
        PATH_SUFFIXES opencog
    )
    if(ASMOSES_INCLUDE_PATH)
        message(STATUS "Found ASMOSES headers at ${ASMOSES_INCLUDE_PATH}")
        include_directories(${ASMOSES_INCLUDE_PATH})
        set(ASMOSES_FOUND TRUE)
    else()
        message(STATUS "ASMOSES not found - AtomSpace evolution features may be limited")
        set(ASMOSES_FOUND FALSE)
    endif()
endif()

# Find learn module
pkg_check_modules(LEARN learn)
if (LEARN_FOUND)
    message(STATUS "Found learn version ${LEARN_VERSION}")
    include_directories(${LEARN_INCLUDE_DIRS})
    link_directories(${LEARN_LIBRARY_DIRS})
else()
    message(STATUS "Learn module not found via pkg-config, looking for system includes")
    find_path(LEARN_INCLUDE_PATH
        NAMES opencog/learning/learning.h
        PATHS /usr/local/include /usr/include
        PATH_SUFFIXES opencog
    )
    if(LEARN_INCLUDE_PATH)
        message(STATUS "Found learn headers at ${LEARN_INCLUDE_PATH}")
        include_directories(${LEARN_INCLUDE_PATH})
        set(LEARN_FOUND TRUE)
    else()
        message(STATUS "Learn module not found - some learning features may be limited")
        set(LEARN_FOUND FALSE)
    endif()
endif()

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    ${CMAKE_CURRENT_SOURCE_DIR}/src
    ${CMAKE_CURRENT_SOURCE_DIR}/../agentzero-core/include  # For AgentZeroCore
)

# Source files
set(AGENTZERO_LEARNING_SOURCES
    src/ExperienceManager.cpp
)

# Header files
set(AGENTZERO_LEARNING_HEADERS
    include/opencog/agentzero/learning/ExperienceManager.h
)

# Create library
add_library(agentzero-learning SHARED
    ${AGENTZERO_LEARNING_SOURCES}
    ${AGENTZERO_LEARNING_HEADERS}
)

# Link required libraries
target_link_libraries(agentzero-learning
    ${COGUTIL_LIBRARIES}
    ${ATOMSPACE_LIBRARIES}
    ${Boost_LIBRARIES}
)

# Link optional libraries
if(COGSERVER_FOUND AND COGSERVER_LIBRARIES)
    target_link_libraries(agentzero-learning ${COGSERVER_LIBRARIES})
    target_compile_definitions(agentzero-learning PRIVATE HAVE_COGSERVER=1)
    message(STATUS "Linking cogserver libraries: ${COGSERVER_LIBRARIES}")
endif()

if(MOSES_FOUND AND MOSES_LIBRARIES)
    target_link_libraries(agentzero-learning ${MOSES_LIBRARIES})
    target_compile_definitions(agentzero-learning PRIVATE HAVE_MOSES=1)
    message(STATUS "Linking MOSES libraries: ${MOSES_LIBRARIES}")
elseif(MOSES_FOUND)
    target_compile_definitions(agentzero-learning PRIVATE HAVE_MOSES=1)
    message(STATUS "MOSES headers found, compiling with MOSES support")
endif()

if(ASMOSES_FOUND AND ASMOSES_LIBRARIES)
    target_link_libraries(agentzero-learning ${ASMOSES_LIBRARIES})
    target_compile_definitions(agentzero-learning PRIVATE HAVE_ASMOSES=1)
    message(STATUS "Linking ASMOSES libraries: ${ASMOSES_LIBRARIES}")
elseif(ASMOSES_FOUND)
    target_compile_definitions(agentzero-learning PRIVATE HAVE_ASMOSES=1)
    message(STATUS "ASMOSES headers found, compiling with ASMOSES support")
endif()

if(LEARN_FOUND AND LEARN_LIBRARIES)
    target_link_libraries(agentzero-learning ${LEARN_LIBRARIES})
    target_compile_definitions(agentzero-learning PRIVATE HAVE_LEARN=1)
    message(STATUS "Linking learn libraries: ${LEARN_LIBRARIES}")
elseif(LEARN_FOUND)
    target_compile_definitions(agentzero-learning PRIVATE HAVE_LEARN=1)
    message(STATUS "Learn headers found, compiling with learn support")
endif()

target_include_directories(agentzero-learning PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>
)

# Set library properties
set_target_properties(agentzero-learning PROPERTIES
    VERSION ${PROJECT_VERSION}
    SOVERSION 1
    OUTPUT_NAME "agentzero-learning"
)

# Installation
install(TARGETS agentzero-learning
    EXPORT AgentZeroCppTargets
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    RUNTIME DESTINATION bin
    INCLUDES DESTINATION include
)

install(DIRECTORY include/
    DESTINATION include
    FILES_MATCHING PATTERN "*.h"
)

# Tests
if(BUILD_TESTING)
    enable_testing()
    add_subdirectory(tests)
endif()

# Examples
if(BUILD_EXAMPLES)
    add_subdirectory(examples)
endif()

# Status message
message(STATUS "Agent-Zero Learning module configuration complete")
message(STATUS "Dependencies found:")
message(STATUS "  MOSES: ${MOSES_FOUND}")
message(STATUS "  ASMOSES: ${ASMOSES_FOUND}")
message(STATUS "  Learn: ${LEARN_FOUND}")
message(STATUS "ExperienceManager ready for build and testing")