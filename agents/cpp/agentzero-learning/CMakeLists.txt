cmake_minimum_required(VERSION 3.16)

# Project metadata
project(agentzero-learning VERSION 0.1.0 DESCRIPTION "Agent-Zero Learning & Adaptation Module")

message(STATUS "Configuring Agent-Zero Learning & Adaptation module...")

# Find required dependencies
find_package(PkgConfig REQUIRED)

# Core OpenCog dependencies
find_path(COGUTIL_INCLUDE_DIR opencog/util/Logger.h PATHS /usr/local/include /usr/include)
find_library(COGUTIL_LIBRARY cogutil PATHS /usr/local/lib /usr/lib)

find_path(ATOMSPACE_INCLUDE_DIR opencog/atomspace/AtomSpace.h PATHS /usr/local/include /usr/include)
find_library(ATOMSPACE_LIBRARY atomspace PATHS /usr/local/lib /usr/lib)

# MOSES dependency for policy optimization
find_path(MOSES_INCLUDE_DIR moses/moses/main/moses_main.h PATHS /usr/local/include /usr/include)
find_library(MOSES_LIBRARY moses PATHS /usr/local/lib /usr/lib)

# Check if required dependencies are found
set(REQUIRED_DEPS_FOUND TRUE)

if(NOT COGUTIL_INCLUDE_DIR OR NOT COGUTIL_LIBRARY)
    message(STATUS "CogUtil not found - required for Agent-Zero Learning")
    set(REQUIRED_DEPS_FOUND FALSE)
endif()

if(NOT ATOMSPACE_INCLUDE_DIR OR NOT ATOMSPACE_LIBRARY)
    message(STATUS "AtomSpace not found - required for Agent-Zero Learning")
    set(REQUIRED_DEPS_FOUND FALSE)
endif()

if(NOT MOSES_INCLUDE_DIR OR NOT MOSES_LIBRARY)
    message(STATUS "MOSES not found - required for policy optimization")
    set(REQUIRED_DEPS_FOUND FALSE)
endif()

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    ${CMAKE_CURRENT_SOURCE_DIR}/src
)

# Optional dependencies
find_path(ASMOSES_INCLUDE_DIR asmoses/asmoses.h PATHS /usr/local/include /usr/include)
find_library(ASMOSES_LIBRARY asmoses PATHS /usr/local/lib /usr/lib)

find_path(LEARN_INCLUDE_DIR learn/feature-selection/main/feature-selection.h PATHS /usr/local/include /usr/include)
find_library(LEARN_LIBRARY learn PATHS /usr/local/lib /usr/lib)

# Source files
set(AGENTZERO_LEARNING_SOURCES
    src/SkillAcquisition.cpp
    src/ExperienceManager.cpp
    src/PolicyOptimizer.cpp
    src/MetaLearning.cpp
)

# Header files
set(AGENTZERO_LEARNING_HEADERS
    include/opencog/agentzero/SkillAcquisition.h
    include/opencog/agentzero/ExperienceManager.h
    include/opencog/agentzero/PolicyOptimizer.h
    include/opencog/agentzero/MetaLearning.h
)

# Create library when dependencies are available
if(REQUIRED_DEPS_FOUND)
    # Create the library
    add_library(agentzero-learning SHARED ${AGENTZERO_LEARNING_SOURCES})

    # Set library properties
    set_target_properties(agentzero-learning PROPERTIES
        VERSION ${PROJECT_VERSION}
        SOVERSION 1
        POSITION_INDEPENDENT_CODE ON
    )

    # Include directories
    target_include_directories(agentzero-learning PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
        $<BUILD_INTERFACE:${COGUTIL_INCLUDE_DIR}>
        $<BUILD_INTERFACE:${ATOMSPACE_INCLUDE_DIR}>
        $<BUILD_INTERFACE:${MOSES_INCLUDE_DIR}>
        $<INSTALL_INTERFACE:include>
    )

    # Link libraries
    target_link_libraries(agentzero-learning
        ${COGUTIL_LIBRARY}
        ${ATOMSPACE_LIBRARY}
        ${MOSES_LIBRARY}
    )

    # Optional library linking
    if(ASMOSES_LIBRARY)
        target_link_libraries(agentzero-learning ${ASMOSES_LIBRARY})
        target_compile_definitions(agentzero-learning PRIVATE HAVE_ASMOSES=1)
        message(STATUS "ASMOSES support enabled for AtomSpace evolution")
    endif()

    if(LEARN_LIBRARY)
        target_link_libraries(agentzero-learning ${LEARN_LIBRARY})
        target_compile_definitions(agentzero-learning PRIVATE HAVE_LEARN=1)
        message(STATUS "Learn library support enabled")
    endif()

    # Compiler definitions
    target_compile_definitions(agentzero-learning PRIVATE
        PROJECT_SOURCE_DIR="${PROJECT_SOURCE_DIR}"
        PROJECT_BINARY_DIR="${PROJECT_BINARY_DIR}"
    )

    # Installation
    install(TARGETS agentzero-learning
        EXPORT AgentZeroLearningTargets
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib
        RUNTIME DESTINATION bin
        INCLUDES DESTINATION include
    )

    install(DIRECTORY include/
        DESTINATION include
        FILES_MATCHING PATTERN "*.h"
    )

    message(STATUS "Agent-Zero Learning module configured successfully")
else()
    message(STATUS "Skipping Agent-Zero Learning module due to missing dependencies")
endif()

# Tests
if(BUILD_TESTING AND REQUIRED_DEPS_FOUND)
    enable_testing()
    add_subdirectory(tests)
endif()

# Status message
message(STATUS "Agent-Zero Learning module configuration complete")