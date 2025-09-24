# Agent-Zero Learning Module CMakeLists.txt
# Phase 5: Learning & Adaptation - ASMOSES Integration

cmake_minimum_required(VERSION 3.16)

project(AgentZeroLearning
    VERSION 0.1.0
    DESCRIPTION "Agent-Zero Learning & Adaptation Module with ASMOSES Integration"
    LANGUAGES CXX
)

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find required OpenCog packages
find_package(PkgConfig REQUIRED)

# Find cogutil
pkg_check_modules(COGUTIL REQUIRED cogutil)
if (COGUTIL_FOUND)
    message(STATUS "Found cogutil version ${COGUTIL_VERSION}")
    include_directories(${COGUTIL_INCLUDE_DIRS})
    link_directories(${COGUTIL_LIBRARY_DIRS})
endif()

# Find atomspace
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
    add_definitions(-DHAVE_COGSERVER)
endif()

# Find MOSES/ASMOSES (required for this module)
find_path(MOSES_INCLUDE_DIR
    NAMES opencog/moses/moses/types.h
    PATHS
        /usr/include
        /usr/local/include
        ${CMAKE_SOURCE_DIR}/../../../moses
        ${CMAKE_SOURCE_DIR}/../../../asmoses
)

find_library(MOSES_LIBRARY
    NAMES moses
    PATHS
        /usr/lib
        /usr/local/lib
        ${CMAKE_SOURCE_DIR}/../../../moses/build/opencog/moses
        ${CMAKE_SOURCE_DIR}/../../../asmoses/build/opencog/asmoses
)

if(MOSES_INCLUDE_DIR AND MOSES_LIBRARY)
    set(MOSES_FOUND TRUE)
    message(STATUS "Found MOSES: ${MOSES_LIBRARY}")
    include_directories(${MOSES_INCLUDE_DIR})
else()
    message(FATAL_ERROR "MOSES/ASMOSES not found - required for agentzero-learning module")
endif()

# Find Boost (required by MOSES)
find_package(Boost REQUIRED COMPONENTS
    system
    filesystem
    program_options
    regex
    thread
    date_time
)

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../agentzero-core/include
    ${Boost_INCLUDE_DIRS}
)

# Link directories
link_directories(
    ${Boost_LIBRARY_DIRS}
)

# Source files
set(LEARNING_SOURCES
    src/ASMOSESIntegrator.cpp
    src/AtomSpaceEvolver.cpp
    src/ExperienceManager.cpp
    src/SkillAcquisition.cpp
    src/PolicyOptimizer.cpp
    src/MetaLearning.cpp
)

# Header files
set(LEARNING_HEADERS
    include/agentzero-learning/ASMOSESIntegrator.h
    include/agentzero-learning/AtomSpaceEvolver.h
    include/agentzero-learning/ExperienceManager.h
    include/agentzero-learning/SkillAcquisition.h
    include/agentzero-learning/PolicyOptimizer.h
    include/agentzero-learning/MetaLearning.h
)

# Create the main library
add_library(agentzero-learning SHARED
    ${LEARNING_SOURCES}
)

# Link libraries
target_link_libraries(agentzero-learning
    ${COGUTIL_LIBRARIES}
    ${ATOMSPACE_LIBRARIES}
    ${MOSES_LIBRARY}
    ${Boost_LIBRARIES}
    pthread
)

if (COGSERVER_FOUND)
    target_link_libraries(agentzero-learning ${COGSERVER_LIBRARIES})
    target_link_libraries(agentzero-learning agentzero-core)
endif()

# Set target properties
set_target_properties(agentzero-learning PROPERTIES
    VERSION ${PROJECT_VERSION}
    SOVERSION ${PROJECT_VERSION_MAJOR}
    PUBLIC_HEADER "${LEARNING_HEADERS}"
)

# Install rules
install(TARGETS agentzero-learning
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    PUBLIC_HEADER DESTINATION include/agentzero-learning
)

# Install additional headers
install(DIRECTORY include/agentzero-learning/
    DESTINATION include/agentzero-learning
    FILES_MATCHING PATTERN "*.h"
)

# Examples
if(BUILD_EXAMPLES)
    add_subdirectory(examples)
endif()

# Tests
if(BUILD_TESTING)
    enable_testing()
    add_subdirectory(tests)
endif()

# Generate pkgconfig file
configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/agentzero-learning.pc.in
    ${CMAKE_CURRENT_BINARY_DIR}/agentzero-learning.pc
    @ONLY
)

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/agentzero-learning.pc
    DESTINATION lib/pkgconfig
)

message(STATUS "AgentZero Learning Module configuration complete")
message(STATUS "  - MOSES Integration: ${MOSES_FOUND}")
message(STATUS "  - CogServer Support: ${COGSERVER_FOUND}")
message(STATUS "  - Build Examples: ${BUILD_EXAMPLES}")
message(STATUS "  - Build Tests: ${BUILD_TESTING}")