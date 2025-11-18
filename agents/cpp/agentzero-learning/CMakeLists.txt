cmake_minimum_required(VERSION 3.16)

# Project metadata
project(agentzero-learning VERSION 0.1.0 DESCRIPTION "Agent-Zero Learning & Adaptation Module")

message(STATUS "Configuring Agent-Zero Learning & Adaptation module...")

# Find required dependencies
find_package(CogUtil REQUIRED)
find_package(AtomSpace REQUIRED)

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    ${COGUTIL_INCLUDE_DIR}
    ${ATOMSPACE_INCLUDE_DIR}
)

# Library sources
set(LEARNING_SOURCES
    src/EpisodicMemory.cpp
)

# Create the agentzero-learning library
add_library(agentzero-learning SHARED ${LEARNING_SOURCES})

# Link dependencies
target_link_libraries(agentzero-learning
    ${COGUTIL_LIBRARY}
    ${ATOMSPACE_LIBRARY}
)

# Set library properties
set_target_properties(agentzero-learning PROPERTIES
    VERSION "1.0.0"
    SOVERSION "1"
)

# Install targets
install(TARGETS agentzero-learning
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
)

# Install headers
install(DIRECTORY include/opencog
    DESTINATION include
    FILES_MATCHING PATTERN "*.h"
)

# Add tests if requested
option(BUILD_TESTS "Build tests" ON)
if(BUILD_TESTS)
    add_subdirectory(tests)
endif()

# Add examples
option(BUILD_EXAMPLES "Build examples" ON)
if(BUILD_EXAMPLES)
    add_subdirectory(examples)
endif()

message(STATUS "Agent-Zero Learning module configured successfully")