cmake_minimum_required(VERSION 3.16)

# Project metadata
project(agentzero-tools VERSION 0.1.0 DESCRIPTION "Agent-Zero Tool Integration Framework")

message(STATUS "Configuring Agent-Zero Tool Integration Framework...")

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
set(TOOLS_SOURCES
    src/CapabilityComposer.cpp
)

# Create the agentzero-tools library
add_library(agentzero-tools SHARED ${TOOLS_SOURCES})

# Link dependencies
target_link_libraries(agentzero-tools
    ${COGUTIL_LIBRARY}
    ${ATOMSPACE_LIBRARY}
)

# Set library properties
set_target_properties(agentzero-tools PROPERTIES
    VERSION "1.0.0"
    SOVERSION "1"
)

# Install targets
install(TARGETS agentzero-tools
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

message(STATUS "Agent-Zero Tools module configured successfully")