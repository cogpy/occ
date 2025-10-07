cmake_minimum_required(VERSION 3.16)

# Project metadata
project(agentzero-memory 
    VERSION 0.1.0 
    DESCRIPTION "Agent-Zero Memory & Context Management Module"
    LANGUAGES CXX
)

message(STATUS "Configuring Agent-Zero Memory & Context module...")

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find required OpenCog packages
find_package(PkgConfig REQUIRED)

# Core OpenCog dependencies (required)
pkg_check_modules(COGUTIL REQUIRED cogutil)
pkg_check_modules(ATOMSPACE REQUIRED atomspace)

# Memory-specific dependencies (required for persistence)
pkg_check_modules(ATOMSPACE_ROCKS REQUIRED atomspace-rocks)
pkg_check_modules(ATTENTION REQUIRED attention)

# Report found dependencies
message(STATUS "Agent-Zero Memory Dependencies:")
message(STATUS "  cogutil: ${COGUTIL_VERSION}")
message(STATUS "  atomspace: ${ATOMSPACE_VERSION}")
message(STATUS "  atomspace-rocks: ${ATOMSPACE_ROCKS_VERSION}")
message(STATUS "  attention: ${ATTENTION_VERSION}")

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    ${CMAKE_CURRENT_SOURCE_DIR}/src
    ${COGUTIL_INCLUDE_DIRS}
    ${ATOMSPACE_INCLUDE_DIRS}
    ${ATOMSPACE_ROCKS_INCLUDE_DIRS}
    ${ATTENTION_INCLUDE_DIRS}
)

# Link directories
link_directories(
    ${COGUTIL_LIBRARY_DIRS}
    ${ATOMSPACE_LIBRARY_DIRS}
    ${ATOMSPACE_ROCKS_LIBRARY_DIRS}
    ${ATTENTION_LIBRARY_DIRS}
)

# Source files
set(AGENTZERO_MEMORY_SOURCES
    src/LongTermMemory.cpp
    src/EpisodicMemory.cpp
    src/WorkingMemory.cpp
    src/ContextManager.cpp
)

# Header files (for installation)
set(AGENTZERO_MEMORY_HEADERS
    include/opencog/agentzero/memory/LongTermMemory.h
    include/opencog/agentzero/memory/EpisodicMemory.h
    include/opencog/agentzero/memory/WorkingMemory.h
    include/opencog/agentzero/memory/ContextManager.h
    include/opencog/agentzero/memory/MemoryTypes.h
)

# Create the library
add_library(agentzero-memory SHARED ${AGENTZERO_MEMORY_SOURCES})

# Set library properties
set_target_properties(agentzero-memory PROPERTIES
    VERSION ${PROJECT_VERSION}
    SOVERSION 0
    PUBLIC_HEADER "${AGENTZERO_MEMORY_HEADERS}"
    OUTPUT_NAME "agentzero-memory"
)

# Link required libraries
target_link_libraries(agentzero-memory
    ${COGUTIL_LIBRARIES}
    ${ATOMSPACE_LIBRARIES}
    ${ATOMSPACE_ROCKS_LIBRARIES}
    ${ATTENTION_LIBRARIES}
)

# Set include directories for target
target_include_directories(agentzero-memory PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>
)

# Compiler definitions
target_compile_definitions(agentzero-memory PRIVATE
    AGENTZERO_MEMORY_VERSION="${PROJECT_VERSION}"
    HAVE_PERSISTENCE=1
    HAVE_ATTENTION=1
)

# Installation
include(GNUInstallDirs)

install(TARGETS agentzero-memory
    EXPORT AgentZeroMemoryTargets
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/opencog/agentzero/memory
)

# Install headers
install(DIRECTORY include/
    DESTINATION include
    FILES_MATCHING PATTERN "*.h"
)

# Install export targets
install(EXPORT AgentZeroMemoryTargets
    FILE AgentZeroMemoryTargets.cmake
    NAMESPACE AgentZero::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/AgentZeroMemory
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

# Generate pkgconfig file
configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/agentzero-memory.pc.in
    ${CMAKE_CURRENT_BINARY_DIR}/agentzero-memory.pc
    @ONLY
)

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/agentzero-memory.pc
    DESTINATION lib/pkgconfig
)

# Status summary
message(STATUS "Agent-Zero Memory module configuration complete")
message(STATUS "  - Persistence support: YES (atomspace-rocks)")
message(STATUS "  - Attention integration: YES")
message(STATUS "  - Build examples: ${BUILD_EXAMPLES}")
message(STATUS "  - Build tests: ${BUILD_TESTING}")