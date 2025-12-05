# Agent-Zero Tools Module CMakeLists.txt

cmake_minimum_required(VERSION 3.16)

project(AgentZeroTools
    VERSION 0.1.0
    DESCRIPTION "Agent-Zero Tool Integration Framework for OpenCog"
    LANGUAGES CXX
)

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find required OpenCog packages
find_package(PkgConfig REQUIRED)

# Try to find cogutil
pkg_check_modules(COGUTIL cogutil)
if (NOT COGUTIL_FOUND)
    # Try CMake find_package as fallback
    find_package(CogUtil REQUIRED)
    set(COGUTIL_INCLUDE_DIRS ${COGUTIL_INCLUDE_DIR})
    set(COGUTIL_LIBRARIES ${COGUTIL_LIBRARY})
endif()

if (COGUTIL_FOUND OR CogUtil_FOUND)
    message(STATUS "Found cogutil")
    include_directories(${COGUTIL_INCLUDE_DIRS})
    link_directories(${COGUTIL_LIBRARY_DIRS})
endif()

# Try to find atomspace  
pkg_check_modules(ATOMSPACE atomspace)
if (NOT ATOMSPACE_FOUND)
    # Try CMake find_package as fallback
    find_package(AtomSpace REQUIRED)
    set(ATOMSPACE_INCLUDE_DIRS ${ATOMSPACE_INCLUDE_DIR})
    set(ATOMSPACE_LIBRARIES ${ATOMSPACE_LIBRARY})
endif()

if (ATOMSPACE_FOUND OR AtomSpace_FOUND)
    message(STATUS "Found atomspace")
    include_directories(${ATOMSPACE_INCLUDE_DIRS})
    link_directories(${ATOMSPACE_LIBRARY_DIRS})
endif()

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    ${CMAKE_CURRENT_SOURCE_DIR}/src
)

# Source files
set(AGENTZERO_TOOLS_SOURCES
    src/ToolRegistry.cpp
)

# Header files
set(AGENTZERO_TOOLS_HEADERS
    include/opencog/agentzero/ToolRegistry.h
)

# Create library
if(AGENTZERO_TOOLS_SOURCES)
    add_library(agentzero-tools SHARED
        ${AGENTZERO_TOOLS_SOURCES}
        ${AGENTZERO_TOOLS_HEADERS}
    )

    target_link_libraries(agentzero-tools
        ${COGUTIL_LIBRARIES}
        ${ATOMSPACE_LIBRARIES}
        ${Boost_LIBRARIES}
    )

    target_include_directories(agentzero-tools PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
        $<INSTALL_INTERFACE:include>
    )

    # Installation
    install(TARGETS agentzero-tools
        EXPORT AgentZeroToolsTargets
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib
        RUNTIME DESTINATION bin
        INCLUDES DESTINATION include
    )

    install(DIRECTORY include/
        DESTINATION include
        FILES_MATCHING PATTERN "*.h"
    )
endif()

# Tests
if(BUILD_TESTING)
    enable_testing()
    add_subdirectory(tests)
endif()

# Status message
message(STATUS "Agent-Zero Tools module configuration complete")
message(STATUS "ToolRegistry implementation ready for build and testing")