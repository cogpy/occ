# Agent-Zero Planning Examples CMakeLists.txt

cmake_minimum_required(VERSION 3.16)

# Find required libraries
find_package(PkgConfig REQUIRED)
pkg_check_modules(COGUTIL REQUIRED cogutil)
pkg_check_modules(ATOMSPACE REQUIRED atomspace)

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../include
    ${COGUTIL_INCLUDE_DIRS}
    ${ATOMSPACE_INCLUDE_DIRS}
)

# Link directories
link_directories(
    ${COGUTIL_LIBRARY_DIRS}
    ${ATOMSPACE_LIBRARY_DIRS}
)

# Simple planning demo
add_executable(simple_planning_demo
    simple_planning_demo.cpp
)

target_link_libraries(simple_planning_demo
    agentzero-planning
    ${ATOMSPACE_LIBRARIES}
    ${COGUTIL_LIBRARIES}
    atom_types
    atombase
)

message(STATUS "Agent-Zero Planning examples configured")