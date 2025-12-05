# Agent-Zero Tools Examples CMakeLists.txt
# Examples for AZ-TOOL-002: ToolWrapper unified interface

cmake_minimum_required(VERSION 3.16)

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../include
    ${COGUTIL_INCLUDE_DIRS}
    ${ATOMSPACE_INCLUDE_DIRS}
)

# ToolWrapper demonstration example
add_executable(ToolWrapperDemo ToolWrapperDemo.cpp)

target_link_libraries(ToolWrapperDemo
    agentzero-tools
    ${COGUTIL_LIBRARIES}
    ${ATOMSPACE_LIBRARIES}
    ${Boost_LIBRARIES}
)

# Installation
install(TARGETS ToolWrapperDemo
    RUNTIME DESTINATION bin/examples
)

message(STATUS "Agent-Zero Tools examples configured")
