cmake_minimum_required(VERSION 3.16)

message(STATUS "Configuring Agent-Zero Tools examples...")

# Include directories for examples
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../include
    ${COGUTIL_INCLUDE_DIR}
    ${ATOMSPACE_INCLUDE_DIR}
)

# CapabilityComposer demonstration
add_executable(CapabilityComposerDemo CapabilityComposerDemo.cpp)

target_link_libraries(CapabilityComposerDemo
    agentzero-tools
    ${COGUTIL_LIBRARY}
    ${ATOMSPACE_LIBRARY}
)

message(STATUS "Agent-Zero Tools examples configured successfully")
