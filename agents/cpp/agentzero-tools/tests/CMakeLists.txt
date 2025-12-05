cmake_minimum_required(VERSION 3.16)

message(STATUS "Configuring Agent-Zero Tools tests...")

# Include directories for tests
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../include
    ${COGUTIL_INCLUDE_DIR}
    ${ATOMSPACE_INCLUDE_DIR}
)

# Simple test executable for CapabilityComposer
add_executable(CapabilityComposerSimpleTest CapabilityComposerSimpleTest.cpp)

target_link_libraries(CapabilityComposerSimpleTest
    agentzero-tools
    ${COGUTIL_LIBRARY}
    ${ATOMSPACE_LIBRARY}
)

# Add test to CTest
add_test(NAME CapabilityComposerSimpleTest COMMAND CapabilityComposerSimpleTest)

message(STATUS "Agent-Zero Tools tests configured successfully")
