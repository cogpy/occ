cmake_minimum_required(VERSION 3.16)

message(STATUS "Configuring Agent-Zero Learning tests...")

# Include directories for tests
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../include
    ${COGUTIL_INCLUDE_DIR}
    ${ATOMSPACE_INCLUDE_DIR}
)

# Simple test executable for EpisodicMemory
add_executable(EpisodicMemorySimpleTest EpisodicMemorySimpleTest.cpp)

target_link_libraries(EpisodicMemorySimpleTest
    agentzero-learning
    ${COGUTIL_LIBRARY}
    ${ATOMSPACE_LIBRARY}
)

# Add test to CTest
add_test(NAME EpisodicMemorySimpleTest COMMAND EpisodicMemorySimpleTest)

message(STATUS "Agent-Zero Learning tests configured successfully")