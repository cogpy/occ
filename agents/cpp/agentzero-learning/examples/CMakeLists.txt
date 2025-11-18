cmake_minimum_required(VERSION 3.16)

message(STATUS "Configuring Agent-Zero Learning examples...")

# Include directories for examples
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../include
    ${COGUTIL_INCLUDE_DIR}
    ${ATOMSPACE_INCLUDE_DIR}
)

# EpisodicMemory demonstration
add_executable(episodic_memory_demo episodic_memory_demo.cpp)

target_link_libraries(episodic_memory_demo
    agentzero-learning
    ${COGUTIL_LIBRARY}
    ${ATOMSPACE_LIBRARY}
)

message(STATUS "Agent-Zero Learning examples configured successfully")
