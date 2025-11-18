# Agent-Zero Memory Examples

cmake_minimum_required(VERSION 3.16)

# Simple usage example
set(MEMORY_EXAMPLE_SOURCES
    WorkingMemoryExample.cpp
)

# Create example executable
add_executable(working-memory-example ${MEMORY_EXAMPLE_SOURCES})

# Link libraries
target_link_libraries(working-memory-example
    agentzero-memory
    ${COGUTIL_LIBRARIES}
    ${ATOMSPACE_LIBRARIES}
    ${Boost_LIBRARIES}
)

# Installation
install(TARGETS working-memory-example
    RUNTIME DESTINATION bin
)