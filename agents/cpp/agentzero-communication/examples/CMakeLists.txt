# Agent-Zero Communication Module Examples

cmake_minimum_required(VERSION 3.16)

# Example source files
set(DIALOGUE_EXAMPLE_SOURCES
    dialogue_manager_example.cpp
)

# Build dialogue manager example
add_executable(dialogue_manager_example ${DIALOGUE_EXAMPLE_SOURCES})
target_link_libraries(dialogue_manager_example
    agentzero-communication
    ${COGUTIL_LIBRARIES}
    ${ATOMSPACE_LIBRARIES}
    ${Boost_LIBRARIES}
)

# Installation (optional)
install(TARGETS dialogue_manager_example
    RUNTIME DESTINATION bin/examples
)

message(STATUS "Communication module examples configured")