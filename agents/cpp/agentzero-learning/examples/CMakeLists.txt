# Agent-Zero Learning Examples

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../include
    ${COGUTIL_INCLUDE_DIRS}
    ${ATOMSPACE_INCLUDE_DIRS}
)

# Example programs
add_executable(asmoses_evolution_example
    asmoses_evolution_example.cpp
)

target_link_libraries(asmoses_evolution_example
    agentzero-learning
    ${COGUTIL_LIBRARIES}
    ${ATOMSPACE_LIBRARIES}
    pthread
)

# Install examples
install(TARGETS asmoses_evolution_example
    RUNTIME DESTINATION bin/examples
)