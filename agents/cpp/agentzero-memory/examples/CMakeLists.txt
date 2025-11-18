cmake_minimum_required(VERSION 3.16)

# Basic LongTermMemory example
add_executable(ltm_basic_example basic_ltm_example.cpp)

target_link_libraries(ltm_basic_example
    agentzero-memory
    ${COGUTIL_LIBRARIES}
    ${ATOMSPACE_LIBRARIES}
    ${ATOMSPACE_ROCKS_LIBRARIES}
    ${ATTENTION_LIBRARIES}
)

target_include_directories(ltm_basic_example PRIVATE
    ${CMAKE_SOURCE_DIR}/include
    ${COGUTIL_INCLUDE_DIRS}
    ${ATOMSPACE_INCLUDE_DIRS}
    ${ATOMSPACE_ROCKS_INCLUDE_DIRS}
    ${ATTENTION_INCLUDE_DIRS}
)

# Persistence demonstration example
add_executable(ltm_persistence_example persistence_example.cpp)

target_link_libraries(ltm_persistence_example
    agentzero-memory
    ${COGUTIL_LIBRARIES}
    ${ATOMSPACE_LIBRARIES}
    ${ATOMSPACE_ROCKS_LIBRARIES}
    ${ATTENTION_LIBRARIES}
)

target_include_directories(ltm_persistence_example PRIVATE
    ${CMAKE_SOURCE_DIR}/include
    ${COGUTIL_INCLUDE_DIRS}
    ${ATOMSPACE_INCLUDE_DIRS}
    ${ATOMSPACE_ROCKS_INCLUDE_DIRS}
    ${ATTENTION_INCLUDE_DIRS}
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

# ContextManager example
add_executable(context-manager-example ContextManagerExample.cpp)

target_link_libraries(context-manager-example
    agentzero-memory
    ${COGUTIL_LIBRARIES}
    ${ATOMSPACE_LIBRARIES}
    ${Boost_LIBRARIES}
)

# Installation
install(TARGETS working-memory-example context-manager-example
    RUNTIME DESTINATION bin
)