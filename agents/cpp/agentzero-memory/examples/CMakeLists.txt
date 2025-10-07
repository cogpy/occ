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
)