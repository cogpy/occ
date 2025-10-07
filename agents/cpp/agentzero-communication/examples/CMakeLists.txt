# Agent-Zero Communication Examples CMakeLists.txt

cmake_minimum_required(VERSION 3.16)

# Example programs
set(COMMUNICATION_EXAMPLES
    basic_communication_example
    agent_messaging_demo
)

# Basic communication example
add_executable(basic_communication_example basic_communication_example.cpp)
target_link_libraries(basic_communication_example
    agentzero-communication
    ${COGUTIL_LIBRARIES}
    ${ATOMSPACE_LIBRARIES}
)
target_include_directories(basic_communication_example PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/../include
    ${COGUTIL_INCLUDE_DIRS}
    ${ATOMSPACE_INCLUDE_DIRS}
)

# Agent messaging demo
add_executable(agent_messaging_demo agent_messaging_demo.cpp)
target_link_libraries(agent_messaging_demo
    agentzero-communication
    ${COGUTIL_LIBRARIES}
    ${ATOMSPACE_LIBRARIES}
)
target_include_directories(agent_messaging_demo PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/../include
    ${COGUTIL_INCLUDE_DIRS}
    ${ATOMSPACE_INCLUDE_DIRS}
)

message(STATUS "Communication examples configured")