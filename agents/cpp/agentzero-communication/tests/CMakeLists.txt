# Tests for Agent-Zero Communication Module
# Part of AZ-HUMAN-001: Create HumanInterface layer

find_package(Threads REQUIRED)

# Simple unit tests
add_executable(test_human_interface
    test_HumanInterface.cpp
)

target_link_libraries(test_human_interface
    agentzero-communication
    ${COGUTIL_LIBRARIES}
    ${ATOMSPACE_LIBRARIES}
    ${Boost_LIBRARIES}
    Threads::Threads
)

target_include_directories(test_human_interface PRIVATE
    ${CMAKE_SOURCE_DIR}/include
    ${COGUTIL_INCLUDE_DIRS}
    ${ATOMSPACE_INCLUDE_DIRS}
)

# Add test
add_test(NAME HumanInterfaceBasicTest COMMAND test_human_interface)

# Test utilities
add_executable(test_communication_utils
    test_CommunicationUtils.cpp
)

target_link_libraries(test_communication_utils
    agentzero-communication
    ${COGUTIL_LIBRARIES}
    ${ATOMSPACE_LIBRARIES}
    ${Boost_LIBRARIES}
    Threads::Threads
)

target_include_directories(test_communication_utils PRIVATE
    ${CMAKE_SOURCE_DIR}/include
    ${COGUTIL_INCLUDE_DIRS}
    ${ATOMSPACE_INCLUDE_DIRS}
)

add_test(NAME CommunicationUtilsTest COMMAND test_communication_utils)