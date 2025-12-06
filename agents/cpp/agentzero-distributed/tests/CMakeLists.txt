# Tests CMakeLists.txt for Agent-Zero Distributed Computing

# Find GTest
find_package(GTest REQUIRED)
if(NOT GTEST_FOUND)
    message(WARNING "GTest not found - tests will not be built")
    return()
endif()

include_directories(
    ${GTEST_INCLUDE_DIRS}
    ${CMAKE_CURRENT_SOURCE_DIR}/../include
)

# Test executables
set(TEST_SOURCES
    DistributedCoordinatorTest.cpp
    ClusterManagerTest.cpp
    LoadBalancerTest.cpp
)

foreach(TEST_SOURCE ${TEST_SOURCES})
    get_filename_component(TEST_NAME ${TEST_SOURCE} NAME_WE)
    
    add_executable(${TEST_NAME} ${TEST_SOURCE})
    
    target_link_libraries(${TEST_NAME}
        agentzero-distributed
        ${GTEST_LIBRARIES}
        ${GTEST_MAIN_LIBRARIES}
        ${COGUTIL_LIBRARIES}
        ${ATOMSPACE_LIBRARIES}
        pthread
    )
    
    add_test(NAME ${TEST_NAME} COMMAND ${TEST_NAME})
endforeach()

message(STATUS "Distributed computing tests configured")
