# Tests CMakeLists.txt for Agent-Zero Core
cmake_minimum_required(VERSION 3.16)

# Find CxxTest if available
find_package(CxxTest)

if(CXXTEST_FOUND)
    include_directories(${CXXTEST_INCLUDE_DIR})
    enable_testing()
    
    # Include the source directory for testing
    include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../include)
    include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../src)
    
    # AgentZeroCore unit tests
    if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/AgentZeroCoreUTest.cxxtest")
        cxxtest_add_test(AgentZeroCoreUTest AgentZeroCoreUTest.cpp 
                        "${CMAKE_CURRENT_SOURCE_DIR}/AgentZeroCoreUTest.cxxtest")
        
        target_link_libraries(AgentZeroCoreUTest
            agentzero-core
            ${COGUTIL_LIBRARIES}
            ${ATOMSPACE_LIBRARIES}
            ${COGSERVER_LIBRARIES}
            ${Boost_LIBRARIES}
        )
    endif()
    
    # ActionScheduler unit tests
    if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/ActionSchedulerTest.cpp")
        add_executable(ActionSchedulerTest ActionSchedulerTest.cpp)
        
        target_link_libraries(ActionSchedulerTest
            agentzero-core
            ${COGUTIL_LIBRARIES}
            ${ATOMSPACE_LIBRARIES}
            ${COGSERVER_LIBRARIES}
            ${Boost_LIBRARIES}
        )
        
        add_test(NAME ActionSchedulerTest COMMAND ActionSchedulerTest)
    endif()
    
    message(STATUS "Agent-Zero Core tests configured with CxxTest")
else()
    message(STATUS "CxxTest not found - tests will not be built")
    message(STATUS "Install CxxTest to enable unit testing")
endif()