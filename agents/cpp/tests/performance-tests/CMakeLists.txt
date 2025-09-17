# Performance Tests CMakeLists.txt
# AZ-TEST-001: Performance benchmarking and stress testing

cmake_minimum_required(VERSION 3.16)

if(CXXTEST_FOUND)
    # Performance Benchmark Tests
    if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/PerformanceBenchmarkTest.cxxtest")
        cxxtest_add_test(PerformanceBenchmarkTest PerformanceBenchmarkTest.cpp 
                        "${CMAKE_CURRENT_SOURCE_DIR}/PerformanceBenchmarkTest.cxxtest")
        
        target_link_libraries(PerformanceBenchmarkTest
            agentzero-test-framework
            agentzero-core
            ${COGUTIL_LIBRARIES}
            ${ATOMSPACE_LIBRARIES}
            ${COGSERVER_LIBRARIES}
            ${Boost_LIBRARIES}
        )
    endif()
    
    # Memory Usage Tests
    if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/MemoryUsageTest.cxxtest")
        cxxtest_add_test(MemoryUsageTest MemoryUsageTest.cpp 
                        "${CMAKE_CURRENT_SOURCE_DIR}/MemoryUsageTest.cxxtest")
        
        target_link_libraries(MemoryUsageTest
            agentzero-test-framework
            agentzero-core
            ${COGUTIL_LIBRARIES}
            ${ATOMSPACE_LIBRARIES}
            ${COGSERVER_LIBRARIES}
            ${Boost_LIBRARIES}
        )
    endif()
    
    message(STATUS "Agent-Zero performance tests configured")
endif()