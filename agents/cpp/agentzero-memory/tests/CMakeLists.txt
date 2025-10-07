cmake_minimum_required(VERSION 3.16)

# Find testing framework
find_package(CxxTest QUIET)

if(CXXTEST_FOUND)
    message(STATUS "CxxTest found - building memory tests")
    
    include_directories(${CXXTEST_INCLUDE_DIRS})
    include_directories(${CMAKE_SOURCE_DIR}/include)
    
    # Test for LongTermMemory
    cxxtest_add_test(LongTermMemoryUTest LongTermMemoryUTest.cpp
        ${CMAKE_CURRENT_SOURCE_DIR}/LongTermMemoryUTest.cxxtest)
    
    target_link_libraries(LongTermMemoryUTest
        agentzero-memory
        ${COGUTIL_LIBRARIES}
        ${ATOMSPACE_LIBRARIES}
        ${ATOMSPACE_ROCKS_LIBRARIES}
        ${ATTENTION_LIBRARIES}
    )
    
    target_include_directories(LongTermMemoryUTest PRIVATE
        ${CMAKE_SOURCE_DIR}/include
        ${COGUTIL_INCLUDE_DIRS}
        ${ATOMSPACE_INCLUDE_DIRS}
        ${ATOMSPACE_ROCKS_INCLUDE_DIRS}
        ${ATTENTION_INCLUDE_DIRS}
    )
    
else()
    message(STATUS "CxxTest not found - skipping memory tests")
endif()