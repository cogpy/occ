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
# Agent-Zero Memory Module Tests

cmake_minimum_required(VERSION 3.16)

# Find CXXTest for unit testing
find_package(PkgConfig REQUIRED)
pkg_check_modules(CXXTEST cxxtest)

if(CXXTEST_FOUND)
    message(STATUS "CXXTest found - building Agent-Zero Memory tests")
    
    # Include CXXTest macros
    include_directories(${CXXTEST_INCLUDE_DIRS})
    
    # Test sources
    set(TEST_SOURCES
        WorkingMemoryTest.cxxtest
        ContextManagerUTest.cxxtest
    )
    
    # Generate test executable
    foreach(TEST_SOURCE ${TEST_SOURCES})
        get_filename_component(TEST_NAME ${TEST_SOURCE} NAME_WE)
        
        # Generate test runner
        add_custom_command(
            OUTPUT ${TEST_NAME}.cpp
            COMMAND ${CXXTEST_TESTGEN_EXECUTABLE} --error-printer -o ${TEST_NAME}.cpp ${CMAKE_CURRENT_SOURCE_DIR}/${TEST_SOURCE}
            DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${TEST_SOURCE}
        )
        
        # Create test executable
        add_executable(${TEST_NAME} ${TEST_NAME}.cpp)
        
        # Link libraries
        target_link_libraries(${TEST_NAME}
            agentzero-memory
            ${COGUTIL_LIBRARIES}
            ${ATOMSPACE_LIBRARIES}
            ${Boost_LIBRARIES}
        )
        
        # Add test to CTest
        add_test(NAME ${TEST_NAME} COMMAND ${TEST_NAME})
    endforeach()
    
else()
    message(STATUS "CXXTest not found - skipping Agent-Zero Memory tests")
endif()