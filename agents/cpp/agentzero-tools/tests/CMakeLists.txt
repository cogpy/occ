# Agent-Zero Tools Tests CMakeLists.txt
# Unit tests for AZ-TOOL-002: ToolWrapper unified interface

cmake_minimum_required(VERSION 3.16)

# Find CxxTest
find_package(CxxTest)

if (CXXTEST_FOUND)
    message(STATUS "CxxTest found - building Agent-Zero Tools tests")
    
    # Enable testing
    enable_testing()
    
    # Include CxxTest
    include_directories(${CXXTEST_INCLUDE_DIRS})
    
    # Include parent directories
    include_directories(
        ${CMAKE_CURRENT_SOURCE_DIR}/../include
        ${COGUTIL_INCLUDE_DIRS}
        ${ATOMSPACE_INCLUDE_DIRS}
    )
    
    # Generate and add test for ToolWrapper
    CXXTEST_ADD_TEST(ToolWrapperUTest ToolWrapperUTest.cpp
        ${CMAKE_CURRENT_SOURCE_DIR}/ToolWrapperUTest.cxxtest
    )
    
    target_link_libraries(ToolWrapperUTest
        agentzero-tools
        ${COGUTIL_LIBRARIES}
        ${ATOMSPACE_LIBRARIES}
    )
    
    # Add test to CTest
    add_test(NAME ToolWrapperUTest COMMAND ToolWrapperUTest)
    
    message(STATUS "Agent-Zero Tools tests configured successfully")
else()
    message(STATUS "CxxTest not found - skipping Agent-Zero Tools tests")
endif()
