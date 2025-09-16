# Agent-Zero C++ Tests CMakeLists.txt

cmake_minimum_required(VERSION 3.16)

# Find CxxTest
find_package(CxxTest)

if(CXXTEST_FOUND)
    message(STATUS "CxxTest found - Agent-Zero tests will be built")
    include_directories(${CXXTEST_INCLUDE_DIR})
    enable_testing()
    
    # Add component test directories
    add_subdirectory(../agentzero-core/tests ${CMAKE_CURRENT_BINARY_DIR}/agentzero-core-tests)
    
    # Add integration tests when available
    # add_subdirectory(integration-tests)
    
    # Create a target to run all Agent-Zero tests
    add_custom_target(agentzero-tests
        DEPENDS AgentZeroCoreUTest
        COMMENT "Building all Agent-Zero tests"
    )
    
    message(STATUS "Agent-Zero test suite configured")
else()
    message(STATUS "CxxTest not found - Agent-Zero tests will not be built")
    message(STATUS "Install CxxTest to enable Agent-Zero unit testing")
    message(STATUS "  Ubuntu/Debian: sudo apt-get install cxxtest")
    message(STATUS "  See: https://cxxtest.com/")
endif()