cmake_minimum_required(VERSION 3.16)

message(STATUS "Configuring Agent-Zero Tools tests...")

# Include directories for tests
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../include
    ${COGUTIL_INCLUDE_DIR}
    ${ATOMSPACE_INCLUDE_DIR}
)

# Simple test executable for CapabilityComposer
add_executable(CapabilityComposerSimpleTest CapabilityComposerSimpleTest.cpp)

target_link_libraries(CapabilityComposerSimpleTest
    agentzero-tools
    ${COGUTIL_LIBRARY}
    ${ATOMSPACE_LIBRARY}
)

# Add test to CTest
add_test(NAME CapabilityComposerSimpleTest COMMAND CapabilityComposerSimpleTest)

# Simple test executable for ResourceManager
add_executable(ResourceManagerSimpleTest ResourceManagerSimpleTest.cpp)

target_link_libraries(ResourceManagerSimpleTest
    agentzero-tools
    ${COGUTIL_LIBRARY}
    ${ATOMSPACE_LIBRARY}
)

# Add test to CTest
add_test(NAME ResourceManagerSimpleTest COMMAND ResourceManagerSimpleTest)

message(STATUS "Agent-Zero Tools tests configured successfully")
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
# Tests CMakeLists.txt for Agent-Zero Tools
cmake_minimum_required(VERSION 3.16)

# Find GTest
find_package(GTest QUIET)
find_package(Threads REQUIRED)

# Find PkgConfig
find_package(PkgConfig REQUIRED)

# Find OpenCog packages for tests
pkg_check_modules(COGUTIL cogutil)
if (NOT COGUTIL_FOUND)
    find_package(CogUtil REQUIRED)
    set(COGUTIL_INCLUDE_DIRS ${COGUTIL_INCLUDE_DIR})
    set(COGUTIL_LIBRARIES ${COGUTIL_LIBRARY})
endif()

pkg_check_modules(ATOMSPACE atomspace)
if (NOT ATOMSPACE_FOUND)
    find_package(AtomSpace REQUIRED)
    set(ATOMSPACE_INCLUDE_DIRS ${ATOMSPACE_INCLUDE_DIR})
    set(ATOMSPACE_LIBRARIES ${ATOMSPACE_LIBRARY})
endif()

if(GTEST_FOUND)
    enable_testing()
    
    # Include directories
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
        ${GTEST_INCLUDE_DIRS}
    )

    # Link directories  
    link_directories(
        ${COGUTIL_LIBRARY_DIRS}
        ${ATOMSPACE_LIBRARY_DIRS}
    )

    # ToolRegistry tests
    if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/ToolRegistryTest.cpp")
        add_executable(ToolRegistryTest ToolRegistryTest.cpp)
        
        target_link_libraries(ToolRegistryTest
            agentzero-tools
            ${COGUTIL_LIBRARIES}
            ${ATOMSPACE_LIBRARIES}
            ${GTEST_LIBRARIES}
            ${GTEST_MAIN_LIBRARIES}
            Threads::Threads
        )
        
        target_compile_features(ToolRegistryTest PRIVATE cxx_std_17)
        
        # Add the test to CTest
        add_test(NAME ToolRegistryTest COMMAND ToolRegistryTest)
        
        set_tests_properties(ToolRegistryTest
            PROPERTIES
            TIMEOUT 120
            LABELS "tools;registry"
        )
        
        message(STATUS "ToolRegistry tests configured")
    endif()
    
    message(STATUS "Agent-Zero Tools tests configured with GTest")
else()
    message(STATUS "GTest not found - tests will not be built")
    message(STATUS "Install GTest to enable unit testing")
endif()
