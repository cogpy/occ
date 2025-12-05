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
