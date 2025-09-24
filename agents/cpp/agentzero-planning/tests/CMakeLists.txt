#<<<<<<< copilot/fix-38
cmake_minimum_required(VERSION 3.16)

# Find GTest
find_package(GTest REQUIRED)
find_package(Threads REQUIRED)

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../include
    ${COGUTIL_INCLUDE_DIR}
    ${ATOMSPACE_INCLUDE_DIR}
    ${OCTOMAP_INCLUDE_DIRS}
    ${GTEST_INCLUDE_DIRS}
)

if(SPACETIME_FOUND)
    include_directories(${SPACETIME_INCLUDE_DIR})
endif()

# Test source files
set(TEST_SOURCES
    test_spacetime_integrator.cpp
)

# Create test executable
add_executable(test_agentzero_planning ${TEST_SOURCES})

# Link libraries
target_link_libraries(test_agentzero_planning
    agentzero-planning
    ${COGUTIL_LIBRARY}
    ${ATOMSPACE_LIBRARY}
    ${OCTOMAP_LIBRARIES}
    ${GTEST_LIBRARIES}
    ${GTEST_MAIN_LIBRARIES}
    Threads::Threads
)

if(SPACETIME_FOUND)
    target_link_libraries(test_agentzero_planning ${SPACETIME_LIBRARY})
endif()

# Add tests to CTest
add_test(NAME SpaceTimeIntegratorTests COMMAND test_agentzero_planning)

# Set test properties
set_tests_properties(SpaceTimeIntegratorTests PROPERTIES
    TIMEOUT 60
    ENVIRONMENT "OPENCOG_LOG_LEVEL=INFO"
)
#=======
# Agent-Zero Planning Module Tests CMakeLists.txt

cmake_minimum_required(VERSION 3.16)

# Find CxxTest for unit testing
find_program(CXXTEST_TESTGEN_EXECUTABLE cxxtestgen)
if(CXXTEST_TESTGEN_EXECUTABLE)
    message(STATUS "Found CxxTest test generator: ${CXXTEST_TESTGEN_EXECUTABLE}")
    
    # Include CxxTest
    find_path(CXXTEST_INCLUDE_DIR
        NAMES cxxtest/TestSuite.h
        PATHS /usr/include /usr/local/include
    )
    
    if(CXXTEST_INCLUDE_DIR)
        include_directories(${CXXTEST_INCLUDE_DIR})
        
        # Test files
        set(PLANNING_TEST_SOURCES
            test_PlanningEngine.h
            test_GoalHierarchy.h
            test_TemporalReasoner.h
        )
        
        # Generate test runner
        add_custom_command(
            OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/test_runner.cpp
            COMMAND ${CXXTEST_TESTGEN_EXECUTABLE} --runner=ErrorPrinter
                    -o ${CMAKE_CURRENT_BINARY_DIR}/test_runner.cpp
                    ${CMAKE_CURRENT_SOURCE_DIR}/test_PlanningEngine.h
                    ${CMAKE_CURRENT_SOURCE_DIR}/test_GoalHierarchy.h
                    ${CMAKE_CURRENT_SOURCE_DIR}/test_TemporalReasoner.h
            DEPENDS ${PLANNING_TEST_SOURCES}
            WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        )
        
        # Create test executable
        add_executable(agentzero-planning-tests
            ${CMAKE_CURRENT_BINARY_DIR}/test_runner.cpp
        )
        
        target_link_libraries(agentzero-planning-tests
            agentzero-planning
            ${COGUTIL_LIBRARIES}
            ${ATOMSPACE_LIBRARIES}
            ${SPACETIME_LIBRARIES}
        )
        
        target_include_directories(agentzero-planning-tests PRIVATE
            ${CMAKE_CURRENT_SOURCE_DIR}/../include
            ${CMAKE_CURRENT_SOURCE_DIR}/../src
        )
        
        # Add test to CTest
        add_test(NAME agentzero-planning-unit-tests 
                 COMMAND agentzero-planning-tests)
        
        message(STATUS "Agent-Zero Planning tests configured")
    else()
        message(WARNING "CxxTest headers not found - tests will not be built")
    endif()
else()
    message(WARNING "CxxTest not found - tests will not be built")
endif()
#>>>>>>> main
