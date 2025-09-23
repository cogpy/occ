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