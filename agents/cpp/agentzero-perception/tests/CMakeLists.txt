#
# CMakeLists.txt for agentzero-perception tests
#

cmake_minimum_required(VERSION 3.16)

# Find CxxTest
find_package(CxxTest)
if(NOT CXXTEST_FOUND)
    message(WARNING "CxxTest not found. Tests will be skipped.")
    return()
endif()

# Include CxxTest
include_directories(${CXXTEST_INCLUDE_DIR})

# Test sources
set(TEST_SOURCES
    PerceptualProcessorUTest.cxxtest
    MultiModalSensorUTest.cxxtest
)

# Create test executable for each test file
foreach(TEST_SOURCE ${TEST_SOURCES})
    get_filename_component(TEST_NAME ${TEST_SOURCE} NAME_WE)
    
    # Generate test runner
    cxxtest_add_test(${TEST_NAME}_test ${TEST_NAME}_test.cpp ${CMAKE_CURRENT_SOURCE_DIR}/${TEST_SOURCE})
    
    # Link with the library and dependencies
    target_link_libraries(${TEST_NAME}_test
        agentzero-perception
        ${COGUTIL_LIBRARIES}
        ${ATOMSPACE_LIBRARIES}
        ${Boost_LIBRARIES}
    )
    
    # Add additional required libraries for tests
    target_link_libraries(${TEST_NAME}_test
        /usr/local/lib/opencog/libatombase.so
        /usr/local/lib/opencog/libatom_types.so
        /usr/local/lib/opencog/libvalue.so
        /usr/local/lib/opencog/libtruthvalue.so
    )
    
    # Set include directories
    target_include_directories(${TEST_NAME}_test PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}/../include
        ${COGUTIL_INCLUDE_DIRS}
        ${ATOMSPACE_INCLUDE_DIRS}
        ${Boost_INCLUDE_DIRS}
    )
    
    # Add to test suite
    add_test(NAME ${TEST_NAME} COMMAND ${TEST_NAME}_test)
endforeach()

# Group tests
set_property(TEST PerceptualProcessorUTest PROPERTY LABELS "perception;unit")
set_property(TEST MultiModalSensorUTest PROPERTY LABELS "sensor;unit")