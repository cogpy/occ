# Agent-Zero Learning Tests

# Find test dependencies
find_package(PkgConfig REQUIRED)
pkg_check_modules(CXXTEST cxxtest)

if(CXXTEST_FOUND)
    message(STATUS "Found CxxTest for agentzero-learning tests")
    
    # Include directories
    include_directories(
        ${CMAKE_CURRENT_SOURCE_DIR}/../include
        ${CXXTEST_INCLUDE_DIRS}
        ${COGUTIL_INCLUDE_DIRS}
        ${ATOMSPACE_INCLUDE_DIRS}
    )

    # Test source files
    set(TEST_SOURCES
        ASMOSESIntegratorTest.cxxtest
        AtomSpaceEvolverTest.cxxtest
    )

    # Generate and add tests
    foreach(TEST_SOURCE ${TEST_SOURCES})
        get_filename_component(TEST_NAME ${TEST_SOURCE} NAME_WE)
        
        add_custom_command(
            OUTPUT ${TEST_NAME}.cpp
            COMMAND ${CXXTEST_TESTGEN_EXECUTABLE} --error-printer -o ${TEST_NAME}.cpp ${CMAKE_CURRENT_SOURCE_DIR}/${TEST_SOURCE}
            DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${TEST_SOURCE}
        )
        
        add_executable(${TEST_NAME} ${TEST_NAME}.cpp)
        target_link_libraries(${TEST_NAME} 
            agentzero-learning
            ${COGUTIL_LIBRARIES}
            ${ATOMSPACE_LIBRARIES}
        )
        
        add_test(NAME ${TEST_NAME} COMMAND ${TEST_NAME})
    endforeach()
else()
    message(STATUS "CxxTest not found - skipping agentzero-learning tests")
endif()