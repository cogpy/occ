# Agent-Zero Communication Module Tests

cmake_minimum_required(VERSION 3.16)

# Find test framework
find_package(PkgConfig REQUIRED)

# Try to find CxxTest
find_program(CXXTEST_BIN cxxtestgen)
find_path(CXXTEST_INCLUDE_DIR cxxtest/TestSuite.h)

if(CXXTEST_BIN AND CXXTEST_INCLUDE_DIR)
    message(STATUS "Found CxxTest: ${CXXTEST_BIN}")
    include_directories(${CXXTEST_INCLUDE_DIR})
    
    # Test source files
    set(TEST_SOURCES
        DialogueManagerUTest.cxxtest
        ConversationStateUTest.cxxtest
        LanguageProcessorUTest.cxxtest
        MessageHandlerUTest.cxxtest
    )
    
    # Generate and build tests
    foreach(TEST_SOURCE ${TEST_SOURCES})
        get_filename_component(TEST_NAME ${TEST_SOURCE} NAME_WE)
        set(TEST_CPP ${CMAKE_CURRENT_BINARY_DIR}/${TEST_NAME}.cpp)
        
        # Generate test file
        add_custom_command(
            OUTPUT ${TEST_CPP}
            COMMAND ${CXXTEST_BIN} --error-printer -o ${TEST_CPP} ${CMAKE_CURRENT_SOURCE_DIR}/${TEST_SOURCE}
            DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${TEST_SOURCE}
            COMMENT "Generating ${TEST_NAME}.cpp from ${TEST_SOURCE}"
        )
        
        # Create test executable
        add_executable(${TEST_NAME} ${TEST_CPP})
        target_link_libraries(${TEST_NAME} 
            agentzero-communication
            ${COGUTIL_LIBRARIES}
            ${ATOMSPACE_LIBRARIES}
            ${Boost_LIBRARIES}
        )
        
        # Add to test suite
        add_test(NAME ${TEST_NAME} COMMAND ${TEST_NAME})
    endforeach()
    
    message(STATUS "Communication module tests configured with CxxTest")
else()
    message(STATUS "CxxTest not found - tests will be disabled")
endif()