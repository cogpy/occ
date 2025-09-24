cmake_minimum_required(VERSION 3.16)

message(STATUS "Configuring Agent-Zero Learning tests...")

# Find CxxTest for unit testing
find_package(PkgConfig REQUIRED)
pkg_check_modules(CXXTEST cxxtest)

if(NOT CXXTEST_FOUND)
    find_program(CXXTEST_TESTGEN cxxtestgen PATHS /usr/bin /usr/local/bin)
    if(CXXTEST_TESTGEN)
        message(STATUS "Found cxxtestgen: ${CXXTEST_TESTGEN}")
        set(CXXTEST_FOUND TRUE)
    endif()
endif()

if(CXXTEST_FOUND OR CXXTEST_TESTGEN)
    # Test source files
    set(TEST_SOURCES
        SkillAcquisitionTest.cpp
        ExperienceManagerTest.cpp
        PolicyOptimizerTest.cpp
        MetaLearningTest.cpp
        IntegrationTest.cpp
    )

    # Include directories for tests
    include_directories(
        ${CMAKE_CURRENT_SOURCE_DIR}
        ${CMAKE_CURRENT_SOURCE_DIR}/../include
        ${CMAKE_CURRENT_SOURCE_DIR}/../src
        ${COGUTIL_INCLUDE_DIR}
        ${ATOMSPACE_INCLUDE_DIR}
        ${MOSES_INCLUDE_DIR}
    )

    # Create test executables
    foreach(TEST_SOURCE ${TEST_SOURCES})
        get_filename_component(TEST_NAME ${TEST_SOURCE} NAME_WE)
        
        add_executable(${TEST_NAME} ${TEST_SOURCE})
        
        target_link_libraries(${TEST_NAME}
            agentzero-learning
            ${COGUTIL_LIBRARY}
            ${ATOMSPACE_LIBRARY}
            ${MOSES_LIBRARY}
        )
        
        # Add test to CTest
        add_test(NAME ${TEST_NAME} COMMAND ${TEST_NAME})
        
        message(STATUS "Configured test: ${TEST_NAME}")
    endforeach()

    message(STATUS "Agent-Zero Learning tests configured successfully")
else()
    message(STATUS "CxxTest not found, skipping Agent-Zero Learning tests")
endif()