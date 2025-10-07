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
# Comprehensive Tests for Agent-Zero Learning Module
# Combines MOSES Policy Optimization with Advanced Experience Management

cmake_minimum_required(VERSION 3.16)

# Find CxxTest
find_package(CxxTest)

if(CXXTEST_FOUND)
    enable_testing()
    include_directories(${CXXTEST_INCLUDE_DIR})
    include_directories(${CMAKE_SOURCE_DIR}/include)
    
    # PolicyOptimizer tests (requires MOSES)
    if(MOSES_FOUND OR ASMOSES_FOUND)
        CXXTEST_ADD_TEST(PolicyOptimizerUTest PolicyOptimizerUTest.cpp
                         ${CMAKE_CURRENT_SOURCE_DIR}/PolicyOptimizerUTest.cxxtest)
        
        target_link_libraries(PolicyOptimizerUTest
            agentzero-learning
            ${COGUTIL_LIBRARIES}
            ${ATOMSPACE_LIBRARIES}
        )
        
        if(MOSES_LIBRARIES)
            target_link_libraries(PolicyOptimizerUTest ${MOSES_LIBRARIES})
        endif()
        
        if(ASMOSES_LIBRARIES)
            target_link_libraries(PolicyOptimizerUTest ${ASMOSES_LIBRARIES})
        endif()
        
        message(STATUS "PolicyOptimizer tests configured")
    else()
        message(STATUS "Skipping PolicyOptimizer tests - MOSES not found")
    endif()
    
    # ExperienceManager tests (comprehensive)
    CXXTEST_ADD_TEST(ExperienceManagerUTest ExperienceManagerUTest.cpp
                     ${CMAKE_CURRENT_SOURCE_DIR}/ExperienceManagerUTest.cxxtest)
    
    target_link_libraries(ExperienceManagerUTest
        agentzero-learning
        ${COGUTIL_LIBRARIES}
        ${ATOMSPACE_LIBRARIES}
    )
    
    # Additional ExperienceManager test from main branch (if exists)
    if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/ExperienceManagerTest.cxxtest)
        CXXTEST_ADD_TEST(experience_manager_test 
            experience_manager_test.cpp
            ${CMAKE_CURRENT_SOURCE_DIR}/ExperienceManagerTest.cxxtest
        )
        
        target_link_libraries(experience_manager_test
            agentzero-learning
            ${COGUTIL_LIBRARIES}
            ${ATOMSPACE_LIBRARIES}
            ${Boost_LIBRARIES}
        )
        
        message(STATUS "Additional ExperienceManager test configured")
    endif()
    
    message(STATUS "Comprehensive Agent-Zero Learning tests configured with CxxTest")
    
else()
    message(STATUS "CxxTest not found - creating simple test executables")
    
    # Simple ExperienceManager test
    if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/ExperienceManagerSimpleTest.cpp)
        add_executable(experience_manager_simple_test
            ExperienceManagerSimpleTest.cpp
        )
        
        target_link_libraries(experience_manager_simple_test
            agentzero-learning
            ${COGUTIL_LIBRARIES}
            ${ATOMSPACE_LIBRARIES}
            ${Boost_LIBRARIES}
        )
        
        add_test(NAME experience_manager_simple_test
                 COMMAND experience_manager_simple_test)
        
        message(STATUS "Simple ExperienceManager test configured")
    endif()
    
    message(STATUS "Simple test executables configured for Agent-Zero Learning")
endif()
# Tests for Agent-Zero Learning Module

cmake_minimum_required(VERSION 3.16)

# Find CxxTest if available
find_package(CxxTest)

if(CXXTEST_FOUND)
    # Enable testing
    include_directories(${CXXTEST_INCLUDE_DIR})
    enable_testing()
    
    # Test files
    set(EXPERIENCE_MANAGER_TEST_SOURCES
        ExperienceManagerTest.cxxtest
    )
    
    # Generate and add test
    CXXTEST_ADD_TEST(experience_manager_test 
        experience_manager_test.cpp
        ${CMAKE_CURRENT_SOURCE_DIR}/ExperienceManagerTest.cxxtest
    )
    
    target_link_libraries(experience_manager_test
        agentzero-learning
        ${COGUTIL_LIBRARIES}
        ${ATOMSPACE_LIBRARIES}
        ${Boost_LIBRARIES}
    )
    
    message(STATUS "CxxTest found - ExperienceManager tests enabled")
    
else()
    message(STATUS "CxxTest not found - creating simple test executable")
    
    # Create simple test executable
    add_executable(experience_manager_simple_test
        ExperienceManagerSimpleTest.cpp
    )
    
    target_link_libraries(experience_manager_simple_test
        agentzero-learning
        ${COGUTIL_LIBRARIES}
        ${ATOMSPACE_LIBRARIES}
        ${Boost_LIBRARIES}
    )
    
    # Add to tests
    add_test(NAME experience_manager_simple_test
             COMMAND experience_manager_simple_test)
    
    message(STATUS "Simple test executable created for ExperienceManager")
endif()
