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