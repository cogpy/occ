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
