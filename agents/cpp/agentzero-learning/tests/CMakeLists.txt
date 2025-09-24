# Agent-Zero Learning Module Tests
# Part of AZ-LEARN-003: MOSES Policy Optimization Integration

cmake_minimum_required(VERSION 3.16)

# Find CxxTest
find_package(CxxTest)

if(CXXTEST_FOUND)
    enable_testing()
    include_directories(${CXXTEST_INCLUDE_DIR})
    include_directories(${CMAKE_SOURCE_DIR}/include)
    
    # Basic unit tests
    CXXTEST_ADD_TEST(PolicyOptimizerUTest PolicyOptimizerUTest.cpp
                     ${CMAKE_CURRENT_SOURCE_DIR}/PolicyOptimizerUTest.cxxtest)
    
    target_link_libraries(PolicyOptimizerUTest
        agentzero-learning
        ${COGUTIL_LIBRARIES}
        ${ATOMSPACE_LIBRARIES}
        ${MOSES_LIBRARIES}
        ${ASMOSES_LIBRARIES}
    )
    
    CXXTEST_ADD_TEST(ExperienceManagerUTest ExperienceManagerUTest.cpp
                     ${CMAKE_CURRENT_SOURCE_DIR}/ExperienceManagerUTest.cxxtest)
    
    target_link_libraries(ExperienceManagerUTest
        agentzero-learning
        ${COGUTIL_LIBRARIES}
        ${ATOMSPACE_LIBRARIES}
    )
    
    message(STATUS "Agent-Zero Learning tests configured")
else()
    message(STATUS "CxxTest not found, tests will not be built")
endif()