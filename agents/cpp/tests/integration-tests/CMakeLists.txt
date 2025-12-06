# Integration Tests CMakeLists.txt
# AZ-INT-001: Comprehensive integration testing

cmake_minimum_required(VERSION 3.16)

if(CXXTEST_FOUND)
    # Component Integration Tests
    if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/ComponentIntegrationTest.cxxtest")
        cxxtest_add_test(ComponentIntegrationTest ComponentIntegrationTest.cpp 
                        "${CMAKE_CURRENT_SOURCE_DIR}/ComponentIntegrationTest.cxxtest")
        
        target_link_libraries(ComponentIntegrationTest
            agentzero-test-framework
            ${COGUTIL_LIBRARIES}
            ${ATOMSPACE_LIBRARIES}
            ${COGSERVER_LIBRARIES}
            ${Boost_LIBRARIES}
        )
    endif()
    
    # AtomSpace Integration Tests
    if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/AtomSpaceIntegrationTest.cxxtest")
        cxxtest_add_test(AtomSpaceIntegrationTest AtomSpaceIntegrationTest.cpp 
                        "${CMAKE_CURRENT_SOURCE_DIR}/AtomSpaceIntegrationTest.cxxtest")
        
        target_link_libraries(AtomSpaceIntegrationTest
            agentzero-test-framework
            ${COGUTIL_LIBRARIES}
            ${ATOMSPACE_LIBRARIES}
            ${COGSERVER_LIBRARIES}
            ${Boost_LIBRARIES}
        )
    endif()
    
    # CogServer Integration Tests
    if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/CogServerIntegrationTest.cxxtest")
        cxxtest_add_test(CogServerIntegrationTest CogServerIntegrationTest.cpp 
                        "${CMAKE_CURRENT_SOURCE_DIR}/CogServerIntegrationTest.cxxtest")
        
        target_link_libraries(CogServerIntegrationTest
            agentzero-test-framework
            ${COGUTIL_LIBRARIES}
            ${ATOMSPACE_LIBRARIES}
            ${COGSERVER_LIBRARIES}
            ${Boost_LIBRARIES}
        )
    endif()
    
    # End-to-End Integration Tests
    if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/EndToEndIntegrationTest.cxxtest")
        cxxtest_add_test(EndToEndIntegrationTest EndToEndIntegrationTest.cpp 
                        "${CMAKE_CURRENT_SOURCE_DIR}/EndToEndIntegrationTest.cxxtest")
        
        target_link_libraries(EndToEndIntegrationTest
            agentzero-test-framework
            ${COGUTIL_LIBRARIES}
            ${ATOMSPACE_LIBRARIES}
            ${COGSERVER_LIBRARIES}
            ${Boost_LIBRARIES}
        )
    endif()
    
    message(STATUS "Agent-Zero comprehensive integration tests configured (AZ-INT-001)")
    message(STATUS "  - ComponentIntegrationTest: Cross-component interactions (10 tests)")
    message(STATUS "  - AtomSpaceIntegrationTest: AtomSpace/cogutil integration (15 tests)")
    message(STATUS "  - CogServerIntegrationTest: CogServer integration (15 tests)")
    message(STATUS "  - EndToEndIntegrationTest: Complete workflows (10 tests)")
    message(STATUS "  Total: 50 comprehensive integration tests")
endif()