# Integration Tests CMakeLists.txt
# AZ-TEST-001: Component integration testing

cmake_minimum_required(VERSION 3.16)

if(CXXTEST_FOUND)
    # Component Integration Tests
    if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/ComponentIntegrationTest.cxxtest")
        cxxtest_add_test(ComponentIntegrationTest ComponentIntegrationTest.cpp 
                        "${CMAKE_CURRENT_SOURCE_DIR}/ComponentIntegrationTest.cxxtest")
        
        target_link_libraries(ComponentIntegrationTest
            agentzero-test-framework
            agentzero-core
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
            agentzero-core
            ${COGUTIL_LIBRARIES}
            ${ATOMSPACE_LIBRARIES}
            ${COGSERVER_LIBRARIES}
            ${Boost_LIBRARIES}
        )
    endif()
    
    message(STATUS "Agent-Zero integration tests configured")
endif()