cmake_minimum_required(VERSION 3.16)
project(agentzero-perception VERSION 0.1.0 DESCRIPTION "Agent-Zero Perception Component")

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find required packages
find_package(CogUtil REQUIRED)
find_package(AtomSpace REQUIRED)

message(STATUS "Configuring Agent-Zero Perception Component")

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    ${COGUTIL_INCLUDE_DIR}
    ${ATOMSPACE_INCLUDE_DIR}
)

# === MultiModalSensor Library ===
set(MULTIMODAL_SENSOR_SOURCES
    src/MultiModalSensor.cc
    src/TextualSensor.cc
)

set(MULTIMODAL_SENSOR_HEADERS
    include/opencog/agentzero/perception/MultiModalSensor.h
    include/opencog/agentzero/perception/TextualSensor.h
)

add_library(agentzero-perception SHARED
    ${MULTIMODAL_SENSOR_SOURCES}
)

target_link_libraries(agentzero-perception
    ${COGUTIL_LIBRARY}
    ${ATOMSPACE_atombase_LIBRARY}
    ${ATOMSPACE_atomspace_LIBRARY}
    ${ATOMSPACE_value_LIBRARY}
    /usr/local/lib/opencog/libsensory.so
    /usr/local/lib/opencog/libsensory-types.so
)

# Set library properties
set_target_properties(agentzero-perception PROPERTIES
    VERSION ${PROJECT_VERSION}
    SOVERSION 1
    PUBLIC_HEADER "${MULTIMODAL_SENSOR_HEADERS}"
)

# === Installation ===
install(TARGETS agentzero-perception
    LIBRARY DESTINATION lib/opencog
    PUBLIC_HEADER DESTINATION include/opencog/agentzero/perception
)

# Install cmake config files
configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/cmake/AgentZeroPerceptionConfig.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/AgentZeroPerceptionConfig.cmake"
    @ONLY
)

install(FILES
    "${CMAKE_CURRENT_BINARY_DIR}/AgentZeroPerceptionConfig.cmake"
    DESTINATION lib/cmake/AgentZeroPerception
)

# === Testing (if CxxTest is available) ===
if(CXXTEST_FOUND)
    message(STATUS "CxxTest found, enabling unit tests for agentzero-perception")
    
    include_directories(${CXXTEST_INCLUDE_DIRS})
    
    # Add test files
    set(PERCEPTION_TEST_SOURCES
        tests/MultiModalSensorUTest.cxxtest
        tests/TextualSensorUTest.cxxtest
    )
    
    # Create test executables
    foreach(TEST_SOURCE ${PERCEPTION_TEST_SOURCES})
        get_filename_component(TEST_NAME ${TEST_SOURCE} NAME_WE)
        
        cxxtest_add_test(${TEST_NAME} ${TEST_NAME}.cc
            ${CMAKE_CURRENT_SOURCE_DIR}/${TEST_SOURCE}
        )
        
        target_link_libraries(${TEST_NAME}
            agentzero-perception
            ${COGUTIL_LIBRARY}
            ${ATOMSPACE_atombase_LIBRARY}
            ${ATOMSPACE_atomspace_LIBRARY}
        )
        
        # Add to test suite
        add_dependencies(test ${TEST_NAME})
    endforeach()
else()
    message(STATUS "CxxTest not found, skipping unit tests for agentzero-perception")
endif()

# === Examples ===
add_subdirectory(examples)

message(STATUS "Agent-Zero Perception Component configuration complete")

# === Build Summary ===
message(STATUS "")
message(STATUS "Agent-Zero Perception Component Build Summary:")
message(STATUS "  Version: ${PROJECT_VERSION}")
message(STATUS "  C++ Standard: C++${CMAKE_CXX_STANDARD}")
message(STATUS "  Source Files: ${MULTIMODAL_SENSOR_SOURCES}")
message(STATUS "  Headers: ${MULTIMODAL_SENSOR_HEADERS}")
message(STATUS "  CogUtil: ${COGUTIL_LIBRARY}")
message(STATUS "  AtomSpace: ${ATOMSPACE_atomspace_LIBRARY}")
message(STATUS "  Tests: ${CXXTEST_FOUND}")
message(STATUS "")