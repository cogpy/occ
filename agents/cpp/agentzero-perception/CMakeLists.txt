# <<<<<<< copilot/fix-26
#
# CMakeLists.txt for agentzero-perception module
# Part of the AGENT-ZERO-GENESIS project - Task AZ-PERC-002
#

cmake_minimum_required(VERSION 3.16)
project(agentzero-perception)

# ===================================================================
# Component Information
# ===================================================================

set(COMPONENT_NAME "agentzero-perception")
set(COMPONENT_DESCRIPTION "Agent-Zero Perception & Sensory Integration")
set(COMPONENT_VERSION "1.0.0")

# ===================================================================
# Dependencies
# ===================================================================

# Find OpenCog components (required)
find_package(PkgConfig REQUIRED)

# CogUtil (required for logging and utilities)
pkg_check_modules(COGUTIL REQUIRED cogutil)
if(NOT COGUTIL_FOUND)
    message(FATAL_ERROR "CogUtil not found. Please install cogutil first.")
endif()

# AtomSpace (required for knowledge representation)
pkg_check_modules(ATOMSPACE REQUIRED atomspace)
if(NOT ATOMSPACE_FOUND)
    message(FATAL_ERROR "AtomSpace not found. Please install atomspace first.")
endif()

# Optional OpenCog perception components
pkg_check_modules(PERCEPTION perception)
pkg_check_modules(SENSORY sensory)
pkg_check_modules(VISION vision)
pkg_check_modules(ATTENTION attention)

# Boost (required)
find_package(Boost REQUIRED COMPONENTS system thread)

# ===================================================================
# Include Directories
# ===================================================================

include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    ${COGUTIL_INCLUDE_DIRS}
    ${ATOMSPACE_INCLUDE_DIRS}
    ${Boost_INCLUDE_DIRS}
)

if(PERCEPTION_FOUND)
    include_directories(${PERCEPTION_INCLUDE_DIRS})
    add_definitions(-DHAVE_PERCEPTION)
endif()

if(SENSORY_FOUND)
    include_directories(${SENSORY_INCLUDE_DIRS})
    add_definitions(-DHAVE_SENSORY)
endif()

if(VISION_FOUND)
    include_directories(${VISION_INCLUDE_DIRS})
    add_definitions(-DHAVE_VISION)
endif()

if(ATTENTION_FOUND)
    include_directories(${ATTENTION_INCLUDE_DIRS})
    add_definitions(-DHAVE_ATTENTION)
endif()

# ===================================================================
# Compiler Flags
# ===================================================================

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Compiler-specific flags
if(CMAKE_COMPILER_IS_GNUCXX)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -Wno-unused-parameter")
endif()

# ===================================================================
# Source Files
# ===================================================================

set(AGENTZERO_PERCEPTION_SOURCES
    src/PerceptualProcessor.cpp
    src/MultiModalSensor.cpp
)

set(AGENTZERO_PERCEPTION_HEADERS
    include/opencog/agentzero/PerceptualProcessor.h
    include/opencog/agentzero/MultiModalSensor.h
)

# ===================================================================
# Library Target
# ===================================================================

add_library(agentzero-perception SHARED ${AGENTZERO_PERCEPTION_SOURCES})

# Link libraries
target_link_libraries(agentzero-perception
    ${COGUTIL_LIBRARIES}
    ${ATOMSPACE_LIBRARIES}
    ${Boost_LIBRARIES}
)

# Link optional perception libraries if available
if(PERCEPTION_FOUND)
    target_link_libraries(agentzero-perception ${PERCEPTION_LIBRARIES})
endif()

if(SENSORY_FOUND)
    target_link_libraries(agentzero-perception ${SENSORY_LIBRARIES})
endif()

if(VISION_FOUND)
    target_link_libraries(agentzero-perception ${VISION_LIBRARIES})
endif()

if(ATTENTION_FOUND)
    target_link_libraries(agentzero-perception ${ATTENTION_LIBRARIES})
endif()

# Set library properties
set_target_properties(agentzero-perception PROPERTIES
    VERSION ${COMPONENT_VERSION}
    SOVERSION 1
    EXPORT_NAME AgentZeroPerception
)

# ===================================================================
# Installation
# ===================================================================

# Install library
install(TARGETS agentzero-perception
    EXPORT AgentZeroPerceptionTargets
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    RUNTIME DESTINATION bin
    INCLUDES DESTINATION include
)

# Install headers
install(FILES ${AGENTZERO_PERCEPTION_HEADERS}
    DESTINATION include/opencog/agentzero
)

# Install CMake config
install(EXPORT AgentZeroPerceptionTargets
    FILE AgentZeroPerceptionTargets.cmake
    NAMESPACE AgentZero::
    DESTINATION lib/cmake/AgentZeroPerception
)

# ===================================================================
# Testing
# ===================================================================

# Enable testing if requested
if(BUILD_TESTING)
    enable_testing()
    add_subdirectory(tests)
endif()

# ===================================================================
# Status Information
# ===================================================================

message(STATUS "")
message(STATUS "========================================")
message(STATUS "Agent-Zero Perception Module Configuration")
message(STATUS "========================================")
message(STATUS "Component: ${COMPONENT_NAME}")
message(STATUS "Description: ${COMPONENT_DESCRIPTION}")
message(STATUS "Version: ${COMPONENT_VERSION}")
message(STATUS "")
message(STATUS "Required Dependencies:")
message(STATUS "  CogUtil: ${COGUTIL_FOUND}")
message(STATUS "  AtomSpace: ${ATOMSPACE_FOUND}")
message(STATUS "  Boost: ${Boost_FOUND}")
message(STATUS "")
message(STATUS "Optional Dependencies:")
message(STATUS "  Perception: ${PERCEPTION_FOUND}")
message(STATUS "  Sensory: ${SENSORY_FOUND}")
message(STATUS "  Vision: ${VISION_FOUND}")
message(STATUS "  Attention: ${ATTENTION_FOUND}")
message(STATUS "")
message(STATUS "Build Configuration:")
message(STATUS "  Testing: ${BUILD_TESTING}")
message(STATUS "  C++ Standard: ${CMAKE_CXX_STANDARD}")
message(STATUS "========================================")
# =======
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
# >>>>>>> main
message(STATUS "")