# Agent-Zero Communication Module CMakeLists.txt
# Part of AZ-HUMAN-001: Create HumanInterface layer
# Phase 6: Communication & NLP - Human-Agent Interaction

cmake_minimum_required(VERSION 3.16)

project(AgentZeroCommunication
    VERSION 0.1.0
    DESCRIPTION "Agent-Zero Communication & Human Interface Module for OpenCog Integration"
    LANGUAGES CXX
)

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

message(STATUS "Configuring Agent-Zero Communication Module...")

# Find required dependencies
find_package(PkgConfig REQUIRED)

# Core OpenCog dependencies (required for communication module)
pkg_check_modules(COGUTIL REQUIRED cogutil)
if (COGUTIL_FOUND)
    message(STATUS "Found cogutil version ${COGUTIL_VERSION}")
    include_directories(${COGUTIL_INCLUDE_DIRS})
    link_directories(${COGUTIL_LIBRARY_DIRS})
endif()

pkg_check_modules(ATOMSPACE REQUIRED atomspace)
if (ATOMSPACE_FOUND)
    message(STATUS "Found atomspace version ${ATOMSPACE_VERSION}")
    include_directories(${ATOMSPACE_INCLUDE_DIRS})
    link_directories(${ATOMSPACE_LIBRARY_DIRS})
endif()

# Optional OpenCog dependencies for enhanced communication
pkg_check_modules(COGSERVER cogserver)
if (COGSERVER_FOUND)
    message(STATUS "Found cogserver version ${COGSERVER_VERSION}")
    include_directories(${COGSERVER_INCLUDE_DIRS})
    link_directories(${COGSERVER_LIBRARY_DIRS})
    add_definitions(-DHAVE_COGSERVER)
else()
    message(STATUS "CogServer not found - some features will be disabled")
endif()

# Link-Grammar for natural language processing (future integration)
pkg_check_modules(LG_ATOMESE lg-atomese)
if (LG_ATOMESE_FOUND)
    message(STATUS "Found lg-atomese version ${LG_ATOMESE_VERSION}")
    include_directories(${LG_ATOMESE_INCLUDE_DIRS})
    link_directories(${LG_ATOMESE_LIBRARY_DIRS})
    add_definitions(-DHAVE_LG_ATOMESE)
else()
    message(STATUS "lg-atomese not found - NLP features will be basic")
endif()

# Find Boost (required by OpenCog)
find_package(Boost REQUIRED COMPONENTS
    system
    filesystem
    thread
    date_time
)

if(Boost_FOUND)
    message(STATUS "Found Boost ${Boost_VERSION}: ${Boost_INCLUDE_DIRS}")
    include_directories(${Boost_INCLUDE_DIRS})
    link_directories(${Boost_LIBRARY_DIRS})
endif()

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    ${CMAKE_CURRENT_SOURCE_DIR}/src
    ${CMAKE_CURRENT_SOURCE_DIR}/../agentzero-core/include  # For AgentZero core integration
)

# Source files
set(COMMUNICATION_SOURCES
    src/HumanInterface.cpp
    src/CommunicationUtils.cpp
)

# Header files (for installation)
set(COMMUNICATION_HEADERS
    include/agentzero/communication/HumanInterface.h
    include/agentzero/communication/CommunicationTypes.h
    include/agentzero/communication/CommunicationUtils.h
)

# Create the library
add_library(agentzero-communication SHARED ${COMMUNICATION_SOURCES})

# Set library properties
set_target_properties(agentzero-communication PROPERTIES
    VERSION ${PROJECT_VERSION}
    SOVERSION 0
    PUBLIC_HEADER "${COMMUNICATION_HEADERS}"
    POSITION_INDEPENDENT_CODE ON
)

# Include directories for the target
target_include_directories(agentzero-communication PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>
)

# Link required libraries
target_link_libraries(agentzero-communication
    ${COGUTIL_LIBRARIES}
    ${ATOMSPACE_LIBRARIES}
    ${Boost_LIBRARIES}
    pthread
)

# Link optional libraries with proper feature flags
if(COGSERVER_FOUND AND COGSERVER_LIBRARIES)
    target_link_libraries(agentzero-communication ${COGSERVER_LIBRARIES})
    target_compile_definitions(agentzero-communication PRIVATE HAVE_COGSERVER=1)
    message(STATUS "Linking cogserver libraries: ${COGSERVER_LIBRARIES}")
endif()

if(LG_ATOMESE_FOUND AND LG_ATOMESE_LIBRARIES)
    target_link_libraries(agentzero-communication ${LG_ATOMESE_LIBRARIES})
    target_compile_definitions(agentzero-communication PRIVATE HAVE_LG_ATOMESE=1)
    message(STATUS "Linking lg-atomese libraries: ${LG_ATOMESE_LIBRARIES}")
endif()

# Compiler flags
target_compile_definitions(agentzero-communication PRIVATE
    AGENTZERO_COMMUNICATION_VERSION="${PROJECT_VERSION}"
)

# Installation
include(GNUInstallDirs)

install(TARGETS agentzero-communication
    EXPORT AgentZeroCommunicationTargets
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/agentzero/communication
)

# Install all headers
install(DIRECTORY include/agentzero/
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/agentzero
    FILES_MATCHING PATTERN "*.h"
)

# Install export targets
install(EXPORT AgentZeroCommunicationTargets
    FILE AgentZeroCommunicationTargets.cmake
    NAMESPACE AgentZero::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/AgentZeroCommunication
)

# Configure package config file
configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/cmake/AgentZeroCommunicationConfig.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/AgentZeroCommunicationConfig.cmake"
    @ONLY
)

install(FILES
    "${CMAKE_CURRENT_BINARY_DIR}/AgentZeroCommunicationConfig.cmake"
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/AgentZeroCommunication
)

# Generate pkgconfig file
configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/agentzero-communication.pc.in
    ${CMAKE_CURRENT_BINARY_DIR}/agentzero-communication.pc
    @ONLY
)

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/agentzero-communication.pc
    DESTINATION lib/pkgconfig
)

# Examples
if(BUILD_EXAMPLES)
    add_subdirectory(examples)
endif()

# Tests
if(BUILD_TESTING)
    enable_testing()
    add_subdirectory(tests)
endif()

# Report configuration
message(STATUS "Agent-Zero Communication Dependencies:")
message(STATUS "  cogutil: ${COGUTIL_VERSION}")
message(STATUS "  atomspace: ${ATOMSPACE_VERSION}")
message(STATUS "  cogserver: ${COGSERVER_FOUND}")
message(STATUS "  lg-atomese: ${LG_ATOMESE_FOUND}")
message(STATUS "  boost: ${Boost_VERSION}")

# Status summary
message(STATUS "Agent-Zero Communication Configuration:")
message(STATUS "  Version: ${PROJECT_VERSION}")
message(STATUS "  CogServer integration: ${COGSERVER_FOUND}")
message(STATUS "  NLP integration: ${LG_ATOMESE_FOUND}")
message(STATUS "  Build examples: ${BUILD_EXAMPLES}")
message(STATUS "  Build tests: ${BUILD_TESTING}")
message(STATUS "")
message(STATUS "Human Interface layer ready for build and testing")