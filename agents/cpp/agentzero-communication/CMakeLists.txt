# Agent-Zero Communication Module CMakeLists.txt

cmake_minimum_required(VERSION 3.16)

project(AgentZeroCommunication
    VERSION 0.1.0
    DESCRIPTION "Agent-Zero Communication Protocols for OpenCog Integration"
    LANGUAGES CXX
)

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find required OpenCog packages
find_package(PkgConfig REQUIRED)

# Find cogutil
pkg_check_modules(COGUTIL REQUIRED cogutil)
if (COGUTIL_FOUND)
    message(STATUS "Found cogutil version ${COGUTIL_VERSION}")
    include_directories(${COGUTIL_INCLUDE_DIRS})
    link_directories(${COGUTIL_LIBRARY_DIRS})
endif()

# Find atomspace  
pkg_check_modules(ATOMSPACE REQUIRED atomspace)
if (ATOMSPACE_FOUND)
    message(STATUS "Found atomspace version ${ATOMSPACE_VERSION}")
    include_directories(${ATOMSPACE_INCLUDE_DIRS})
    link_directories(${ATOMSPACE_LIBRARY_DIRS})
endif()

# Find cogserver (optional but recommended for communication)
pkg_check_modules(COGSERVER cogserver)
if (COGSERVER_FOUND)
    message(STATUS "Found cogserver version ${COGSERVER_VERSION}")
    include_directories(${COGSERVER_INCLUDE_DIRS})
    link_directories(${COGSERVER_LIBRARY_DIRS})
else()
    message(STATUS "CogServer not found - network communication features will be disabled")
    set(COGSERVER_FOUND FALSE)
endif()

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    ${CMAKE_CURRENT_SOURCE_DIR}/src
)

# Source files
set(AGENTZERO_COMMUNICATION_SOURCES
    src/AgentComms.cpp
    src/MessageRouter.cpp
    src/ProtocolManager.cpp
    src/MessageSerializer.cpp
)

# Header files
set(AGENTZERO_COMMUNICATION_HEADERS
    include/opencog/agentzero/communication/AgentComms.h
    include/opencog/agentzero/communication/MessageRouter.h
    include/opencog/agentzero/communication/ProtocolManager.h
    include/opencog/agentzero/communication/MessageSerializer.h
    include/opencog/agentzero/communication/CommTypes.h
)

# Create library
add_library(agentzero-communication SHARED
    ${AGENTZERO_COMMUNICATION_SOURCES}
    ${AGENTZERO_COMMUNICATION_HEADERS}
)

target_link_libraries(agentzero-communication
    ${COGUTIL_LIBRARIES}
    ${ATOMSPACE_LIBRARIES}
    ${Boost_LIBRARIES}
)

# Link cogserver libraries if available
if(COGSERVER_FOUND AND COGSERVER_LIBRARIES)
    target_link_libraries(agentzero-communication ${COGSERVER_LIBRARIES})
    target_compile_definitions(agentzero-communication PRIVATE HAVE_COGSERVER=1)
    message(STATUS "Linking cogserver libraries: ${COGSERVER_LIBRARIES}")
endif()

target_include_directories(agentzero-communication PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>
)

# Installation
install(TARGETS agentzero-communication
    EXPORT AgentZeroCppTargets
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    RUNTIME DESTINATION bin
    INCLUDES DESTINATION include
)

install(DIRECTORY include/
    DESTINATION include
    FILES_MATCHING PATTERN "*.h"
)

# Tests
if(BUILD_TESTING)
    enable_testing()
    add_subdirectory(tests)
endif()

# Examples
if(BUILD_EXAMPLES)
    add_subdirectory(examples)
endif()

# Status message
message(STATUS "Agent-Zero Communication module configuration complete")
message(STATUS "Ready for build and testing with OpenCog integration")