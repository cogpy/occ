# Agent-Zero Communication Module CMakeLists.txt

cmake_minimum_required(VERSION 3.16)

project(AgentZeroCommunication
    VERSION 0.1.0
    DESCRIPTION "Agent-Zero Communication & NLP Module for OpenCog Integration"
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
pkg_check_modules(ATOMSPACE atomspace)
if (ATOMSPACE_FOUND)
    message(STATUS "Found atomspace version ${ATOMSPACE_VERSION}")
    include_directories(${ATOMSPACE_INCLUDE_DIRS})
    link_directories(${ATOMSPACE_LIBRARY_DIRS})
else()
    message(STATUS "atomspace not found via pkg-config, looking for system installation")
    find_path(ATOMSPACE_INCLUDE_DIR
        NAMES opencog/atomspace/AtomSpace.h
        PATHS /usr/local/include /usr/include
    )
    find_library(ATOMSPACE_LIBRARY
        NAMES atomspace
        PATHS /usr/local/lib/opencog /usr/local/lib /usr/lib
    )
    if(ATOMSPACE_INCLUDE_DIR AND ATOMSPACE_LIBRARY)
        set(ATOMSPACE_FOUND TRUE)
        set(ATOMSPACE_LIBRARIES ${ATOMSPACE_LIBRARY})
        message(STATUS "Found atomspace at ${ATOMSPACE_INCLUDE_DIR} and ${ATOMSPACE_LIBRARY}")
        include_directories(${ATOMSPACE_INCLUDE_DIR})
    else()
        message(FATAL_ERROR "atomspace not found. Please install atomspace.")
    endif()
endif()

# Find lg-atomese
pkg_check_modules(LG_ATOMESE lg-atomese)
if (LG_ATOMESE_FOUND)
    message(STATUS "Found lg-atomese version ${LG_ATOMESE_VERSION}")
    include_directories(${LG_ATOMESE_INCLUDE_DIRS})
    link_directories(${LG_ATOMESE_LIBRARY_DIRS})
else()
    message(STATUS "lg-atomese not found via pkg-config, looking for system includes")
    find_path(LG_ATOMESE_INCLUDE_PATH
        NAMES opencog/nlp/lg-parse/LGParseLink.h
        PATHS /usr/local/include /usr/include
        PATH_SUFFIXES opencog
    )
    if(LG_ATOMESE_INCLUDE_PATH)
        message(STATUS "Found lg-atomese headers at ${LG_ATOMESE_INCLUDE_PATH}")
        include_directories(${LG_ATOMESE_INCLUDE_PATH})
        set(LG_ATOMESE_FOUND TRUE)
    endif()
endif()

# Find link-grammar
find_path(LINK_GRAMMAR_INCLUDE_DIR
    NAMES link-grammar/link-includes.h
    PATHS /usr/include /usr/local/include
)

find_library(LINK_GRAMMAR_LIBRARY
    NAMES link-grammar linkgrammar
    PATHS /usr/lib /usr/local/lib
)

if(LINK_GRAMMAR_INCLUDE_DIR AND LINK_GRAMMAR_LIBRARY)
    set(LINK_GRAMMAR_FOUND TRUE)
    message(STATUS "Found link-grammar at ${LINK_GRAMMAR_INCLUDE_DIR} and ${LINK_GRAMMAR_LIBRARY}")
    include_directories(${LINK_GRAMMAR_INCLUDE_DIR})
else()
    message(FATAL_ERROR "link-grammar not found. Please install link-grammar development package.")
endif()

# Find opencog main package (optional)
pkg_check_modules(OPENCOG opencog)
if (OPENCOG_FOUND)
    message(STATUS "Found opencog version ${OPENCOG_VERSION}")
    include_directories(${OPENCOG_INCLUDE_DIRS})
    link_directories(${OPENCOG_LIBRARY_DIRS})
else()
    message(STATUS "OpenCog main package not found - basic functionality available")
    set(OPENCOG_FOUND FALSE)
endif()

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    ${CMAKE_CURRENT_SOURCE_DIR}/src
)

# Source files
set(AGENTZERO_COMMUNICATION_SOURCES
    src/LanguageProcessor.cpp
    src/DialogueManager.cpp
    src/AgentComms.cpp
    src/HumanInterface.cpp
)

# Header files
set(AGENTZERO_COMMUNICATION_HEADERS
    include/agentzero/communication/LanguageProcessor.h
    include/agentzero/communication/DialogueManager.h
    include/agentzero/communication/AgentComms.h
    include/agentzero/communication/HumanInterface.h
)

# Create library
add_library(agentzero-communication SHARED
    ${AGENTZERO_COMMUNICATION_SOURCES}
    ${AGENTZERO_COMMUNICATION_HEADERS}
)

target_link_libraries(agentzero-communication
    ${COGUTIL_LIBRARIES}
    ${ATOMSPACE_LIBRARIES}
    ${LINK_GRAMMAR_LIBRARY}
)

# Find additional required libraries
find_library(ATOMBASE_LIBRARY
    NAMES atombase
    PATHS /usr/local/lib/opencog /usr/local/lib /usr/lib
)

find_library(ATOMCORE_LIBRARY
    NAMES atomcore
    PATHS /usr/local/lib/opencog /usr/local/lib /usr/lib
)

find_library(VALUE_LIBRARY
    NAMES value
    PATHS /usr/local/lib/opencog /usr/local/lib /usr/lib
)

if(ATOMBASE_LIBRARY)
    target_link_libraries(agentzero-communication ${ATOMBASE_LIBRARY})
    message(STATUS "Linking atombase library: ${ATOMBASE_LIBRARY}")
endif()

if(ATOMCORE_LIBRARY)
    target_link_libraries(agentzero-communication ${ATOMCORE_LIBRARY})
    message(STATUS "Linking atomcore library: ${ATOMCORE_LIBRARY}")
endif()

if(VALUE_LIBRARY)
    target_link_libraries(agentzero-communication ${VALUE_LIBRARY})
    message(STATUS "Linking value library: ${VALUE_LIBRARY}")
endif()

# Link lg-atomese libraries if available
if(LG_ATOMESE_FOUND AND LG_ATOMESE_LIBRARIES)
    target_link_libraries(agentzero-communication ${LG_ATOMESE_LIBRARIES})
    target_compile_definitions(agentzero-communication PRIVATE HAVE_LG_ATOMESE=1)
    message(STATUS "Linking lg-atomese libraries: ${LG_ATOMESE_LIBRARIES}")
endif()

# Link opencog libraries if available
if(OPENCOG_FOUND AND OPENCOG_LIBRARIES)
    target_link_libraries(agentzero-communication ${OPENCOG_LIBRARIES})
    target_compile_definitions(agentzero-communication PRIVATE HAVE_OPENCOG=1)
    message(STATUS "Linking opencog libraries: ${OPENCOG_LIBRARIES}")
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
message(STATUS "Dependencies found: cogutil=${COGUTIL_FOUND}, atomspace=${ATOMSPACE_FOUND}, link-grammar=${LINK_GRAMMAR_FOUND}, lg-atomese=${LG_ATOMESE_FOUND}")