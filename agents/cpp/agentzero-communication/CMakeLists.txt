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
pkg_check_modules(ATOMSPACE REQUIRED atomspace)
if (ATOMSPACE_FOUND)
    message(STATUS "Found atomspace version ${ATOMSPACE_VERSION}")
    include_directories(${ATOMSPACE_INCLUDE_DIRS})
    link_directories(${ATOMSPACE_LIBRARY_DIRS})
endif()

# Find lg-atomese for language processing
pkg_check_modules(LG_ATOMESE lg-atomese)
if (LG_ATOMESE_FOUND)
    message(STATUS "Found lg-atomese version ${LG_ATOMESE_VERSION}")
    include_directories(${LG_ATOMESE_INCLUDE_DIRS})
    link_directories(${LG_ATOMESE_LIBRARY_DIRS})
else()
    message(STATUS "lg-atomese not found via pkg-config, looking for system includes")
    find_path(LG_ATOMESE_INCLUDE_PATH
        NAMES opencog/lg-atomese/LGAtomese.h
        PATHS /usr/local/include /usr/include
        PATH_SUFFIXES opencog
    )
    if(LG_ATOMESE_INCLUDE_PATH)
        message(STATUS "Found lg-atomese headers at ${LG_ATOMESE_INCLUDE_PATH}")
        include_directories(${LG_ATOMESE_INCLUDE_PATH})
        set(LG_ATOMESE_FOUND TRUE)
    endif()
endif()

# Find link-grammar for parsing
pkg_check_modules(LINK_GRAMMAR link-grammar)
if (LINK_GRAMMAR_FOUND)
    message(STATUS "Found link-grammar version ${LINK_GRAMMAR_VERSION}")
    include_directories(${LINK_GRAMMAR_INCLUDE_DIRS})
    link_directories(${LINK_GRAMMAR_LIBRARY_DIRS})
else()
    message(STATUS "link-grammar not found - some NLP features will be limited")
    set(LINK_GRAMMAR_FOUND FALSE)
endif()

# Find opencog main module
pkg_check_modules(OPENCOG opencog)
if (OPENCOG_FOUND)
    message(STATUS "Found opencog version ${OPENCOG_VERSION}")
    include_directories(${OPENCOG_INCLUDE_DIRS})
    link_directories(${OPENCOG_LIBRARY_DIRS})
else()
    message(STATUS "OpenCog main module not found - using core AtomSpace functionality only")
    set(OPENCOG_FOUND FALSE)
endif()

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    ${CMAKE_CURRENT_SOURCE_DIR}/src
)

# Source files
set(AGENTZERO_COMMUNICATION_SOURCES
    src/DialogueManager.cpp
    src/ConversationState.cpp
    src/LanguageProcessor.cpp
    src/MessageHandler.cpp
)

# Header files
set(AGENTZERO_COMMUNICATION_HEADERS
    include/opencog/agentzero/DialogueManager.h
    include/opencog/agentzero/ConversationState.h
    include/opencog/agentzero/LanguageProcessor.h
    include/opencog/agentzero/MessageHandler.h
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

# Link lg-atomese libraries if available
if(LG_ATOMESE_FOUND AND LG_ATOMESE_LIBRARIES)
    target_link_libraries(agentzero-communication ${LG_ATOMESE_LIBRARIES})
    target_compile_definitions(agentzero-communication PRIVATE HAVE_LG_ATOMESE=1)
    message(STATUS "Linking lg-atomese libraries: ${LG_ATOMESE_LIBRARIES}")
elseif(LG_ATOMESE_FOUND)
    target_compile_definitions(agentzero-communication PRIVATE HAVE_LG_ATOMESE=1)
    message(STATUS "lg-atomese headers found, compiling with lg-atomese support")
endif()

# Link link-grammar libraries if available
if(LINK_GRAMMAR_FOUND AND LINK_GRAMMAR_LIBRARIES)
    target_link_libraries(agentzero-communication ${LINK_GRAMMAR_LIBRARIES})
    target_compile_definitions(agentzero-communication PRIVATE HAVE_LINK_GRAMMAR=1)
    message(STATUS "Linking link-grammar libraries: ${LINK_GRAMMAR_LIBRARIES}")
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
add_subdirectory(examples)

# Status message
message(STATUS "Agent-Zero Communication module configuration complete")
message(STATUS "Available features:")
if(LG_ATOMESE_FOUND)
    message(STATUS "  - lg-atomese integration: YES")
else()
    message(STATUS "  - lg-atomese integration: NO")
endif()
if(LINK_GRAMMAR_FOUND)
    message(STATUS "  - Link Grammar parsing: YES")
else()
    message(STATUS "  - Link Grammar parsing: NO")
endif()
if(OPENCOG_FOUND)
    message(STATUS "  - OpenCog NLP integration: YES")
else()
    message(STATUS "  - OpenCog NLP integration: NO")
endif()