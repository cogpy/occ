# Tests for Agent-Zero Communication Module
# Part of AZ-HUMAN-001: Create HumanInterface layer

find_package(Threads REQUIRED)

# Simple unit tests
add_executable(test_human_interface
    test_HumanInterface.cpp
)

target_link_libraries(test_human_interface
    agentzero-communication
    ${COGUTIL_LIBRARIES}
    ${ATOMSPACE_LIBRARIES}
    ${Boost_LIBRARIES}
    Threads::Threads
)

target_include_directories(test_human_interface PRIVATE
    ${CMAKE_SOURCE_DIR}/include
    ${COGUTIL_INCLUDE_DIRS}
    ${ATOMSPACE_INCLUDE_DIRS}
)

# Add test
add_test(NAME HumanInterfaceBasicTest COMMAND test_human_interface)

# Test utilities
add_executable(test_communication_utils
    test_CommunicationUtils.cpp
)

target_link_libraries(test_communication_utils
    agentzero-communication
    ${COGUTIL_LIBRARIES}
    ${ATOMSPACE_LIBRARIES}
    ${Boost_LIBRARIES}
    Threads::Threads
)

target_include_directories(test_communication_utils PRIVATE
    ${CMAKE_SOURCE_DIR}/include
    ${COGUTIL_INCLUDE_DIRS}
    ${ATOMSPACE_INCLUDE_DIRS}
)

add_test(NAME CommunicationUtilsTest COMMAND test_communication_utils)

cmake_minimum_required(VERSION 3.16)

# Find required packages for testing
find_package(PkgConfig REQUIRED)

# Find cogutil and atomspace (required for tests)
pkg_check_modules(COGUTIL REQUIRED cogutil)
pkg_check_modules(ATOMSPACE atomspace)
if (NOT ATOMSPACE_FOUND)
    # Find atomspace manually
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
        set(ATOMSPACE_INCLUDE_DIRS ${ATOMSPACE_INCLUDE_DIR})
    endif()
endif()

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../include
    ${COGUTIL_INCLUDE_DIRS}
    ${ATOMSPACE_INCLUDE_DIRS}
)

# Link directories
link_directories(
    ${COGUTIL_LIBRARY_DIRS}
    ${ATOMSPACE_LIBRARY_DIRS}
)

# Test for LanguageProcessor
add_executable(test_language_processor
    test_language_processor.cpp
)

target_link_libraries(test_language_processor
    agentzero-communication
    ${COGUTIL_LIBRARIES}
    ${ATOMSPACE_LIBRARIES}
)

# Add OpenCog libraries that might be needed
find_library(ATOMBASE_LIBRARY NAMES atombase PATHS /usr/local/lib/opencog)
find_library(ATOMCORE_LIBRARY NAMES atomcore PATHS /usr/local/lib/opencog)
find_library(VALUE_LIBRARY NAMES value PATHS /usr/local/lib/opencog)

if(ATOMBASE_LIBRARY)
    target_link_libraries(test_language_processor ${ATOMBASE_LIBRARY})
endif()
if(ATOMCORE_LIBRARY)
    target_link_libraries(test_language_processor ${ATOMCORE_LIBRARY})
endif()
if(VALUE_LIBRARY)
    target_link_libraries(test_language_processor ${VALUE_LIBRARY})
endif()

# Add test
add_test(NAME LanguageProcessorTest COMMAND test_language_processor)

# Test for basic communication functionality
add_executable(test_communication_basic
    test_communication_basic.cpp
)

target_link_libraries(test_communication_basic
    agentzero-communication
    ${COGUTIL_LIBRARIES}
    ${ATOMSPACE_LIBRARIES}
)

if(ATOMBASE_LIBRARY)
    target_link_libraries(test_communication_basic ${ATOMBASE_LIBRARY})
endif()
if(ATOMCORE_LIBRARY)
    target_link_libraries(test_communication_basic ${ATOMCORE_LIBRARY})
endif()
if(VALUE_LIBRARY)
    target_link_libraries(test_communication_basic ${VALUE_LIBRARY})
endif()

add_test(NAME CommunicationBasicTest COMMAND test_communication_basic)
