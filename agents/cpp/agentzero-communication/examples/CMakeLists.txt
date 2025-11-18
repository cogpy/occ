# Examples for Agent-Zero Communication Module
# Part of AZ-HUMAN-001: Create HumanInterface layer

# Basic HumanInterface demonstration
add_executable(human_interface_demo
    HumanInterfaceDemo.cpp
)

target_link_libraries(human_interface_demo
    agentzero-communication
    ${COGUTIL_LIBRARIES}
    ${ATOMSPACE_LIBRARIES}
    ${Boost_LIBRARIES}
    pthread
)

target_include_directories(human_interface_demo PRIVATE
    ${CMAKE_SOURCE_DIR}/include

cmake_minimum_required(VERSION 3.16)

# Find required packages
find_package(PkgConfig REQUIRED)

# Find cogutil and atomspace
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

# Interactive conversation example
add_executable(conversation_example
    ConversationExample.cpp
)

target_link_libraries(conversation_example
    agentzero-communication
    ${COGUTIL_LIBRARIES}
    ${ATOMSPACE_LIBRARIES}
    ${Boost_LIBRARIES}
    pthread
)

target_include_directories(conversation_example PRIVATE
    ${CMAKE_SOURCE_DIR}/include
    ${COGUTIL_INCLUDE_DIRS}
    ${ATOMSPACE_INCLUDE_DIRS}
)

# Install examples
install(TARGETS human_interface_demo conversation_example
    DESTINATION ${CMAKE_INSTALL_BINDIR}/examples
)
# Link directories
link_directories(
    ${COGUTIL_LIBRARY_DIRS}
    ${ATOMSPACE_LIBRARY_DIRS}
)

# Example: Basic LanguageProcessor usage
add_executable(language_processor_example
    language_processor_example.cpp
)

target_link_libraries(language_processor_example
    agentzero-communication
    ${COGUTIL_LIBRARIES}
    ${ATOMSPACE_LIBRARIES}
)

# Add OpenCog libraries that might be needed
find_library(ATOMBASE_LIBRARY NAMES atombase PATHS /usr/local/lib/opencog)
find_library(ATOMCORE_LIBRARY NAMES atomcore PATHS /usr/local/lib/opencog)
find_library(VALUE_LIBRARY NAMES value PATHS /usr/local/lib/opencog)

# Link additional OpenCog libraries to both examples
if(ATOMBASE_LIBRARY)
    target_link_libraries(language_processor_example ${ATOMBASE_LIBRARY})
endif()
if(ATOMCORE_LIBRARY)
    target_link_libraries(language_processor_example ${ATOMCORE_LIBRARY})
endif()
if(VALUE_LIBRARY)
    target_link_libraries(language_processor_example ${VALUE_LIBRARY})
endif()

# Example: Batch processing
add_executable(batch_processing_example
    batch_processing_example.cpp
)

target_link_libraries(batch_processing_example
    agentzero-communication
    ${COGUTIL_LIBRARIES}
    ${ATOMSPACE_LIBRARIES}
)

# Link libraries to batch processing example too
if(ATOMBASE_LIBRARY)
    target_link_libraries(batch_processing_example ${ATOMBASE_LIBRARY})
endif()
if(ATOMCORE_LIBRARY)
    target_link_libraries(batch_processing_example ${ATOMCORE_LIBRARY})
endif()
if(VALUE_LIBRARY)
    target_link_libraries(batch_processing_example ${VALUE_LIBRARY})
endif()
