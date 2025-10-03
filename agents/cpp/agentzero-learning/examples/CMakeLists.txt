# Examples for Agent-Zero Learning Module

cmake_minimum_required(VERSION 3.16)

# Create experience manager demo
add_executable(experience_manager_demo
    ExperienceManagerDemo.cpp
)

target_link_libraries(experience_manager_demo
    agentzero-learning
    ${COGUTIL_LIBRARIES}
    ${ATOMSPACE_LIBRARIES}
    ${Boost_LIBRARIES}
)

# Set output directory
set_target_properties(experience_manager_demo PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

message(STATUS "ExperienceManager examples configured")