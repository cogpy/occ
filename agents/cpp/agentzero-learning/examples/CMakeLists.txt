# Agent-Zero Learning Module Examples
# Part of AZ-LEARN-003: MOSES Policy Optimization Integration

cmake_minimum_required(VERSION 3.16)

# Simple policy optimization example
add_executable(policy_optimization_example
    policy_optimization_example.cpp
)

target_link_libraries(policy_optimization_example
    agentzero-learning
    ${COGUTIL_LIBRARIES}
    ${ATOMSPACE_LIBRARIES}
    ${MOSES_LIBRARIES}
    ${ASMOSES_LIBRARIES}
)

# Experience management example
add_executable(experience_management_example
    experience_management_example.cpp
)

target_link_libraries(experience_management_example
    agentzero-learning
    ${COGUTIL_LIBRARIES}
    ${ATOMSPACE_LIBRARIES}
)

message(STATUS "Agent-Zero Learning examples configured")