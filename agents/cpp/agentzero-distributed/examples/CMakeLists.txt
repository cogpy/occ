# Examples CMakeLists.txt for Agent-Zero Distributed Computing

# Example executables
add_executable(DistributedComputingExample
    DistributedComputingExample.cpp
)

target_link_libraries(DistributedComputingExample
    agentzero-distributed
    ${COGUTIL_LIBRARIES}
    ${ATOMSPACE_LIBRARIES}
    ${Boost_LIBRARIES}
)

install(TARGETS DistributedComputingExample
    RUNTIME DESTINATION bin/examples
)

message(STATUS "Distributed computing examples configured")
