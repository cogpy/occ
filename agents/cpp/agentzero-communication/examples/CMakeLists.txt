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