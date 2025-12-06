# Examples CMakeLists.txt

cmake_minimum_required(VERSION 3.16)

# Install example scripts
install(FILES
    basic_example.py
    knowledge_example.py
    task_example.py
    DESTINATION share/opencog/agentzero/examples
    PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE
                GROUP_READ GROUP_EXECUTE
                WORLD_READ WORLD_EXECUTE
)

message(STATUS "Agent-Zero Python Bridge examples configured")
