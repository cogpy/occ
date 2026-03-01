# OpenCog Inferno AGI OS Architecture

## Overview

OpenCog Inferno is a pure kernel-based distributed AGI operating system. It implements artificial general intelligence as a fundamental operating system service, following the Inferno/Plan 9 philosophy where everything is a file and all resources are accessible through the 9P protocol.

## Design Principles

1. **Intelligence as a Kernel Service**: Cognition is not an application; it is a core OS function.
2. **Everything is a File**: All cognitive structures (atoms, links, truth values, attention values) are exposed as files in the cognitive namespace.
3. **Distributed by Default**: Knowledge, reasoning, and attention are natively distributed across nodes using 9P/Styx.
4. **Zero-Copy Cognitive Operations**: Kernel-level integration eliminates user-kernel boundary crossing overhead.
5. **Economic Attention**: Limited computational resources are allocated using economic principles (ECAN).

## Architecture Layers

```
┌─────────────────────────────────────────────────────────────┐
│                 Cognitive Applications (Limbo)               │
│  cognitive_demo.b  hello_cognition.b  simple_inference.b     │
├─────────────────────────────────────────────────────────────┤
│              Limbo Cognitive Libraries                        │
│  atomspace.m  pln.m  attention.m  cognet.m                   │
├─────────────────────────────────────────────────────────────┤
│              Cognitive Filesystems (9P)                       │
│  /atoms/  /reasoning/  /attention/  /learning/               │
│  /perception/  /action/  /net/cognitive/                      │
├─────────────────────────────────────────────────────────────┤
│              Kernel Cognitive Modules (C)                     │
│  atomspace_portable.c  reasoning_portable.c                  │
│  attention_portable.c  cognitive9p_portable.c                │
├─────────────────────────────────────────────────────────────┤
│              Userspace Client Libraries (C)                   │
│  cogfs_client.c  atomspace_client.c  reasoning_client.c      │
├─────────────────────────────────────────────────────────────┤
│              Dis Virtual Machine / Native Platform            │
├─────────────────────────────────────────────────────────────┤
│                       Hardware                                │
└─────────────────────────────────────────────────────────────┘
```

## Component Details

### Kernel Modules (`kernel/`)

The kernel modules implement cognitive services at the lowest level:

| Module | File | Description |
|--------|------|-------------|
| AtomSpace | `atomspace_portable.c` | Hypergraph database with atoms, links, truth values |
| Reasoning | `reasoning_portable.c` | PLN inference engine with deduction, induction, abduction |
| Attention | `attention_portable.c` | ECAN economic attention allocation |
| Cognitive 9P | `cognitive9p_portable.c` | 9P protocol server for cognitive namespace |

### Cognitive Filesystems (`fs/`)

Each cognitive service is exposed as a filesystem:

| Filesystem | Mount Point | Purpose |
|------------|-------------|---------|
| atomfs | `/atoms/` | AtomSpace CRUD operations |
| reasonfs | `/reasoning/` | PLN inference and rule management |
| attnfs | `/attention/` | ECAN stimulation and focus queries |
| learnfs | `/learning/` | MOSES evolution and URE rules |
| perceptfs | `/perception/` | Sensory input channels |
| actionfs | `/action/` | Motor output commands |

### Limbo Modules (`limbo/`)

High-level Limbo language bindings for cognitive services:

- `atomspace/atomspace.m` - AtomSpace ADT and operations
- `pln/pln.m` - PLN inference rules and chaining
- `attention/attention.m` - ECAN attention bank management
- `distributed/cognet.m` - Distributed cognitive networking

### Userspace Libraries (`userspace/`)

C client libraries for accessing cognitive services from userspace:

- `cogfs_client.c` - Low-level 9P filesystem client
- `atomspace_client.c` - High-level AtomSpace API
- `reasoning_client.c` - High-level reasoning API

## Cognitive Filesystem Protocol

All cognitive operations follow the read/write file paradigm:

### Creating an Atom
```
echo "ConceptNode Socrates 0.9 0.8" > /atoms/new
cat /atoms/new  # Returns atom ID
```

### Querying Atoms
```
echo "ConceptNode" > /atoms/query
cat /atoms/query  # Returns matching atom IDs
```

### Performing Inference
```
echo "42 43" > /reasoning/pln  # Premise atom IDs
cat /reasoning/results/latest  # Read conclusion
```

### Stimulating Attention
```
echo "42 100" > /attention/stimulate  # Atom 42, +100 STI
cat /attention/focus  # Read atoms in attentional focus
```

## Distributed Architecture

The distributed cognitive network uses 9P/Styx for inter-node communication:

```
   Node A                    Node B                    Node C
┌──────────┐            ┌──────────┐            ┌──────────┐
│ AtomSpace│◄──9P/Styx─►│ AtomSpace│◄──9P/Styx─►│ AtomSpace│
│ PLN      │            │ PLN      │            │ PLN      │
│ ECAN     │            │ ECAN     │            │ ECAN     │
└──────────┘            └──────────┘            └──────────┘
     │                       │                       │
     └───────────────────────┴───────────────────────┘
                    Cognitive Namespace
                /net/cognitive/<host>/atoms/
```

Each node:
1. Runs its own cognitive kernel
2. Exports its AtomSpace via 9P
3. Can mount remote AtomSpaces
4. Synchronizes knowledge automatically

## Build System

The project uses CMake for cross-platform builds:

```bash
mkdir build && cd build
cmake .. -DBUILD_KERNEL_MODULES=ON \
         -DBUILD_FILESYSTEMS=ON \
         -DBUILD_USERSPACE=ON \
         -DBUILD_TESTS=ON
make
ctest
```

## Testing

Three test suites validate the kernel modules:

1. `test_atomspace` - AtomSpace CRUD, truth values, queries
2. `test_reasoning` - PLN rules, forward/backward chaining
3. `test_cognitive9p` - 9P filesystem interface, atom creation via files
