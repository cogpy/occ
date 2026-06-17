# OpenCog Collection (OCC) CI/CD Documentation

> **Last Updated**: June 2026  
> **Status**: Active  
> **Maintainer**: OCC Team

This document is the single source of truth for the CI/CD architecture in the OpenCog Collection monorepo.

## Table of Contents
1. [Active Workflows](#active-workflows)
2. [Build Targets](#build-targets)
3. [Component Dependency Order](#component-dependency-order)
4. [Workflow Inventory](#workflow-inventory)
5. [Adding New Components](#adding-new-components)
6. [Troubleshooting](#troubleshooting)

---

## Active Workflows

### Core Build Workflows

| Workflow | Purpose | Trigger | Required |
|----------|---------|---------|----------|
| `ci-linux.yml` | Linux CMake build (primary gate) | Push/PR to main | ✅ Yes |
| `ci-windows.yml` | Windows vcpkg build | Push/PR to main | ✅ Yes |
| `ci-python-tests.yml` | Python tests + linting | Push/PR to main | ✅ Yes |
| `ci-guix.yml` | Guix reproducible build | Push/PR to main | ⚠️ Syntax only |
| `ci-docker.yml` | Docker image build | Push/PR to main | ❌ Optional |
| `ci-integration.yml` | E2E integration tests | Push/PR to main | ⚠️ Soft gate |

### Packaging Workflows

| Workflow | Purpose | Trigger |
|----------|---------|---------|
| `vcpkg-prebuild.yml` | Prebuilt vcpkg packages | Manual/Weekly |
| `vendor-builds.yml` | Smart incremental vendoring | Manual |
| `vendor-dependencies.yml` | Vendor vcpkg deps | Manual |
| `heavy-deps-build.yml` | Heavy deps (gRPC, RocksDB) | Manual/Weekly |
| `electron-app-build.yml` | Electron desktop app | After Windows build |
| `chocolatey-package.yml` | Chocolatey package | After Windows build |
| `winget.yml` | Windows Package Manager | On release |

### Support Workflows

| Workflow | Purpose | Trigger |
|----------|---------|---------|
| `auto-sync-runner.yml` | Repository sync | After Windows build |
| `ci.yml` | Orchestrator (calls others) | Push/PR to main |

---

## Build Targets

The OCC supports multiple build targets:

### 1. Linux Native (CMake)
```bash
mkdir build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
make -j$(nproc)
```

### 2. GNU Guix (Reproducible)
```bash
guix build -f guix.scm
```

### 3. Windows (vcpkg)
```powershell
cmake .. -G "Visual Studio 17 2022" -A x64 \
  -DCMAKE_TOOLCHAIN_FILE="$env:VCPKG_ROOT/scripts/buildsystems/vcpkg.cmake"
cmake --build . --config Release
```

### 4. Docker
```bash
docker build -t occ .
docker-compose up
```

### 5. Debian Packages
```bash
cd packaging && ./build-debian.sh
```

---

## Component Dependency Order

Components **must** be built in this order due to inter-dependencies:

```
Layer 0: Build Tools
└── MIG (Mach Interface Generator)

Layer 1: Foundation
└── cogutil (no dependencies)

Layer 2: Core
├── atomspace (requires cogutil)
└── coggml (requires cogutil)

Layer 3: Storage
├── atomspace-storage (requires atomspace)
├── atomspace-rocks (requires atomspace-storage)
├── atomspace-cog (requires atomspace-storage)
└── atomspace-pgres (requires atomspace-storage)

Layer 4: Networking
├── cogserver (requires atomspace, atomspace-storage)
└── matrix (requires atomspace)

Layer 5: Reasoning
├── unify (requires atomspace)
├── ure (requires unify)
├── pln (requires ure)
└── miner (requires ure)

Layer 6: Learning & Attention
├── learn (requires atomspace)
├── attention (requires atomspace)
├── agents (requires atomspace)
└── sensory (requires atomspace)

Layer 7: Evolution
└── asmoses (requires cogutil)

Layer 8: Cognitive Architecture
├── cogself (requires coggml)
├── atomspace-accelerator (independent)
├── agentic-chatbots (independent)
└── tensor-logic (requires aten, atenspace)

Layer 9: AGI-OS (optional)
├── cognumach (Layer 1 - microkernel)
└── hurdcog (Layer 2 - requires cognumach)
```

### CI Matrix Definition

```yaml
strategy:
  matrix:
    layer:
      - { name: "foundation", components: "cogutil" }
      - { name: "core", components: "atomspace,coggml" }
      - { name: "storage", components: "atomspace-storage,atomspace-rocks" }
      - { name: "network", components: "cogserver,matrix" }
      - { name: "reasoning", components: "unify,ure,pln,miner" }
      - { name: "learning", components: "learn,attention,agents" }
      - { name: "evolution", components: "asmoses" }
      - { name: "cognitive", components: "cogself,tensor-logic" }
```

---

## Workflow Inventory

### Active Workflows (`.yml`)

| File | Status | Decision |
|------|--------|----------|
| `ci-linux.yml` | ✅ Active | Primary Linux build |
| `ci-windows.yml` | ✅ Active | Primary Windows build |
| `ci-python-tests.yml` | ✅ Active | Python tests & linting |
| `ci-guix.yml` | ✅ Active | Guix syntax + build |
| `ci-docker.yml` | ✅ Active | Docker build |
| `ci-integration.yml` | ✅ Active | E2E tests |
| `ci.yml` | ✅ Active | Orchestrator |
| `vcpkg-prebuild.yml` | ✅ Active | vcpkg caching |
| `vendor-builds.yml` | ✅ Active | Vendoring |
| `vendor-dependencies.yml` | ✅ Active | Vendoring |
| `heavy-deps-build.yml` | ✅ Active | Heavy deps |
| `electron-app-build.yml` | ✅ Active | Electron app |
| `chocolatey-package.yml` | ✅ Active | Chocolatey |
| `winget.yml` | ✅ Active | WinGet |
| `auto-sync-runner.yml` | ✅ Active | Sync |

### Archived Workflows (`.disabled`)

These workflows were disabled during the Windows-first focus period and have been:
- **Merged**: Functionality consolidated into new workflows
- **Archived**: Kept for reference but not needed

| File | Decision | Reason |
|------|----------|--------|
| `agi-os-debian-build.yml.disabled` | Merged → `ci-debian.yml` | Consolidated |
| `agi-os-guix-build.yml.disabled` | Merged → `ci-guix.yml` | Consolidated |
| `agi-os-integration-test.yml.disabled` | Merged → `ci-integration.yml` | Consolidated |
| `occ-win-build.yml.disabled` | Merged → `ci-windows.yml` | Consolidated |
| `occ-win-build-fast.yml.disabled` | Merged → `ci-windows.yml` | Consolidated |
| `wincogpre.yml.disabled` | Merged → `ci-windows.yml` | Consolidated |
| `python-lint.yml.disabled` | Merged → `ci-python-tests.yml` | Consolidated |
| `python-type-check.yml.disabled` | Merged → `ci-python-tests.yml` | Consolidated |
| `docker.yml.disabled` | Merged → `ci-docker.yml` | Consolidated |
| `cogci.yml.disabled` | Archive | Superseded by ci-linux.yml |
| `occ-evo.yml.disabled` | Archive | Experimental, not maintained |
| `ocog9.yml.disabled` | Archive | Legacy, not maintained |

---

## Adding New Components

### 1. Update CMakeLists.txt
```cmake
OPTION(BUILD_NEWCOMPONENT "Build NewComponent" ON)

IF(BUILD_NEWCOMPONENT AND BUILD_ATOMSPACE)
    MESSAGE(STATUS "Building NewComponent...")
    add_subdirectory(newcomponent)
ENDIF()
```

### 2. Add to CI Matrix
Edit `ci-linux.yml`:
```yaml
matrix:
  layer:
    - { name: "cognitive", components: "cogself,newcomponent" }
```

### 3. Add Tests
Create `tests/newcomponent/test_*.py` and update `ci-python-tests.yml`.

---

## Troubleshooting

### Windows Build Fails
1. Check vcpkg cache: Clear with `rmdir /s /q vcpkg_cache`
2. Verify `VCPKG_ROOT` environment variable
3. Check prebuilt packages from `vcpkg-prebuild.yml`

### Linux Build Fails
1. Check dependency order in build logs
2. Verify system dependencies: `sudo apt-get install libboost-all-dev guile-3.0-dev`
3. Review component's `CMakeLists.txt`

### Guix Build Fails
1. Expected for cross-compilation targets
2. Syntax validation should always pass
3. Run `./test-guix-syntax.sh` locally

### Integration Tests Fail
1. Check specific test suite logs
2. Some tests may require built binaries
3. Verify Python dependencies: `pip install -r requirements.txt`

---

## Workflow Dependencies

```
                    ┌──────────────────────────────────────────┐
                    │              ci.yml (orchestrator)        │
                    └──────────────────────────────────────────┘
                                        │
        ┌───────────────────────────────┼───────────────────────────────┐
        │                               │                               │
        ▼                               ▼                               ▼
┌───────────────┐              ┌───────────────┐              ┌───────────────┐
│  ci-linux.yml │              │ ci-windows.yml│              │  ci-guix.yml  │
│  (required)   │              │  (required)   │              │  (optional)   │
└───────────────┘              └───────────────┘              └───────────────┘
        │                               │
        ▼                               ▼
┌───────────────┐              ┌───────────────┐
│ci-python-tests│              │electron-app   │
│  (required)   │              │chocolatey     │
└───────────────┘              │winget         │
        │                      └───────────────┘
        ▼
┌───────────────┐
│ci-integration │
│ (soft gate)   │
└───────────────┘
```

---

## Status Badges

```markdown
[![Linux Build](https://github.com/cogpy/occ/actions/workflows/ci-linux.yml/badge.svg)](https://github.com/cogpy/occ/actions/workflows/ci-linux.yml)
[![Windows Build](https://github.com/cogpy/occ/actions/workflows/ci-windows.yml/badge.svg)](https://github.com/cogpy/occ/actions/workflows/ci-windows.yml)
[![Python Tests](https://github.com/cogpy/occ/actions/workflows/ci-python-tests.yml/badge.svg)](https://github.com/cogpy/occ/actions/workflows/ci-python-tests.yml)
[![Guix Build](https://github.com/cogpy/occ/actions/workflows/ci-guix.yml/badge.svg)](https://github.com/cogpy/occ/actions/workflows/ci-guix.yml)
```

---

## References

- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [OCC Architecture](./architecture.md)
- [Build Sequences](./BUILD_SEQUENCES.md)
- [Contributing Guide](../CONTRIBUTING.md)
