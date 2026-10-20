# OpenCog Collection (OCC) CI/CD Workflows

> **Last Updated**: June 2026  
> **Version**: 2.0  
> **Status**: Active ✅

This directory contains the CI/CD workflows for the OpenCog Collection monorepo.

## Quick Reference

| Workflow | Purpose | Trigger | Required |
|----------|---------|---------|----------|
| `ci.yml` | Orchestrator | Push/PR | ✅ |
| `ci-linux.yml` | Linux CMake build | Push/PR | ✅ |
| `ci-windows.yml` | Windows vcpkg build | Push/PR | ✅ |
| `ci-python-tests.yml` | Python tests & linting | Push/PR | ✅ |
| `ci-guix.yml` | Guix syntax + build | Push/PR | ⚠️ Syntax only |
| `ci-docker.yml` | Docker build | Push/PR | ❌ Optional |
| `ci-integration.yml` | E2E integration tests | Push/PR + Daily | ⚠️ Soft gate |

## Status Badges

```markdown
[![CI](https://github.com/cogpy/occ/actions/workflows/ci.yml/badge.svg)](https://github.com/cogpy/occ/actions/workflows/ci.yml)
[![Linux Build](https://github.com/cogpy/occ/actions/workflows/ci-linux.yml/badge.svg)](https://github.com/cogpy/occ/actions/workflows/ci-linux.yml)
[![Windows Build](https://github.com/cogpy/occ/actions/workflows/ci-windows.yml/badge.svg)](https://github.com/cogpy/occ/actions/workflows/ci-windows.yml)
[![Python Tests](https://github.com/cogpy/occ/actions/workflows/ci-python-tests.yml/badge.svg)](https://github.com/cogpy/occ/actions/workflows/ci-python-tests.yml)
```

---

## Active Workflows

### Core Build Workflows

#### `ci.yml` - Orchestrator
The top-level workflow that coordinates all CI checks based on changed files.

```bash
# Run all checks
gh workflow run ci.yml -f run_all=true
```

#### `ci-linux.yml` - Linux Build
Native Ubuntu CMake build of the OCC stack in dependency order.

**Build Stages:**
1. Foundation (cogutil)
2. Core (atomspace, coggml)
3. Network (cogserver, matrix)
4. Reasoning (unify, ure)

```bash
gh workflow run ci-linux.yml -f build_type=Debug
```

#### `ci-windows.yml` - Windows Build
Windows build with mode selection (fast/full).

**Modes:**
- `fast`: Uses vendored prebuilt dependencies (5-10 min)
- `full`: Uses vcpkg with caching (30-120 min first build)

```bash
gh workflow run ci-windows.yml -f mode=full
```

#### `ci-python-tests.yml` - Python Tests
Python test suites, linting, and type checking.

**Checks:**
- Syntax validation (required)
- Ruff/Flake8 linting
- Black formatting
- Synergy tests (`tests/synergy/*.py`)
- Type checking with mypy

#### `ci-guix.yml` - Guix Build
GNU Guix reproducible build validation.

**Note:** Syntax validation is required; full builds are non-blocking due to CI limitations.

#### `ci-docker.yml` - Docker Build
Dockerfile and docker-compose.yml validation and build.

#### `ci-integration.yml` - Integration Tests
End-to-end integration and cognitive synergy tests.

**Test Suites:**
1. Repository structure validation
2. Build system integration
3. Cognitive synergy scripts
4. Documentation validation
5. Dependency order validation

---

### Packaging Workflows

| Workflow | Purpose |
|----------|---------|
| `vcpkg-prebuild.yml` | Prebuilt vcpkg packages |
| `vendor-builds.yml` | Smart incremental vendoring |
| `vendor-dependencies.yml` | Vendor vcpkg deps |
| `heavy-deps-build.yml` | Heavy deps (gRPC, RocksDB) |
| `electron-app-build.yml` | Electron desktop app |
| `chocolatey-package.yml` | Chocolatey package |
| `winget.yml` | Windows Package Manager |

---

## Workflow Architecture

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

## Archived Workflows

Files with `.disabled` or `.temp_disabled` extensions are archived workflows that were disabled during the Windows-first development phase. Their functionality has been consolidated into the new `ci-*.yml` workflows.

See [docs/CI.md](../../docs/CI.md) for the full inventory and migration decisions.

---

## Common Operations

### Trigger Manual Builds

```bash
# Full CI run
gh workflow run ci.yml -f run_all=true

# Specific workflow
gh workflow run ci-linux.yml
gh workflow run ci-windows.yml -f mode=fast
```

### Download Artifacts

```bash
# List recent runs
gh run list --workflow=ci-linux.yml

# Download artifacts
gh run download RUN_ID --name cogutil-build
```

### Check Status

```bash
# View workflow runs
gh run list

# View specific run
gh run view RUN_ID
```

---

## Troubleshooting

### Linux Build Fails
1. Check dependency order in build logs
2. Verify system dependencies installed
3. Review component's CMakeLists.txt

### Windows Build Fails
1. Check vcpkg cache status
2. Verify prebuilt packages available
3. Try `mode=full` to rebuild dependencies

### Python Tests Fail
1. Check syntax validation first
2. Review test output for specific failures
3. Verify requirements.txt is up to date

### Guix Build Fails
1. Expected for cross-compilation targets
2. Syntax validation should always pass
3. Run `./test-guix-syntax.sh` locally

---

## Documentation

- **Full CI Documentation**: [docs/CI.md](../../docs/CI.md)
- **Build Sequences**: [docs/BUILD_SEQUENCES.md](../../docs/BUILD_SEQUENCES.md)
- **Architecture**: [docs/architecture.md](../../docs/architecture.md)

---

## References

- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [vcpkg Documentation](https://vcpkg.io/en/docs/)
- [GNU Guix Manual](https://guix.gnu.org/manual/)
