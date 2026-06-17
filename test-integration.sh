#!/bin/bash
# Integration Test Suite for AGI-OS
# Works in CI ($GITHUB_WORKSPACE) and locally (git root)

set -euo pipefail

# Determine repository root
if [ -n "${GITHUB_WORKSPACE:-}" ]; then
    REPO_ROOT="$GITHUB_WORKSPACE"
elif git rev-parse --show-toplevel >/dev/null 2>&1; then
    REPO_ROOT="$(git rev-parse --show-toplevel)"
else
    REPO_ROOT="$(cd "$(dirname "$0")" && pwd)"
fi

echo "╔════════════════════════════════════════════════════════════╗"
echo "║  AGI-OS Integration Test Suite                            ║"
echo "╚════════════════════════════════════════════════════════════╝"
echo ""
echo "Repository root: $REPO_ROOT"
echo ""

PASSED=0
FAILED=0
SKIPPED=0

# Helper function for test results
test_file() {
    local description="$1"
    local filepath="$2"
    
    if [ -f "$filepath" ]; then
        echo "  ✓ $description"
        PASSED=$((PASSED + 1))
        return 0
    else
        echo "  ✗ $description (not found: $filepath)"
        FAILED=$((FAILED + 1))
        return 1
    fi
}

test_dir() {
    local description="$1"
    local dirpath="$2"
    
    if [ -d "$dirpath" ]; then
        echo "  ✓ $description"
        PASSED=$((PASSED + 1))
        return 0
    else
        echo "  ✗ $description (not found: $dirpath)"
        FAILED=$((FAILED + 1))
        return 1
    fi
}

# Test 1: Core components exist
echo "Test 1: Checking core OCC components..."
test_dir "cogutil directory" "$REPO_ROOT/cogutil"
test_dir "atomspace directory" "$REPO_ROOT/atomspace"
test_dir "cogserver directory" "$REPO_ROOT/cogserver"
test_file "Root CMakeLists.txt" "$REPO_ROOT/CMakeLists.txt"

# Test 2: Cognumach cognitive headers (optional AGI-OS layer)
echo ""
echo "Test 2: Checking Cognumach cognitive headers (optional)..."
if [ -d "$REPO_ROOT/cognumach" ]; then
    test_file "AtomSpace IPC header" "$REPO_ROOT/cognumach/include/mach/cognitive/atomspace_ipc.h" || true
    test_file "Cognitive VM header" "$REPO_ROOT/cognumach/include/mach/cognitive/cognitive_vm.h" || true
else
    echo "  ⊘ Cognumach not present (optional)"
    SKIPPED=$((SKIPPED + 1))
fi

# Test 3: HurdCog integration layer (optional AGI-OS layer)
echo ""
echo "Test 3: Checking HurdCog integration layer (optional)..."
if [ -d "$REPO_ROOT/hurdcog" ]; then
    test_file "MachSpace bridge" "$REPO_ROOT/hurdcog/cogkernel/mach-integration/machspace-bridge.scm" || true
else
    echo "  ⊘ HurdCog not present (optional)"
    SKIPPED=$((SKIPPED + 1))
fi

# Test 4: OCC integration layer
echo ""
echo "Test 4: Checking OCC integration components..."
test_dir "HurdCog integration" "$REPO_ROOT/hurdcog-integration" || true
if [ -f "$REPO_ROOT/hurdcog-integration/atomspace-hurdcog-bridge.py" ]; then
    test_file "AtomSpace-HurdCog bridge" "$REPO_ROOT/hurdcog-integration/atomspace-hurdcog-bridge.py"
fi

# Test 5: Python bridge functionality
echo ""
echo "Test 5: Testing Python environment..."
if command -v python3 >/dev/null 2>&1; then
    if python3 -c "import sys; print(f'Python {sys.version}')" >/dev/null 2>&1; then
        echo "  ✓ Python3 is functional"
        PASSED=$((PASSED + 1))
    else
        echo "  ✗ Python3 failed basic test"
        FAILED=$((FAILED + 1))
    fi
    
    # Test Python bridge if it exists
    if [ -f "$REPO_ROOT/hurdcog-integration/atomspace-hurdcog-bridge.py" ]; then
        if python3 -m py_compile "$REPO_ROOT/hurdcog-integration/atomspace-hurdcog-bridge.py" 2>/dev/null; then
            echo "  ✓ Python bridge syntax valid"
            PASSED=$((PASSED + 1))
        else
            echo "  ✗ Python bridge has syntax errors"
            FAILED=$((FAILED + 1))
        fi
    fi
else
    echo "  ⊘ Python3 not available"
    SKIPPED=$((SKIPPED + 1))
fi

# Test 6: Guile/Scheme functionality
echo ""
echo "Test 6: Testing Guile/Scheme environment..."
if command -v guile >/dev/null 2>&1; then
    if guile -c "(format #t \"Guile OK~%\")" >/dev/null 2>&1; then
        echo "  ✓ Guile is functional"
        PASSED=$((PASSED + 1))
    else
        echo "  ✗ Guile test failed"
        FAILED=$((FAILED + 1))
    fi
else
    echo "  ⊘ Guile not available"
    SKIPPED=$((SKIPPED + 1))
fi

# Test 7: Synergy scripts
echo ""
echo "Test 7: Checking synergy scripts..."
test_file "synergy.sh" "$REPO_ROOT/synergy.sh"
test_file "synergy_agi_os.sh" "$REPO_ROOT/synergy_agi_os.sh"

# Test 8: Build system files
echo ""
echo "Test 8: Checking build system..."
test_file "Makefile" "$REPO_ROOT/Makefile"
test_file "Makefile.build-sequences" "$REPO_ROOT/Makefile.build-sequences"
test_file "guix.scm" "$REPO_ROOT/guix.scm"
test_file "vcpkg.json" "$REPO_ROOT/vcpkg.json"

# Test 9: Test infrastructure
echo ""
echo "Test 9: Checking test infrastructure..."
test_dir "tests directory" "$REPO_ROOT/tests"
test_dir "tests/synergy" "$REPO_ROOT/tests/synergy"

# Summary
echo ""
echo "════════════════════════════════════════════════════════════"
echo "Test Results: $PASSED passed, $FAILED failed, $SKIPPED skipped"
echo "════════════════════════════════════════════════════════════"

# Output GitHub Actions summary if available
if [ -n "${GITHUB_STEP_SUMMARY:-}" ]; then
    echo "## Integration Test Results" >> "$GITHUB_STEP_SUMMARY"
    echo "| Metric | Count |" >> "$GITHUB_STEP_SUMMARY"
    echo "|--------|-------|" >> "$GITHUB_STEP_SUMMARY"
    echo "| ✅ Passed | $PASSED |" >> "$GITHUB_STEP_SUMMARY"
    echo "| ❌ Failed | $FAILED |" >> "$GITHUB_STEP_SUMMARY"
    echo "| ⊘ Skipped | $SKIPPED |" >> "$GITHUB_STEP_SUMMARY"
fi

if [ $FAILED -eq 0 ]; then
    echo "✓ All required tests passed!"
    exit 0
else
    echo "✗ Some tests failed"
    exit 1
fi
