#!/bin/bash
###############################################################################
# run_all_demos.sh - Agent-Zero Demonstration Test Runner
#
# This script runs all Agent-Zero demonstration scenarios and validates
# their execution. It provides comprehensive output and error handling.
#
# Usage: ./run_all_demos.sh
#
# Task: AZ-DEMO-001
###############################################################################

set -e  # Exit on error

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Demo directory
DEMO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Results tracking
TOTAL_DEMOS=0
PASSED_DEMOS=0
FAILED_DEMOS=0

###############################################################################
# Helper Functions
###############################################################################

print_header() {
    echo ""
    echo "╔════════════════════════════════════════════════════════════╗"
    echo "║     Agent-Zero Demonstration Test Runner                 ║"
    echo "║     Testing All Demonstration Scenarios                  ║"
    echo "╚════════════════════════════════════════════════════════════╝"
    echo ""
}

print_demo_header() {
    local demo_name=$1
    echo ""
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "  Testing: ${demo_name}"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
}

print_result() {
    local status=$1
    local demo_name=$2
    
    if [ "$status" = "PASS" ]; then
        echo -e "${GREEN}✓ PASS${NC}: ${demo_name}"
        ((PASSED_DEMOS++))
    else
        echo -e "${RED}✗ FAIL${NC}: ${demo_name}"
        ((FAILED_DEMOS++))
    fi
    ((TOTAL_DEMOS++))
}

check_dependencies() {
    echo "→ Checking dependencies..."
    
    # Check for Guile
    if ! command -v guile &> /dev/null; then
        echo -e "${RED}Error: Guile not found${NC}"
        echo "Please install Guile 3.0: sudo apt-get install guile-3.0"
        exit 1
    fi
    
    echo "  ✓ Guile found: $(guile --version | head -1)"
    
    # Check for OpenCog modules (optional, for better validation)
    # This is a basic check - actual availability will be tested by demos
    echo "  ✓ Dependencies check complete"
    echo ""
}

run_demo() {
    local demo_file=$1
    local demo_name=$2
    local log_file="${DEMO_DIR}/logs/${demo_name}.log"
    
    print_demo_header "$demo_name"
    
    # Create logs directory if it doesn't exist
    mkdir -p "${DEMO_DIR}/logs"
    
    # Run the demo and capture output
    echo "  Running demo..."
    if guile -l "${demo_file}" > "${log_file}" 2>&1; then
        # Check if demo completed successfully
        if grep -q "Complete:" "${log_file}"; then
            print_result "PASS" "$demo_name"
            
            # Show brief summary
            echo "  Summary:"
            grep "Key Achievements:" -A 5 "${log_file}" | head -6 | sed 's/^/    /'
            
            return 0
        else
            print_result "FAIL" "$demo_name"
            echo "  Error: Demo did not complete successfully"
            echo "  Check log: ${log_file}"
            return 1
        fi
    else
        print_result "FAIL" "$demo_name"
        echo "  Error: Demo execution failed"
        echo "  Last 10 lines of output:"
        tail -10 "${log_file}" | sed 's/^/    /'
        return 1
    fi
}

print_summary() {
    echo ""
    echo "╔════════════════════════════════════════════════════════════╗"
    echo "║     Test Summary                                          ║"
    echo "╚════════════════════════════════════════════════════════════╝"
    echo ""
    echo "  Total Demos:  ${TOTAL_DEMOS}"
    echo -e "  Passed:       ${GREEN}${PASSED_DEMOS}${NC}"
    echo -e "  Failed:       ${RED}${FAILED_DEMOS}${NC}"
    
    if [ ${FAILED_DEMOS} -eq 0 ]; then
        echo ""
        echo -e "  ${GREEN}✓✓✓ All demonstrations passed! ✓✓✓${NC}"
        echo ""
        return 0
    else
        echo ""
        echo -e "  ${RED}✗ Some demonstrations failed${NC}"
        echo "  Review logs in: ${DEMO_DIR}/logs/"
        echo ""
        return 1
    fi
}

###############################################################################
# Main Execution
###############################################################################

main() {
    print_header
    
    # Check dependencies
    check_dependencies
    
    # List of demos to run
    declare -a DEMOS=(
        "demo1_cognitive_loop.scm:Demo 1 - Basic Cognitive Loop"
        "demo2_knowledge_integration.scm:Demo 2 - Knowledge Integration"
        "demo3_perception_action.scm:Demo 3 - Perception-Action Cycle"
        "demo4_goal_management.scm:Demo 4 - Goal Management"
        "demo5_full_integration.scm:Demo 5 - Full System Integration"
    )
    
    echo "Running ${#DEMOS[@]} demonstration scenarios..."
    echo ""
    
    # Run each demo
    for demo_entry in "${DEMOS[@]}"; do
        IFS=':' read -r demo_file demo_name <<< "$demo_entry"
        
        # Check if demo file exists
        if [ ! -f "${DEMO_DIR}/${demo_file}" ]; then
            echo -e "${YELLOW}Warning: ${demo_file} not found, skipping${NC}"
            continue
        fi
        
        # Run the demo (continue even if it fails)
        run_demo "${DEMO_DIR}/${demo_file}" "$demo_name" || true
    done
    
    # Print summary
    print_summary
    
    # Return appropriate exit code
    if [ ${FAILED_DEMOS} -eq 0 ]; then
        exit 0
    else
        exit 1
    fi
}

# Run main function
main "$@"
