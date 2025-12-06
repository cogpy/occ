#!/usr/bin/env python3
"""
Validation script for Agent-Zero demonstration scenarios.

This script validates the structure, syntax, and documentation of the
demonstration scenarios without requiring Guile or OpenCog to be installed.

Task: AZ-DEMO-001
"""

import os
import sys
import re
from pathlib import Path

# Color codes for output
class Colors:
    GREEN = '\033[0;32m'
    RED = '\033[0;31m'
    YELLOW = '\033[1;33m'
    BLUE = '\033[0;34m'
    NC = '\033[0m'  # No Color


class DemoValidator:
    """Validator for demonstration scenarios"""
    
    def __init__(self, demo_dir):
        self.demo_dir = Path(demo_dir)
        self.results = []
        self.total_checks = 0
        self.passed_checks = 0
        
    def print_header(self):
        """Print validation header"""
        print()
        print("╔════════════════════════════════════════════════════════════╗")
        print("║     Agent-Zero Demonstration Validator                    ║")
        print("║     Validating Demo Structure and Documentation          ║")
        print("╚════════════════════════════════════════════════════════════╝")
        print()
    
    def check_file_exists(self, filename):
        """Check if a required file exists"""
        filepath = self.demo_dir / filename
        self.total_checks += 1
        
        if filepath.exists():
            self.passed_checks += 1
            print(f"{Colors.GREEN}✓{Colors.NC} File exists: {filename}")
            return True
        else:
            print(f"{Colors.RED}✗{Colors.NC} File missing: {filename}")
            return False
    
    def check_demo_structure(self, filename):
        """Check the structure of a demo file"""
        filepath = self.demo_dir / filename
        
        if not filepath.exists():
            return False
        
        with open(filepath, 'r') as f:
            content = f.read()
        
        # Check for required elements
        checks = [
            ("Shebang line", r"^#!/usr/bin/env guile"),
            ("use-modules opencog", r"\(use-modules \(opencog\)\)"),
            ("Demo header", r"Demo \d+:"),
            ("Task reference", r"AZ-DEMO-001"),
            ("Main demo function", r"\(define \(run-demo\)"),
            ("Demo execution", r"\(run-demo\)"),
        ]
        
        print(f"\n→ Validating {filename}:")
        all_passed = True
        
        for check_name, pattern in checks:
            self.total_checks += 1
            if re.search(pattern, content, re.MULTILINE):
                self.passed_checks += 1
                print(f"  {Colors.GREEN}✓{Colors.NC} {check_name}")
            else:
                print(f"  {Colors.RED}✗{Colors.NC} {check_name} not found")
                all_passed = False
        
        return all_passed
    
    def check_readme_documentation(self):
        """Check README documentation completeness"""
        readme_path = self.demo_dir / "README.md"
        
        if not readme_path.exists():
            return False
        
        with open(readme_path, 'r') as f:
            content = f.read()
        
        print("\n→ Validating README.md:")
        
        required_sections = [
            ("Overview section", r"## 🎯 Overview"),
            ("Demonstration Scenarios", r"## 📚 Demonstration Scenarios"),
            ("Demo 1 description", r"Demo 1:.*Cognitive Loop"),
            ("Demo 2 description", r"Demo 2:.*Knowledge Integration"),
            ("Demo 3 description", r"Demo 3:.*Perception-Action"),
            ("Demo 4 description", r"Demo 4:.*Goal Management"),
            ("Demo 5 description", r"Demo 5:.*Full Integration"),
            ("Building instructions", r"## 🔧 Building and Running"),
            ("Prerequisites", r"Prerequisites"),
            ("Usage instructions", r"Usage"),
            ("Key concepts", r"## 🔍 Key Concepts"),
            ("Task ID reference", r"AZ-DEMO-001"),
        ]
        
        all_passed = True
        for section_name, pattern in required_sections:
            self.total_checks += 1
            if re.search(pattern, content, re.IGNORECASE):
                self.passed_checks += 1
                print(f"  {Colors.GREEN}✓{Colors.NC} {section_name}")
            else:
                print(f"  {Colors.RED}✗{Colors.NC} {section_name} missing")
                all_passed = False
        
        return all_passed
    
    def check_cmake_configuration(self):
        """Check CMakeLists.txt"""
        cmake_path = self.demo_dir / "CMakeLists.txt"
        
        if not cmake_path.exists():
            return False
        
        with open(cmake_path, 'r') as f:
            content = f.read()
        
        print("\n→ Validating CMakeLists.txt:")
        
        required_elements = [
            ("Project declaration", r"project\(agent-zero-demonstrations"),
            ("Demo files list", r"DEMO_FILES"),
            ("Install configuration", r"install\("),
            ("Custom targets", r"add_custom_target"),
            ("Run all demos target", r"run-demonstrations"),
        ]
        
        all_passed = True
        for element_name, pattern in required_elements:
            self.total_checks += 1
            if re.search(pattern, content):
                self.passed_checks += 1
                print(f"  {Colors.GREEN}✓{Colors.NC} {element_name}")
            else:
                print(f"  {Colors.RED}✗{Colors.NC} {element_name} missing")
                all_passed = False
        
        return all_passed
    
    def check_test_runner(self):
        """Check test runner script"""
        script_path = self.demo_dir / "run_all_demos.sh"
        
        if not script_path.exists():
            return False
        
        with open(script_path, 'r') as f:
            content = f.read()
        
        print("\n→ Validating run_all_demos.sh:")
        
        required_elements = [
            ("Shebang", r"^#!/bin/bash"),
            ("Error handling", r"set -e"),
            ("Check dependencies", r"check_dependencies"),
            ("Run demo function", r"run_demo"),
            ("Demo list", r"DEMOS="),
            ("All 5 demos listed", r"demo1.*demo2.*demo3.*demo4.*demo5"),
            ("Summary function", r"print_summary"),
        ]
        
        all_passed = True
        for element_name, pattern in required_elements:
            self.total_checks += 1
            if re.search(pattern, content, re.DOTALL):
                self.passed_checks += 1
                print(f"  {Colors.GREEN}✓{Colors.NC} {element_name}")
            else:
                print(f"  {Colors.RED}✗{Colors.NC} {element_name} missing")
                all_passed = False
        
        # Check if executable
        self.total_checks += 1
        if os.access(script_path, os.X_OK):
            self.passed_checks += 1
            print(f"  {Colors.GREEN}✓{Colors.NC} Script is executable")
        else:
            print(f"  {Colors.RED}✗{Colors.NC} Script is not executable")
            all_passed = False
        
        return all_passed
    
    def print_summary(self):
        """Print validation summary"""
        print()
        print("╔════════════════════════════════════════════════════════════╗")
        print("║     Validation Summary                                    ║")
        print("╚════════════════════════════════════════════════════════════╝")
        print()
        
        success_rate = (self.passed_checks / self.total_checks * 100) if self.total_checks > 0 else 0
        
        print(f"  Total Checks:  {self.total_checks}")
        print(f"  Passed:        {Colors.GREEN}{self.passed_checks}{Colors.NC}")
        print(f"  Failed:        {Colors.RED}{self.total_checks - self.passed_checks}{Colors.NC}")
        print(f"  Success Rate:  {success_rate:.1f}%")
        print()
        
        if self.passed_checks == self.total_checks:
            print(f"  {Colors.GREEN}✓✓✓ All validation checks passed! ✓✓✓{Colors.NC}")
            print()
            return True
        else:
            print(f"  {Colors.YELLOW}⚠ Some validation checks failed{Colors.NC}")
            print()
            return False
    
    def validate_all(self):
        """Run all validation checks"""
        self.print_header()
        
        # Check required files
        print("→ Checking required files:")
        required_files = [
            "README.md",
            "CMakeLists.txt",
            "run_all_demos.sh",
            "demo1_cognitive_loop.scm",
            "demo2_knowledge_integration.scm",
            "demo3_perception_action.scm",
            "demo4_goal_management.scm",
            "demo5_full_integration.scm",
        ]
        
        for filename in required_files:
            self.check_file_exists(filename)
        
        # Validate demo files
        demo_files = [
            "demo1_cognitive_loop.scm",
            "demo2_knowledge_integration.scm",
            "demo3_perception_action.scm",
            "demo4_goal_management.scm",
            "demo5_full_integration.scm",
        ]
        
        for demo_file in demo_files:
            self.check_demo_structure(demo_file)
        
        # Validate documentation
        self.check_readme_documentation()
        
        # Validate build system
        self.check_cmake_configuration()
        
        # Validate test runner
        self.check_test_runner()
        
        # Print summary
        return self.print_summary()


def main():
    """Main validation function"""
    # Get demo directory (same directory as this script)
    demo_dir = Path(__file__).parent
    
    # Create validator
    validator = DemoValidator(demo_dir)
    
    # Run validation
    success = validator.validate_all()
    
    # Exit with appropriate code
    sys.exit(0 if success else 1)


if __name__ == "__main__":
    main()
