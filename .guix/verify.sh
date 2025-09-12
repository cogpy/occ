#!/bin/bash

# Simple verification script for Guix packaging
# This script checks if all required files exist and have basic syntax

echo "=== OpenCog Collection Guix Packaging Verification ==="
echo ""

# Check if required files exist
files=(.guix-channel .guix/manifest.scm .guix/modules/opencog-package.scm guix.scm)
missing_files=()

for file in "${files[@]}"; do
    if [[ -f "$file" ]]; then
        echo "✓ $file exists"
    else
        echo "✗ $file missing"
        missing_files+=("$file")
    fi
done

echo ""

# Check if guix is available
if command -v guix &> /dev/null; then
    echo "✓ Guix command available"
    
    # Basic syntax check for guix.scm
    echo "Checking guix.scm syntax..."
    if guix build -f guix.scm --dry-run &> /dev/null; then
        echo "✓ guix.scm syntax appears valid"
    else
        echo "✗ guix.scm may have syntax issues"
    fi
    
    # Basic syntax check for manifest
    echo "Checking manifest syntax..."
    if guix shell -m .guix/manifest.scm --dry-run &> /dev/null; then
        echo "✓ manifest.scm syntax appears valid"
    else
        echo "✗ manifest.scm may have syntax issues"
    fi
else
    echo "! Guix not available - cannot test syntax"
    echo "  Install Guix to test the packaging files"
fi

echo ""

# Check Python app
if command -v python3 &> /dev/null; then
    echo "Checking Python dependencies..."
    if python3 -c "import numpy, pandas, sklearn, matplotlib" 2>/dev/null; then
        echo "✓ Python dependencies available"
        
        echo "Testing app.py..."
        if python3 app.py >/dev/null 2>&1; then
            echo "✓ app.py runs successfully"
        else
            echo "! app.py has issues (may need dependencies)"
        fi
    else
        echo "! Python scientific packages not installed"
        echo "  Run: pip3 install -r requirements.txt"
    fi
else
    echo "! Python3 not available"
fi

echo ""

# Check Rust component
if command -v cargo &> /dev/null; then
    echo "Checking Rust component..."
    if [[ -f "Cargo.toml" ]]; then
        if cargo check &>/dev/null; then
            echo "✓ Rust component compiles"
        else
            echo "! Rust component has issues"
        fi
    else
        echo "! Cargo.toml not found"
    fi
else
    echo "! Cargo not available"
fi

echo ""

if [[ ${#missing_files[@]} -eq 0 ]]; then
    echo "🎉 All required Guix files are present!"
    echo ""
    echo "To use with Guix:"
    echo "  guix shell -m .guix/manifest.scm    # Development environment"
    echo "  guix build -f guix.scm             # Build package"
    echo "  guix install -f guix.scm           # Install package"
    echo ""
    echo "See .guix/README.md for detailed instructions."
else
    echo "❌ Missing files: ${missing_files[*]}"
fi