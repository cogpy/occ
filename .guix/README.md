# Guix Packaging for OpenCog Collection

This directory contains GNU Guix packaging files for the OpenCog Collection (OCC) project.

## Files

- **`.guix/modules/opencog-package.scm`** - Main package definition module
- **`.guix/manifest.scm`** - Development environment manifest
- **`.guix-channel`** - Channel definition for adding this repo as a Guix channel
- **`guix.scm`** - Top-level package file for building

## Usage

### Using the development environment

To enter a development shell with all dependencies:

```bash
guix shell -m .guix/manifest.scm
```

This will provide you with:
- Python 3 with NumPy, Pandas, scikit-learn, and Matplotlib
- Rust and Cargo for building Hyperon components
- CMake and build tools
- Guile Scheme for OpenCog development
- Various development utilities

### Building the package

To build the OpenCog Collection package:

```bash
guix build -f guix.scm
```

### Installing the package

To install the package to your profile:

```bash
guix install -f guix.scm
```

### Using as a channel

To use this repository as a Guix channel, add it to your `~/.config/guix/channels.scm`:

```scheme
(cons* (channel
        (name 'opencog-collection)
        (url "https://github.com/rzonedevops/occ.git")
        (branch "main"))
       %default-channels)
```

Then run:

```bash
guix pull
guix install opencog-collection
```

## Package Structure

The package includes:
- Python machine learning demonstration using the iris dataset
- Rust-based Hyperon cognitive computing framework
- Complete OpenCog collection source code
- Development environment setup

## Dependencies

The package automatically handles dependencies including:
- Python scientific computing stack (NumPy, Pandas, scikit-learn, Matplotlib)
- Rust toolchain for Hyperon components
- CMake and build tools
- Guile Scheme runtime
- Boost C++ libraries

## Running the Application

After installation, you can run the main application with:

```bash
opencog-collection
```

Or directly with Python:

```bash
python3 app.py
```

## Development

For development, use the manifest to enter a complete development environment:

```bash
guix shell -m .guix/manifest.scm
# Now you have access to all development tools
cargo build --release  # Build Rust components
python3 app.py         # Run Python application
```