# GNU Guix Shepherd DevContainer Usage Guide

This guide explains how to use the GNU Guix Shepherd devcontainer for OpenCog development.

## Quick Start

1. **Open in DevContainer**
   ```bash
   # In VSCode: Command Palette -> "Dev Containers: Open in Container"
   # Or clone and open the .devcontainer
   ```

2. **Verify Guix Installation**
   ```bash
   guix --version
   shepherd --version
   ```

3. **Build OpenCog Package**
   ```bash
   guix build -f guix.scm
   ```

4. **Start Shepherd Services**
   ```bash
   shepherd -c .config/shepherd/init.scm &
   herd start opencog-build
   ```

## Package Management

### Install Dependencies
```bash
guix install cmake boost python guile
```

### Local Development
```bash
# Build locally with Guix
guix build -f guix.scm

# Install locally
guix install -f guix.scm
```

## Service Management

### Start Services
```bash
herd start opencog-build
```

### Check Service Status
```bash
herd status
herd status opencog-build
```

### Stop Services
```bash
herd stop opencog-build
```

## CI/CD Integration

The repository includes GitHub Actions workflow for automated Guix builds:

- **Workflow**: `.github/workflows/guix-build.yml`
- **Trigger**: Push to main branch or pull requests
- **Installation**: Non-interactive Guix installation using `curl -fsSL https://git.savannah.gnu.org/cgit/guix.git/plain/etc/guix-install.sh -o /tmp/guix-install.sh` followed by `printf '\n' | sudo bash /tmp/guix-install.sh`
- **Action**: Builds the package using `guix build -f guix.scm`

## Directory Structure

```
.project-root/
├── .devcontainer/
│   ├── Dockerfile              # Debian + Guix + Shepherd
│   └── devcontainer.json       # VSCode devcontainer config
├── guix.scm                    # Guix package definition
├── .config/
│   └── shepherd/init.scm      # Shepherd service config
└── src/...                    # Source code
```

## Benefits

- **Reproducible**: Identical builds across environments
- **FSF Compliant**: Uses only free software
- **Declarative**: Configuration as code
- **Portable**: Works on any Guix-supported platform
- **Integrated**: Seamless CI/CD with GitHub Actions