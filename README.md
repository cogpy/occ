


# Redox OS Machine Learning Integration Environment

This repository contains the necessary files to set up a development environment for the integration of machine learning into Redox OS using Python, Rust, Prolog, and C. This environment is specifically designed for the development of OpenCog Hyperon.

## Set up

### Traditional Setup

To set up the environment, run the following command in the terminal:

```
pip3 install -r requirements.txt && cargo build --release
```

This will install all the necessary dependencies and packages for the environment.

### Using GNU Guix

This repository includes complete GNU Guix packaging support. To use with Guix:

```bash
# Enter development environment with all dependencies
guix shell -m .guix/manifest.scm

# Or build the package directly
guix build -f guix.scm

# Or install the package
guix install -f guix.scm
```

See `.guix/README.md` for detailed Guix usage instructions.

## Get started

To start the development environment, run the following command in the terminal:

```
python3 app.py
```

This will run the sample code provided in `app.py` and allow you to start developing and testing your own code.

## Additional files

This repository also includes the following files:

- `requirements.txt`: contains a list of required Python packages for the environment
- `.vscode/launch.json`: contains configuration settings for debugging in Visual Studio Code
- `Cargo.toml`: contains configuration settings for the Rust package manager
- `src/main.rs`: contains a sample Rust code for the Hyperon library
- `src/lib.rs`: contains a sample Rust code for the Hyperon library
- `src/test.rs`: contains a sample Rust code for testing the Hyperon library
- `.guix/`: GNU Guix packaging files for reproducible environments

Feel free to modify these files as needed for your development process.

## License

This repository is licensed under the MIT License. See the `LICENSE` file for more information.
