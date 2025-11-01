> # OpenCog Collection (OCC)
> ### An Integrated Architecture for Cognitive Synergy

**The OpenCog Collection (OCC) is a monorepo that integrates multiple OpenCog components and external tools into a unified cognitive architecture.** Our primary goal is to foster **cognitive synergy**, where the interaction of diverse AI components leads to emergent intelligence and capabilities beyond the sum of their individual parts.

This repository provides a complete, FSF-endorsed, and reproducible environment for research and development in Artificial General Intelligence (AGI), cognitive computing, and hypergraph-based AI.

---

## Key Features

- **Unified Cognitive Architecture**: A coherent system that brings together core OpenCog components like the **AtomSpace** (a hypergraph database) with advanced AI tools.
- **Cognitive Synergy by Design**: The architecture is explicitly designed to facilitate the interaction and collaboration of different AI paradigms, including symbolic reasoning, machine learning, and evolutionary algorithms.
- **Reproducible Development Environment**: Utilizes **GNU Guix** and a devcontainer to provide a fully declarative and isolated development environment, ensuring that all developers have a consistent setup.
- **Extensible and Modular**: The OCC is designed to be easily extended with new components and integrations, allowing researchers to experiment with different cognitive architectures.
- **Focus on AGI Research**: The ultimate goal of the OCC is to provide a platform for building and experimenting with AGI systems.

---

## Getting Started

We recommend using the provided devcontainer for the best experience. This will automatically set up a complete development environment with all the necessary dependencies.

1.  **Clone the repository:**

    ```bash
    git clone --recurse-submodules https://github.com/Kaw-Aii/occ.git
    ```

2.  **Open in a devcontainer-compatible IDE (like VS Code).**

3.  **Start building and experimenting!**

For more detailed instructions, please see our [Getting Started Guide](docs/getting-started.md).

---

## Documentation

### Core Documentation
- **[Architecture Overview](docs/architecture.md)**: A detailed look at the OCC architecture and its components.
- **[Cognitive Synergy](docs/cognitive-synergy.md)**: An explanation of the principles of cognitive synergy and how they are applied in the OCC.
- **[Contributing Guide](CONTRIBUTING.md)**: How to contribute to the OpenCog Collection.

### AGI-OS / AGI-Kern Evaluation
- **[AGI Kernel Evaluation](docs/AGI_KERNEL_EVALUATION.md)**: Comprehensive evaluation of OCC against traditional OS kernel primitives for AGI-OS development.
- **[AGI-OS Integration Guide](docs/AGI_OS_INTEGRATION_GUIDE.md)**: Technical specifications for integrating 5 OpenCog repositories into a unified AGI Operating System.
- **[AGI-Kern Coverage Summary](docs/AGI_KERN_COVERAGE_SUMMARY.md)**: Quick reference guide for kernel feature coverage and integration roadmap.

---

## Community and Support

The OpenCog project is a community-driven effort. We welcome contributions from researchers, developers, and anyone interested in AGI.

- **Discussions:** [GitHub Discussions](https://github.com/opencog/occ/discussions)
- **Mailing List:** [OpenCog Google Group](https://groups.google.com/g/opencog)

---

*The OpenCog Collection is a continuation of the OpenCog project, pioneered by Dr. Ben Goertzel.*

## Cognitive Synergy Enhancements

To enforce and test the cognitive synergy between the various components (C++, Python, Rust), a unified entry point has been created.

### Unified Synergy Check

Run the following script to build all components, and perform mock interoperability and demo tests:

```bash
./synergy.sh
```

This script ensures that the entire collection is a coherent whole, a key principle for achieving cognitive synergy.

