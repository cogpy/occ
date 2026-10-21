;;; Guix package definitions for the OpenCog Collection (OCC) monorepo.
;;;
;;; Each OpenCog component in this repository is a standalone CMake project
;;; that expects its dependencies to be installed first, so each one is its
;;; own package here, built from its subdirectory and chained in dependency
;;; order (the same order as .github/workflows/ci-linux.yml):
;;;
;;;   cogutil -> atomspace -> atomspace-storage -> cogserver
;;;                                             -> atomspace-rocks
;;;                        -> matrix, unify -> ure, sensory
;;;                        -> lg-atomese
;;;   cogserver + atomspace-rocks + lg-atomese -> learn
;;;
;;; agents (Scheme only), coggml and cogself (standalone C++) have no OpenCog
;;; dependencies.
;;;
;;; Not packaged: atomspace-websockets (needs uSockets, which Guix doesn't
;;; have), atomspace-ipfs (needs an IPFS C++ client that Guix doesn't
;;; have), atomspace-agents (its lib/CogAgentsConfig.cmake.in is missing from
;;; the repo), dimensional-embedding (needs opencog/util/cluster.h, which
;;; cogutil no longer has), atomspace-dht (needs opencog/atomspace/AtomTable.h,
;;; removed from the AtomSpace), integration (MACHSPACE_STORAGE_NODE is never
;;; defined as an atom type), tensor-logic (its code doesn't compile with any compiler: it passes
;;; a FlowTensor where a FlowRecord is expected), motor (installs only a
;;; README), the OS layers (cognumach, hurdcog, inferno-agi-os), wrappers for
;;; external projects (gnucash, koboldcpp, aphrodite-engine), ROS/catkin
;;; packages and the benchmark harness.
;;;
;;; The recipes follow upstream Guix's (gnu packages opencog), adapted to
;;; build from local checkouts instead of separate git repositories.
;;;
;;; Build everything:      guix build -f guix.scm
;;; Build one component:   guix build -e '(begin (load "guix.scm") occ-atomspace)'

(use-modules (srfi srfi-1)
             (guix packages)
             (guix gexp)
             (guix git-download)
             (guix utils)
             (guix build-system cmake)
             (guix build-system copy)
             ((guix licenses) #:prefix license:)
             (gnu packages)
             (gnu packages adns)
             (gnu packages boost)
             (gnu packages check)
             (gnu packages compression)
             (gnu packages cpp)
             (gnu packages databases)
             (gnu packages datastructures)
             (gnu packages gtk)
             (gnu packages guile)
             (gnu packages image-processing)
             (gnu packages language)
             (gnu packages linux)
             (gnu packages multiprecision)
             (gnu packages networking)
             (gnu packages opencl)
             (gnu packages pkg-config)
             (gnu packages protobuf)
             (gnu packages python)
             (gnu packages python-xyz)
             (gnu packages regex)
             (gnu packages rpc)
             (gnu packages serialization)
             (gnu packages tbb)
             (gnu packages tls))

(define %occ-root
  ;; Directory holding this file, i.e. the repository root.
  (current-source-directory))

(define %occ-version "0.1.0")

(define* (occ-source directory #:key (exclude '()))
  "Return the source of the OCC component in DIRECTORY, without VCS data
and build artifacts.  EXCLUDE lists subdirectories of DIRECTORY to leave
out, such as archives the build doesn't use."
  (define root (string-append %occ-root "/" directory))
  (define excluded
    (map (lambda (sub) (string-append root "/" sub)) exclude))
  (local-file root
              (string-append "occ-" directory "-source")
              #:recursive? #t
              #:select? (lambda (file stat)
                          (not (or (string-contains file "/.git")
                                   (string-contains file "/build/")
                                   (string-contains file "/__pycache__/")
                                   (string-suffix? ".pyc" file)
                                   (string-suffix? ".o" file)
                                   (string-suffix? ".so" file)
                                   (any (lambda (dir)
                                          (or (string=? file dir)
                                              (string-prefix?
                                               (string-append dir "/") file)))
                                        excluded))))))

(define (occ-configure-flags)
  "CMake flags shared by the Guile-using OpenCog components: point them at
Guix's Guile and install Guile and Python modules into the output."
  #~(list "-DSKIP_LDCONF=ON"
          (string-append "-DGUILE_INCLUDE_DIR=" #$guile-3.0
                         "/include/guile/3.0/")
          (string-append "-DGUILE_SITE_DIR=" #$output
                         "/share/guile/site/3.0/")
          (string-append "-DGUILE_CCACHE_DIR=" #$output
                         "/lib/guile/3.0/site-ccache")
          (string-append "-DPYTHON_INSTALL_PREFIX=" #$output
                         "/lib/python"
                         #$(version-major+minor (package-version python))
                         "/site-packages")))

(define (occ-atomspace-configure-flags)
  "Configure flags for components built on the AtomSpace.  Their CMake files
only search cogutil's module directory, which works when everything shares
one prefix such as /usr/local.  In Guix each package has its own prefix, so
also point them at the AtomSpace's modules (OpenCogMacros, OpenCogGuile,
OpenCogAtomTypes, ...)."
  #~(cons (string-append "-DCMAKE_MODULE_PATH=" #$occ-atomspace
                         "/share/opencog/cmake")
          #$(occ-configure-flags)))

(define-public occ-cogutil
  (package
    (name "occ-cogutil")
    (version %occ-version)
    (source (occ-source "cogutil"))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                       ;not yet run in CI either
      ;; etc/CMakeLists.txt installs an ld.so.conf.d snippet under
      ;; CMAKE_CONF_PREFIX, /etc by default, which is outside the store.
      ;; Guix doesn't use ld.so.conf, so keep it inside the output.
      #:configure-flags
      #~(list (string-append "-DCMAKE_CONF_PREFIX=" #$output "/etc"))))
    (inputs (list boost))
    (native-inputs (list cxxtest pkg-config python-minimal))
    (home-page "https://github.com/cogpy/occ")
    (synopsis "Low-level C++ programming utilities used by OpenCog components")
    (description "CogUtil is a collection of C++ utilities used for typical
programming tasks across OpenCog projects.  This is the OCC monorepo copy.")
    (license (list license:agpl3 license:asl2.0))))

(define-public occ-atomspace
  (package
    (name "occ-atomspace")
    (version %occ-version)
    (source (occ-source "atomspace"))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags (occ-configure-flags)))
    (inputs (list boost occ-cogutil gmp guile-3.0 python))
    (native-inputs (list cxxtest pkg-config python-cython))
    (home-page "https://github.com/cogpy/occ")
    (synopsis "OpenCog hypergraph database, query system and rule engine")
    (description "The OpenCog AtomSpace is an in-RAM knowledge representation
database with an associated query engine, graph-rewriting system and
rule-driven inference engine.  This is the OCC monorepo copy.")
    (license license:agpl3)))

(define-public occ-atomspace-storage
  (package
    (name "occ-atomspace-storage")
    (version %occ-version)
    (source (occ-source "atomspace-storage"))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags (occ-atomspace-configure-flags)))
    (inputs (list boost occ-atomspace occ-cogutil gmp guile-3.0 python))
    (native-inputs (list cxxtest pkg-config python-cython))
    (home-page "https://github.com/cogpy/occ")
    (synopsis "StorageNode API for the OpenCog AtomSpace")
    (description "This AtomSpace module provides the StorageNode base class,
the primary API for moving Atoms into, out of and between AtomSpaces.  This
is the OCC monorepo copy.")
    (license license:agpl3)))

(define-public occ-cogserver
  (package
    (name "occ-cogserver")
    (version %occ-version)
    (source (occ-source "cogserver"))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f                 ;tests start a network server
           #:configure-flags (occ-atomspace-configure-flags)))
    (inputs (list asio boost occ-atomspace occ-atomspace-storage occ-cogutil
                  gmp guile-3.0 jsoncpp openssl python))
    (native-inputs (list cxxtest pkg-config python-cython))
    (home-page "https://github.com/cogpy/occ")
    (synopsis "OpenCog network server")
    (description "The CogServer is a network and job server for OpenCog,
providing shell and WebSocket access to an AtomSpace.  This is the OCC
monorepo copy.")
    (license license:agpl3)))

(define-public occ-matrix
  (package
    (name "occ-matrix")
    (version %occ-version)
    (source (occ-source "matrix"))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags (occ-atomspace-configure-flags)))
    (inputs (list boost occ-atomspace occ-cogutil gmp guile-3.0 python))
    (native-inputs (list cxxtest pkg-config python-cython))
    (home-page "https://github.com/cogpy/occ")
    (synopsis "Sparse vector and matrix processing for the OpenCog AtomSpace")
    (description "Matrix provides sparse vector and matrix views of AtomSpace
data, used for statistical and machine-learning analysis of graphs.  This is
the OCC monorepo copy.")
    (license license:agpl3)))

(define-public occ-unify
  (package
    (name "occ-unify")
    (version %occ-version)
    (source (occ-source "unify"))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags (occ-atomspace-configure-flags)))
    (inputs (list boost occ-atomspace occ-cogutil gmp guile-3.0 python))
    (native-inputs (list cxxtest pkg-config python-cython))
    (home-page "https://github.com/cogpy/occ")
    (synopsis "Unification algorithms for the OpenCog AtomSpace")
    (description "Unify implements unification over AtomSpace patterns, used
by the Unified Rule Engine.  This is the OCC monorepo copy.")
    (license license:agpl3)))

(define-public occ-ure
  (package
    (name "occ-ure")
    (version %occ-version)
    (source (occ-source "ure"))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags (occ-atomspace-configure-flags)))
    (inputs (list boost occ-atomspace occ-cogutil occ-unify gmp guile-3.0
                  python))
    (native-inputs (list cxxtest pkg-config python-cython))
    (home-page "https://github.com/cogpy/occ")
    (synopsis "OpenCog Unified Rule Engine")
    (description "The Unified Rule Engine (URE) performs forward and backward
chaining over rules stored in the AtomSpace.  This is the OCC monorepo copy.")
    (license license:agpl3)))

(define-public occ-sensory
  (package
    (name "occ-sensory")
    (version %occ-version)
    (source (occ-source "sensory"))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags (occ-atomspace-configure-flags)))
    (inputs (list boost occ-atomspace occ-cogutil gmp guile-3.0 python))
    (native-inputs (list cxxtest pkg-config python-cython))
    (home-page "https://github.com/cogpy/occ")
    (synopsis "Dataflow system connecting the AtomSpace to external sources")
    (description "Sensory provides a dataflow system for moving data between
the AtomSpace and the external world.  This is the OCC monorepo copy.")
    (license license:agpl3)))

(define-public occ-atomspace-rocks
  (package
    (name "occ-atomspace-rocks")
    (version %occ-version)
    (source (occ-source "atomspace-rocks"))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags (occ-atomspace-configure-flags)))
    (inputs (list boost occ-atomspace occ-atomspace-storage occ-cogutil gmp
                  guile-3.0 python rocksdb))
    (native-inputs (list cxxtest pkg-config python-cython))
    (home-page "https://github.com/cogpy/occ")
    (synopsis "RocksDB storage backend for the OpenCog AtomSpace")
    (description "AtomSpace Rocks provides RocksStorageNode, which saves and
restores Atoms and AtomSpaces to disk using RocksDB.  This is the OCC
monorepo copy.")
    (license license:agpl3)))

(define-public occ-lg-atomese
  (package
    (name "occ-lg-atomese")
    (version %occ-version)
    (source (occ-source "lg-atomese"))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags (occ-atomspace-configure-flags)))
    (inputs (list boost occ-atomspace occ-atomspace-storage occ-cogutil gmp
                  guile-3.0 link-grammar python `(,util-linux "lib")))
    (native-inputs (list cxxtest pkg-config python-cython))
    (home-page "https://github.com/cogpy/occ")
    (synopsis "Link Grammar parses as Atomese for the OpenCog AtomSpace")
    (description "LG-Atomese makes Link Grammar parses available in the
AtomSpace and provides the @code{(opencog nlp)} Guile modules that Learn
uses.  This is the OCC monorepo copy.")
    (license license:agpl3)))

(define-public occ-learn
  (package
    (name "occ-learn")
    (version %occ-version)
    ;; attic/ (old experiments) and learn-lang-diary/ (research notes) are
    ;; about 700 MB and not part of the build.
    (source (occ-source "learn" #:exclude '("attic" "learn-lang-diary")))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags (occ-atomspace-configure-flags)))
    (inputs (list boost occ-atomspace occ-atomspace-rocks occ-atomspace-storage
                  occ-cogserver occ-cogutil occ-matrix gmp guile-3.0 python))
    ;; Needed at run time by (opencog learn): (ice-9 readline) from
    ;; scm/utils/utilities.scm, and (opencog nlp) from lg-atomese.
    (propagated-inputs (list guile-readline occ-lg-atomese))
    (native-inputs (list cxxtest pkg-config python-cython))
    (home-page "https://github.com/cogpy/occ")
    (synopsis "Symbolic learning of structure from data for OpenCog")
    (description "Learn discovers grammatical and semantic structure in data,
such as natural language, by counting statistics in the AtomSpace.  This is
the OCC monorepo copy.")
    (license license:agpl3)))

(define-public occ-agents
  (package
    (name "occ-agents")
    (version %occ-version)
    (source (occ-source "agents"))
    (build-system cmake-build-system)
    (arguments (list #:tests? #f))       ;installs Scheme files only
    (home-page "https://github.com/cogpy/occ")
    (synopsis "Prototype interactive agents written in Atomese")
    (description "Agents is an early prototype of interactive cognitive
agents written in Atomese (Scheme).  This is the OCC monorepo copy.")
    (license license:agpl3)))

(define-public occ-coggml
  (package
    (name "occ-coggml")
    (version %occ-version)
    (source (occ-source "coggml"))
    (build-system cmake-build-system)
    (arguments (list #:tests? #f))
    (home-page "https://github.com/cogpy/occ")
    (synopsis "CogGML self-aware microkernel for cognitive processing")
    (description "CogGML is a microkernel of cognitive shards with
self-awareness and distributed coordination, part of the OCC cognitive
architecture.")
    (license license:agpl3)))

(define-public occ-cogself
  (package
    (name "occ-cogself")
    (version %occ-version)
    (source (occ-source "cogself"))
    (build-system cmake-build-system)
    (arguments (list #:tests? #f))
    (home-page "https://github.com/cogpy/occ")
    (synopsis "CogSelf AGI cognitive synergy framework")
    (description "CogSelf is the OCC framework that coordinates cognitive
synergy between components, part of the OCC cognitive architecture.")
    (license license:agpl3)))

;;; Components below share one of two recipes, so they're built with these
;;; helpers.  Each adds what a component needs on top of the common inputs.

(define* (occ-atomspace-component directory
                                  #:key (inputs '()) (propagated-inputs '())
                                  synopsis description)
  "Return a package for the OCC component in DIRECTORY, a CMake project
built on the AtomSpace, with extra INPUTS and PROPAGATED-INPUTS."
  (package
    (name (string-append "occ-" directory))
    (version %occ-version)
    (source (occ-source directory))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags (occ-atomspace-configure-flags)))
    (inputs (append (list boost occ-atomspace occ-cogutil gmp guile-3.0 python)
                    inputs))
    (propagated-inputs propagated-inputs)
    (native-inputs (list cxxtest pkg-config python-cython))
    (home-page "https://github.com/cogpy/occ")
    (synopsis synopsis)
    (description (string-append description "  This is the OCC monorepo copy."))
    (license license:agpl3)))

(define* (occ-standalone-component directory #:key (inputs '())
                                   synopsis description)
  "Return a package for the OCC component in DIRECTORY, a CMake project
with no OpenCog dependencies."
  (package
    (name (string-append "occ-" directory))
    (version %occ-version)
    (source (occ-source directory))
    (build-system cmake-build-system)
    (arguments (list #:tests? #f))
    (inputs inputs)
    (home-page "https://github.com/cogpy/occ")
    (synopsis synopsis)
    (description description)
    (license license:agpl3)))

;; Attention allocation, reasoning and learning.

(define-public occ-attention
  (occ-atomspace-component "attention"
    #:inputs (list occ-atomspace-storage occ-cogserver)
    #:synopsis "Economic attention allocation for the OpenCog AtomSpace"
    #:description "Attention implements ECAN, which spreads short- and
long-term importance through the AtomSpace to focus processing."))

(define octomap
  ;; Not packaged in Guix; spacetime (and so pln) needs it.  Only the core
  ;; library is built, not the octovis viewer or dynamicEDT3D.
  (package
    (name "octomap")
    (version "1.10.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/OctoMap/octomap")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1k7cgczv079qwf0pk1cp2cdnln5g4qnz5nqzpjf1x2aw273hf523"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags
           #~(list "-DBUILD_OCTOVIS_SUBPROJECT=OFF"
                   "-DBUILD_DYNAMICETD3D_SUBPROJECT=OFF")))
    (home-page "https://octomap.github.io/")
    (synopsis "Probabilistic 3D occupancy mapping based on octrees")
    (description "OctoMap implements a 3D occupancy grid mapping approach
based on octrees, providing data structures and mapping algorithms.")
    (license license:bsd-3)))

(define-public occ-spacetime
  (occ-atomspace-component "spacetime"
    #:inputs (list octomap)
    #:synopsis "Spatial and temporal representations for the AtomSpace"
    #:description "SpaceTime provides time and space servers that index
Atoms by location and time."))

(define-public occ-pln
  (occ-atomspace-component "pln"
    #:inputs (list occ-spacetime occ-unify occ-ure)
    #:synopsis "Probabilistic Logic Networks for OpenCog"
    #:description "PLN is a probabilistic reasoning system built as rules for
the Unified Rule Engine."))

(define-public occ-miner
  (occ-atomspace-component "miner"
    #:inputs (list occ-unify occ-ure)
    #:synopsis "Frequent pattern miner for the OpenCog AtomSpace"
    #:description "Miner finds frequent and surprising patterns in the
AtomSpace using the Unified Rule Engine."))

(define-public occ-moses
  (occ-atomspace-component "moses"
    #:synopsis "Meta-Optimizing Semantic Evolutionary Search"
    #:description "MOSES is a program-learning system that evolves
programs to fit data using estimation-of-distribution algorithms."))

(define-public occ-asmoses
  (occ-atomspace-component "asmoses"
    #:inputs (list occ-unify occ-ure)
    #:synopsis "MOSES ported to represent programs in the AtomSpace"
    #:description "AS-MOSES is MOSES with its programs represented as
Atomese in the AtomSpace."))

;; AtomSpace storage and networking backends.

(define-public occ-atomspace-cog
  (occ-atomspace-component "atomspace-cog"
    #:inputs (list occ-atomspace-storage occ-cogserver)
    #:synopsis "CogServer-based network storage for the AtomSpace"
    #:description "AtomSpace Cog provides CogStorageNode, which moves Atoms
between AtomSpaces over the network through a CogServer."))

(define-public occ-atomspace-pgres
  (occ-atomspace-component "atomspace-pgres"
    #:inputs (list occ-atomspace-storage postgresql unixodbc)
    #:synopsis "PostgreSQL storage backend for the AtomSpace"
    #:description "AtomSpace Pgres stores Atoms and AtomSpaces in a
PostgreSQL database."))

(define-public occ-atomspace-bridge
  (occ-atomspace-component "atomspace-bridge"
    #:inputs (list occ-atomspace-storage postgresql)
    #:synopsis "Bridge between SQL databases and the AtomSpace"
    #:description "AtomSpace Bridge maps rows of SQL tables to Atoms."))

(define-public occ-atomspace-metta
  (occ-atomspace-component "atomspace-metta"
    #:synopsis "MeTTa language support for the AtomSpace"
    #:description "AtomSpace MeTTa lets MeTTa programs work with the
AtomSpace."))

(define-public occ-atomspace-rpc
  (occ-atomspace-component "atomspace-rpc"
    #:inputs (list occ-agi-bio occ-atomspace-storage abseil-cpp c-ares grpc
                   nlohmann-json openssl protobuf re2 zlib)
    #:synopsis "RPC interface to the AtomSpace"
    #:description "AtomSpace RPC exposes AtomSpace operations over remote
procedure calls."))

(define-public occ-atomspace-restful
  (occ-atomspace-component "atomspace-restful"
    #:inputs (list occ-atomspace-storage occ-attention occ-cogserver
                   cppzmq jsoncpp tbb zeromq)
    #:synopsis "REST API for the AtomSpace"
    #:description "AtomSpace RESTful serves the AtomSpace over HTTP with a
JSON REST API."))

(define-public occ-pattern-index
  (occ-atomspace-component "pattern-index"
    #:synopsis "Pattern index for fast AtomSpace queries"
    #:description "Pattern Index builds an index of AtomSpace patterns to
speed up pattern queries."))

(define-public occ-generate
  (occ-atomspace-component "generate"
    #:inputs (list `(,util-linux "lib"))
    #:synopsis "Network generation from AtomSpace grammars"
    #:description "Generate creates random networks that follow a grammar
expressed in the AtomSpace."))

(define-public occ-agi-bio
  (occ-atomspace-component "agi-bio"
    #:synopsis "OpenCog tools for biological data"
    #:description "AGI-Bio applies the AtomSpace and its reasoning tools to
genomic and biological data."))

(define-public occ-cheminformatics
  (occ-atomspace-component "cheminformatics"
    #:synopsis "Chemical structures in the AtomSpace"
    #:description "Cheminformatics represents molecules and chemical
reactions as Atoms."))

(define-public occ-vision
  (occ-atomspace-component "vision"
    #:inputs (list opencv)
    #:synopsis "Computer vision for the OpenCog AtomSpace"
    #:description "Vision brings OpenCV image processing results into the
AtomSpace."))

(define-public occ-visualization
  (occ-atomspace-component "visualization"
    #:inputs (list asio occ-atomspace-storage occ-cogserver gtk+)
    #:synopsis "AtomSpace visualization tools"
    #:description "Visualization provides GTK-based tools for viewing the
AtomSpace."))

(define-public occ-atomese-simd
  (occ-atomspace-component "atomese-simd"
    #:inputs (list occ-sensory opencl-clhpp opencl-headers opencl-icd-loader)
    #:synopsis "SIMD and GPU processing for Atomese"
    #:description "Atomese SIMD runs Atomese vector operations on SIMD
hardware."))

(define-public occ-opencog
  (occ-atomspace-component "opencog"
    #:inputs (list occ-atomspace-storage occ-attention occ-cogserver
                   occ-lg-atomese occ-pln occ-unify occ-ure link-grammar)
    #:synopsis "OpenCog cognitive architecture glue"
    #:description "OpenCog combines the AtomSpace components into the
cognitive architecture, with natural-language and reasoning modules."))

(define-public occ-agentic-chatbots
  (occ-standalone-component "agentic-chatbots"
    #:synopsis "Conversational AI integration for OCC"
    #:description "Agentic Chatbots connects conversational agents to the
OCC cognitive architecture."))

(define-public occ-atomspace-accelerator
  (occ-standalone-component "atomspace-accelerator"
    #:synopsis "High-performance inference engine for OCC"
    #:description "AtomSpace Accelerator is a high-performance inference
engine for the OCC cognitive architecture."))

(define-public occ-entelechy
  (occ-standalone-component "entelechy"
    #:synopsis "Entelechy self-actualization framework for OCC"
    #:description "Entelechy is the OCC framework for goal-directed
self-development of the cognitive architecture."))

(define-public opencog-collection
  (package
    (name "opencog-collection")
    (version %occ-version)
    (source (occ-source "synergy"))
    (build-system copy-build-system)
    (arguments
     ;; Install the cognitive-synergy Scheme modules where Guile finds them.
     (list #:install-plan #~'(("." "share/guile/site/3.0/synergy"
                               #:include-regexp ("\\.scm$")))))
    (propagated-inputs
     (list occ-cogutil
           occ-atomspace
           occ-atomspace-storage
           occ-cogserver
           occ-matrix
           occ-unify
           occ-ure
           occ-sensory
           occ-atomspace-rocks
           occ-lg-atomese
           occ-learn
           occ-agents
           occ-coggml
           occ-cogself
           occ-attention
           occ-spacetime
           occ-pln
           occ-miner
           occ-moses
           occ-asmoses
           occ-atomspace-cog
           occ-atomspace-pgres
           occ-atomspace-bridge
           occ-atomspace-metta
           occ-atomspace-rpc
           occ-atomspace-restful
           occ-pattern-index
           occ-generate
           occ-agi-bio
           occ-cheminformatics
           occ-vision
           occ-visualization
           occ-atomese-simd
           occ-opencog
           occ-agentic-chatbots
           occ-atomspace-accelerator
           occ-entelechy))
    (home-page "https://github.com/cogpy/occ")
    (synopsis "OpenCog Collection - Cognitive Computing and AGI Framework")
    (description
     "This package brings together the OpenCog components of the OpenCog
Collection (OCC) monorepo, an integrated environment for cognitive computing
and artificial general intelligence (AGI) research.

It installs the cognitive-synergy Scheme modules and propagates the
components, each built as its own package:
@itemize
@item CogUtil - base utilities and configuration system
@item AtomSpace - hypergraph database and knowledge representation system
@item AtomSpace Storage - StorageNode API for moving Atoms between AtomSpaces
@item CogServer - networking and inter-process communication layer
@item Matrix - sparse matrix and graph processing for machine learning
@item Unify and URE - unification and the Unified Rule Engine
@item Sensory - dataflow system for external world interaction
@item AtomSpace Rocks - RocksDB storage backend
@item LG-Atomese - Link Grammar parses in the AtomSpace
@item Learn - symbolic learning of structure from data
@item Agents - prototype interactive agents in Atomese
@item CogGML - self-aware microkernel for cognitive processing
@item CogSelf - AGI cognitive synergy framework
@item Attention - economic attention allocation (ECAN)
@item SpaceTime - spatial and temporal representations
@item PLN - Probabilistic Logic Networks
@item Miner - frequent pattern mining
@item MOSES and AS-MOSES - evolutionary program learning
@item AtomSpace Cog, Pgres, Bridge, MeTTa, RPC and RESTful - network, SQL,
MeTTa, RPC and REST interfaces to the AtomSpace
@item Pattern Index - pattern index for fast AtomSpace queries
@item Generate - network generation from AtomSpace grammars
@item AGI-Bio and Cheminformatics - biological and chemical data
@item Vision and Visualization - computer vision and AtomSpace viewers
@item Atomese SIMD - SIMD and GPU processing for Atomese
@item OpenCog - cognitive architecture glue
@item Agentic Chatbots, AtomSpace Accelerator and Entelechy - OCC conversational
AI, inference engine and self-actualization framework
@end itemize")
    (license license:agpl3+)))

;; Return the collection so that `guix build -f guix.scm' builds everything.
opencog-collection
