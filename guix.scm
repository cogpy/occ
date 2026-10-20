;;; Guix package definitions for the OpenCog Collection (OCC) monorepo.
;;;
;;; Each OpenCog component in this repository is a standalone CMake project
;;; that expects its dependencies to be installed first, so each one is its
;;; own package here, built from its subdirectory and chained in dependency
;;; order (the same order as .github/workflows/ci-linux.yml):
;;;
;;;   cogutil -> atomspace -> atomspace-storage -> cogserver
;;;                        -> matrix, unify -> ure, sensory
;;;
;;; The recipes follow upstream Guix's (gnu packages opencog), adapted to
;;; build from local checkouts instead of separate git repositories.
;;;
;;; Build everything:      guix build -f guix.scm
;;; Build one component:   guix build -e '(begin (load "guix.scm") occ-atomspace)'

(use-modules (guix packages)
             (guix gexp)
             (guix utils)
             (guix build-system cmake)
             (guix build-system copy)
             ((guix licenses) #:prefix license:)
             (gnu packages)
             (gnu packages boost)
             (gnu packages check)
             (gnu packages cpp)
             (gnu packages datastructures)
             (gnu packages guile)
             (gnu packages multiprecision)
             (gnu packages networking)
             (gnu packages pkg-config)
             (gnu packages python)
             (gnu packages python-xyz)
             (gnu packages serialization)
             (gnu packages tls))

(define %occ-root
  ;; Directory holding this file, i.e. the repository root.
  (current-source-directory))

(define %occ-version "0.1.0")

(define (occ-source directory)
  "Return the source of the OCC component in DIRECTORY, without VCS data
and build artifacts."
  (local-file (string-append %occ-root "/" directory)
              (string-append "occ-" directory "-source")
              #:recursive? #t
              #:select? (lambda (file stat)
                          (not (or (string-contains file "/.git")
                                   (string-contains file "/build/")
                                   (string-contains file "/__pycache__/")
                                   (string-suffix? ".pyc" file)
                                   (string-suffix? ".o" file)
                                   (string-suffix? ".so" file))))))

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

(define-public occ-cogutil
  (package
    (name "occ-cogutil")
    (version %occ-version)
    (source (occ-source "cogutil"))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                       ;not yet run in CI either
      ;; Skip ldconfig, which requires root and is unnecessary in Guix.
      #:configure-flags #~(list "-DSKIP_LDCONF=ON")))
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
           #:configure-flags (occ-configure-flags)))
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
           #:configure-flags (occ-configure-flags)))
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
           #:configure-flags (occ-configure-flags)))
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
           #:configure-flags (occ-configure-flags)))
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
           #:configure-flags (occ-configure-flags)))
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
           #:configure-flags (occ-configure-flags)))
    (inputs (list boost occ-atomspace occ-cogutil gmp guile-3.0 python))
    (native-inputs (list cxxtest pkg-config python-cython))
    (home-page "https://github.com/cogpy/occ")
    (synopsis "Dataflow system connecting the AtomSpace to external sources")
    (description "Sensory provides a dataflow system for moving data between
the AtomSpace and the external world.  This is the OCC monorepo copy.")
    (license license:agpl3)))

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
           occ-sensory))
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
@end itemize")
    (license license:agpl3+)))

;; Return the collection so that `guix build -f guix.scm' builds everything.
opencog-collection
