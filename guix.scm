;;; Top-level Guix package file for OpenCog Collection
;;; Use with: guix build -f guix.scm

(use-modules (guix packages)
             (guix download)
             (guix git-download)
             (guix gexp)
             (guix build-system python)
             (guix build-system cargo)
             (guix build-system cmake)
             (guix build-system gnu)
             (guix build-system trivial)
             (guix build utils)
             (guix build cmake-build-system)
             ((guix licenses) #:prefix license:)
             (gnu packages)
             (gnu packages base)
             (gnu packages python)
             (gnu packages python-xyz)
             (gnu packages python-science)
             (gnu packages machine-learning)
             (gnu packages rust)
             (gnu packages crates-io)
             (gnu packages cmake)
             (gnu packages pkg-config)
             (gnu packages guile)
             (gnu packages boost)
             (gnu packages serialization)
             (gnu packages databases)
             (gnu packages version-control)
             (gnu packages maths)
             (gnu packages cpp)
             (gnu packages check)
             (gnu packages build-tools))

(define-public opencog-collection
  (package
    (name "opencog-collection")
    (version "0.1.0")
    (source (local-file "." "opencog-collection-checkout"
                        #:recursive? #t))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:configure-flags
      #~(list "-DCMAKE_BUILD_TYPE=Release"
              "-DBUILD_COGUTIL=ON"
              "-DBUILD_ATOMSPACE=ON"
              "-DBUILD_COGSERVER=ON"
              "-DBUILD_MATRIX=ON"
              "-DBUILD_LEARN=ON"
              "-DBUILD_AGENTS=ON"
              "-DBUILD_SENSORY=ON"
              "-DBUILD_COGGML=OFF"
              "-DBUILD_COGSELF=OFF"
              "-DBUILD_ATOMSPACE_ACCELERATOR=OFF"
              "-DBUILD_AGENTIC_CHATBOTS=OFF"
              "-DBUILD_ATOMSPACE_STORAGE=OFF"
              "-DBUILD_ATOMSPACE_EXTENSIONS=OFF"
              "-DBUILD_GNUCASH=OFF"
              "-DBUILD_KOBOLDCPP=OFF"
              "-DBUILD_APHRODITE=OFF"
              (string-append "-DCMAKE_INSTALL_PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'check-dependencies
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (for-each (lambda (dir)
                         (if (file-exists? dir)
                             (format #t "Found directory: ~a~%" dir)
                             (begin
                               (format #t "ERROR: Required directory ~a not found~%" dir)
                               (error "Missing required directory" dir))))
                       '("cogutil" "atomspace" "cogserver" "matrix" "learn" "agents" "sensory"))
              #t))
          (add-before 'configure 'set-environment
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (setenv "HOME" (getcwd))
              (let ((boost (assoc-ref inputs "boost")))
                (when boost
                  (setenv "BOOST_ROOT" boost)
                  (format #t "Set BOOST_ROOT to ~a~%" boost)))
              #t))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (lib (string-append out "/lib"))
                     (share (string-append out "/share/opencog-collection")))
                
                (invoke "make" "install")
                
                (mkdir-p share)
                (mkdir-p bin)
                (mkdir-p lib)
                
                (when (file-exists? "../app.py")
                  (install-file "../app.py" share)
                  (call-with-output-file (string-append bin "/opencog-demo")
                    (lambda (port)
                      (format port "#!/bin/sh~%exec python3 ~a/app.py \"$@\"~%"
                              share)))
                  (chmod (string-append bin "/opencog-demo") #o755))
                
                (when (file-exists? "../README.md")
                  (install-file "../README.md" share))
                (when (file-exists? "../requirements.txt")
                  (install-file "../requirements.txt" share))
                
                #t))))))
    (native-inputs
     (list pkg-config
           cmake
           cxxtest))
    (inputs
     (list python
           python-numpy
           python-pandas
           python-scikit-learn
           python-matplotlib
           guile-3.0
           boost
           blas
           lapack
           gsl))
    (propagated-inputs
     (list python-numpy
           python-pandas
           python-scikit-learn
           python-matplotlib))
    (home-page "https://github.com/rzonedevops/occ")
    (synopsis "OpenCog Collection - Machine Learning Integration Environment")
    (description
     "This package provides the OpenCog Collection monorepo - an integrated
development environment for cognitive computing and artificial general intelligence (AGI).
The collection brings together multiple OpenCog-related projects into a coherent
whole for cognitive synergy.

The package includes the core OpenCog components:
@itemize
@item CogUtil - Base utilities and configuration system
@item AtomSpace - Hypergraph database and query engine
@item CogServer - Networking and communication layer
@item Matrix - Sparse vector and graph processing
@item Learn - Symbolic learning algorithms
@item Agents - Interactive cognitive agents
@item Sensory - Dataflow system for external world interaction
@end itemize

Additionally includes:
@itemize
@item Python-based machine learning demonstration using scikit-learn
@item Complete source for research and development
@item Development environment for cognitive computing applications
@end itemize")
    (license license:gpl3+)))

opencog-collection
