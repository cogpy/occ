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
             ((guix licenses) #:prefix license:)
             (gnu packages)
             (gnu packages base)
             (gnu packages python)
             (gnu packages python-xyz)
             (gnu packages python-science)
             (gnu packages machine-learning)
             (gnu packages rust)
             (gnu packages rust-apps)
             (gnu packages crates-io)
             (gnu packages cmake)
             (gnu packages pkg-config)
             (gnu packages guile)
             (gnu packages boost)
             (gnu packages serialization)
             (gnu packages databases)
             (gnu packages version-control)
             (gnu packages maths)
             (gnu packages algebra)
             (gnu packages cpp)
             (gnu packages check)
             (gnu packages build-tools)
             (gnu packages commencement))

(define-public opencog-collection
  (package
    (name "opencog-collection")
    (version "0.1.0")
    (source (local-file "." "opencog-collection-checkout"
                        #:recursive? #t))
    (build-system cmake-build-system)
    (arguments
      `(#:tests? #f  ; Disable tests for now as they may require network access
        #:configure-flags
        ,(list "-DCMAKE_BUILD_TYPE=Release"
               "-DCMAKE_VERBOSE_MAKEFILE=ON"
               "-DBUILD_COGUTIL=ON"
               "-DBUILD_ATOMSPACE=ON"
               "-DBUILD_COGSERVER=ON"
               "-DBUILD_MATRIX=ON"
               "-DBUILD_LEARN=ON"
               "-DBUILD_AGENTS=ON"
               "-DBUILD_SENSORY=ON"
               "-DBUILD_COGGML=ON"
               "-DBUILD_COGSELF=ON"
               "-DBUILD_ATOMSPACE_ACCELERATOR=ON"
               "-DBUILD_AGENTIC_CHATBOTS=ON"
               "-DBUILD_ATOMSPACE_STORAGE=OFF"
               "-DBUILD_ATOMSPACE_EXTENSIONS=OFF"
               "-DBUILD_GNUCASH=OFF"
               "-DBUILD_KOBOLDCPP=OFF"
               "-DBUILD_APHRODITE=OFF")
        #:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'set-source-permissions
            (lambda _
              ;; Ensure all source files are readable and directories accessible
              (for-each (lambda (file)
                          (when (file-exists? file)
                            (chmod file #o644)))
                        (find-files "." ".*" #:directories? #f))
              (for-each (lambda (dir)
                          (when (file-exists? dir)
                            (chmod dir #o755)))
                        (find-files "." ".*" #:directories? #t))
              #t))
          (add-before 'configure 'prepare-build-environment
            (lambda* (#:key inputs outputs #:allow-other-keys)
              ;; Set up environment variables for the build
              (let ((boost (assoc-ref inputs "boost"))
                    (guile (assoc-ref inputs "guile"))
                    (pkg-config (assoc-ref inputs "pkg-config")))
                (when boost
                  (setenv "BOOST_ROOT" boost))
                (when guile
                  (setenv "GUILE_LOAD_PATH" 
                          (string-append guile "/share/guile/site/3.0")))
                (when pkg-config
                  (setenv "PKG_CONFIG_PATH"
                          (string-append pkg-config "/lib/pkgconfig"))))
              #t))
          (replace 'build
            (lambda* (#:key parallel-build? #:allow-other-keys)
              ;; Build with all available cores
              (let ((job-count (if parallel-build?
                                   (number->string (parallel-job-count))
                                   "1")))
                ;; Use invoke instead of system* for better error handling
                (invoke "make" (string-append "-j" job-count) "VERBOSE=1"))))
          (add-after 'install 'install-additional-components
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (lib (string-append out "/lib"))
                     (share (string-append out "/share/opencog-collection"))
                     (python (search-input-file inputs "/bin/python3")))
                (mkdir-p share)
                (mkdir-p bin)
                (mkdir-p lib)
                
                ;; Navigate back to source directory from build directory
                (chdir "..")
                
                ;; Install Python demonstration application if present
                (when (file-exists? "app.py")
                  (install-file "app.py" share)
                  (call-with-output-file (string-append bin "/opencog-demo")
                    (lambda (port)
                      (format port "#!/bin/sh~%exec ~a ~a/app.py \"$@\"~%"
                              python share)))
                  (chmod (string-append bin "/opencog-demo") #o755))
                
                ;; Install documentation files
                (when (file-exists? "README.md")
                  (install-file "README.md" share))
                (when (file-exists? "requirements.txt")
                  (install-file "requirements.txt" share))
                (when (file-exists? "LICENSE")
                  (install-file "LICENSE" share))
                
                ;; Optionally build and install Rust/Hyperon components
                ;; This is wrapped in a catch to make it non-fatal if Rust build fails
                (when (file-exists? "Cargo.toml")
                  (catch #t
                    (lambda ()
                      (format #t "Building Rust components...~%")
                      (setenv "CARGO_HOME" (string-append (getcwd) "/.cargo"))
                      (setenv "RUSTFLAGS" "-C opt-level=2")
                      ;; Try to build Rust components
                      (invoke "cargo" "build" "--release" "--workspace")
                      ;; Install any executable binaries that were built
                      (when (file-exists? "target/release")
                        (let ((release-files (find-files "target/release")))
                          (for-each
                            (lambda (file-path)
                              (let ((stat-info (stat file-path)))
                                (when (and (eq? 'regular (stat:type stat-info))
                                           (not (zero? (logand #o111 (stat:perms stat-info))))
                                           (not (string-suffix? ".so" file-path))
                                           (not (string-suffix? ".a" file-path))
                                           (not (string-suffix? ".d" file-path))
                                           (not (string-contains file-path "build-script"))
                                           (not (string-contains file-path ".dSYM"))
                                           (not (string-contains file-path "incremental")))
                                  (format #t "Installing Rust binary: ~a~%" file-path)
                                  (install-file file-path bin))))
                            release-files))))
                    (lambda (key . args)
                      (format #t "Note: Rust build skipped or failed (this is optional): ~a ~a~%" 
                              key args)
                      ;; Don't fail the build if Rust components can't be built
                      #t)))
                
                #t))))))
    (native-inputs
     (list pkg-config
           cmake-minimal
           gcc-toolchain))
    (inputs
     (list python
           python-numpy
           python-pandas
           python-scikit-learn
           python-matplotlib
           guile-3.0
           boost
           openblas
           lapack
           gsl
           cxxtest))
    (propagated-inputs
     (list python-numpy
           python-pandas
           python-scikit-learn
           python-matplotlib))
    (home-page "https://github.com/cogpy/occ")
    (synopsis "OpenCog Collection - Cognitive Computing and AGI Framework")
    (description
     "This package provides the OpenCog Collection (OCC) monorepo, an integrated
development environment for cognitive computing and artificial general intelligence
(AGI) research and development.

The collection brings together multiple OpenCog-related projects into a coherent
whole, enabling cognitive synergy through the integration of various AI and
cognitive computing approaches.

Core OpenCog Components:
@itemize
@item CogUtil - Base utilities and configuration system for OpenCog
@item AtomSpace - Hypergraph database and knowledge representation system
@item CogServer - Networking and inter-process communication layer
@item Matrix - Sparse matrix and graph processing for machine learning
@item Learn - Symbolic learning algorithms and pattern mining
@item Agents - Interactive cognitive agents framework
@item Sensory - Dataflow system for external world interaction
@end itemize

Advanced Cognitive Architecture:
@itemize
@item CogGML - Self-aware microkernel for cognitive processing
@item CogSelf - AGI cognitive synergy framework
@item AtomSpace Accelerator - High-performance inference engine
@item Agentic Chatbots - Conversational AI integration
@end itemize

Additional Features:
@itemize
@item Python-based machine learning demonstrations using scikit-learn
@item Rust-based Hyperon cognitive computing framework (optional)
@item Complete source code for research and development
@item Modular architecture for flexible deployment
@end itemize

This package is suitable for researchers, developers, and students working on
artificial general intelligence, cognitive computing, knowledge representation,
and related fields.")
    (license license:agpl3+)))

;; Return the package for building
opencog-collection
