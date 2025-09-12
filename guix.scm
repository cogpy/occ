;;; Top-level Guix package file for OpenCog Collection
;;; Use with: guix build -f guix.scm

(use-modules (guix packages)
             (guix download)
             (guix git-download)
             (guix build-system python)
             (guix build-system cargo)
             (guix build-system cmake)
             (guix build-system gnu)
             (guix build-system trivial)
             ((guix licenses) #:prefix license:)
             (gnu packages)
             (gnu packages python)
             (gnu packages python-xyz)
             (gnu packages python-science)
             (gnu packages rust)
             (gnu packages crates-io)
             (gnu packages cmake)
             (gnu packages pkg-config)
             (gnu packages guile)
             (gnu packages boost)
             (gnu packages serialization)
             (gnu packages databases)
             (gnu packages version-control))

(define-public opencog-collection
  (package
    (name "opencog-collection")
    (version "0.1.0")
    (source (local-file "." "opencog-collection-checkout"
                        #:recursive? #t))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f  ; Disable tests for now as they may require network access
       #:phases
       (modify-phases %standard-phases
         ;; Skip configure phase as this is primarily a Python application
         (delete 'configure)
         (replace 'build
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Build the Rust Hyperon component if Cargo.toml exists
             (when (file-exists? "Cargo.toml")
               (setenv "CARGO_HOME" (string-append (getcwd) "/.cargo"))
               (invoke "cargo" "build" "--release"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (share (string-append out "/share/opencog-collection"))
                    (python-sitedir (string-append out
                                                  "/lib/python"
                                                  (python-version (assoc-ref %build-inputs "python"))
                                                  "/site-packages")))
               ;; Create directories
               (mkdir-p bin)
               (mkdir-p share)
               (mkdir-p python-sitedir)
               
               ;; Install the complete source tree to share directory
               (copy-recursively "." share
                                #:select? (lambda (file stat)
                                           (not (or (string-contains file "/.git")
                                                   (string-contains file "/target")
                                                   (string-contains file "/.cargo")
                                                   (string-contains file "/__pycache__")))))
               
               ;; Create a wrapper script for the Python application
               (call-with-output-file (string-append bin "/opencog-collection")
                 (lambda (port)
                   (format port "#!/bin/sh
export PYTHONPATH=~a:$PYTHONPATH
exec ~a ~a/app.py \"$@\"~%"
                           python-sitedir
                           (which "python3")
                           share)))
               (chmod (string-append bin "/opencog-collection") #o755)
               
               ;; Install Python dependencies to site-packages
               (copy-file "app.py" (string-append python-sitedir "/opencog_collection.py"))
               
               ;; Install Rust binary and library if they were built
               (when (file-exists? "target/release/hyperon")
                 (install-file "target/release/hyperon" bin))
               (when (file-exists? "target/release/libhyperon.so")
                 (install-file "target/release/libhyperon.so" (string-append out "/lib")))
               #t)))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; Basic smoke test - try to import numpy and run a simple operation
               (invoke "python3" "-c" "import numpy as np; print('NumPy version:', np.__version__)"))
             #t)))))
    (native-inputs
     (list pkg-config
           cmake
           rust
           `(,rust "cargo")))
    (inputs
     (list python
           python-numpy
           python-pandas
           python-scikit-learn
           python-matplotlib
           guile-3.0
           boost))
    (propagated-inputs
     (list python-numpy
           python-pandas
           python-scikit-learn
           python-matplotlib))
    (home-page "https://github.com/rzonedevops/occ")
    (synopsis "OpenCog Collection - Machine Learning Integration Environment")
    (description
     "This package provides a development environment for the integration of
machine learning into Redox OS using Python, Rust, Prolog, and C.  It includes
the OpenCog Hyperon system and various machine learning tools and libraries.
The collection contains multiple OpenCog-related projects and provides both
Python and Rust interfaces for cognitive computing applications.

The package includes:
@itemize
@item A Python-based machine learning demonstration using scikit-learn
@item Rust-based Hyperon cognitive computing framework
@item Multiple OpenCog subprojects and tools
@item Development environment setup for cognitive computing research
@end itemize")
    (license license:mit)))

;; Return the package for building
opencog-collection