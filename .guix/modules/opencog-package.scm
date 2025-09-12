;;; OpenCog Collection (OCC) package definition for GNU Guix

(define-module (opencog-package)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages version-control))

(define-public opencog-collection
  (package
    (name "opencog-collection")
    (version "0.1.0")
    (source (local-file "." "opencog-collection-checkout"
                        #:recursive? #t))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Skip configure and build phases for Python since this is mainly a collection
         (delete 'configure)
         (replace 'build
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Build the Rust Hyperon component
             (with-directory-excursion "."
               (invoke "cargo" "build" "--release"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (python-sitedir (string-append (assoc-ref outputs "out")
                                                 "/lib/python"
                                                 (python-version (assoc-ref %build-inputs "python"))
                                                 "/site-packages")))
               ;; Install Python application
               (copy-recursively "." (string-append out "/share/opencog-collection"))
               ;; Install Python entry point
               (install-file "app.py" (string-append out "/bin"))
               (chmod (string-append out "/bin/app.py") #o755)
               ;; Install Rust binary and library if built
               (when (file-exists? "target/release/hyperon")
                 (install-file "target/release/hyperon" (string-append out "/bin")))
               (when (file-exists? "target/release/libhyperon.so")
                 (install-file "target/release/libhyperon.so" (string-append out "/lib")))
               #t)))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; Run basic Python test
               (invoke "python" "app.py"))
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
Python and Rust interfaces for cognitive computing applications.")
    (license license:mit)))

;; Export the package for use in other modules
opencog-collection