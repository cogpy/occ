;;; Simplified Guix package file for OpenCog Collection
;;; Use with: guix build -f guix-simple.scm

(use-modules (guix packages)
             (guix gexp)
             (guix build-system python)
             ((guix licenses) #:prefix license:)
             (gnu packages python)
             (gnu packages python-xyz)
             (gnu packages python-science)
             (gnu packages machine-learning))

(define-public opencog-collection
  (package
    (name "opencog-collection")
    (version "0.1.0")
    (source (local-file "." "opencog-collection-checkout"
                        #:recursive? #t))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f  ; Skip tests for now
       #:phases
       (modify-phases %standard-phases
         (delete 'build)  ; No build step needed for demo
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (share (string-append out "/share/opencog-collection")))
               (mkdir-p bin)
               (mkdir-p share)
               ;; Install the demo app
               (copy-file "app.py" (string-append share "/app.py"))
               ;; Create executable wrapper
               (call-with-output-file (string-append bin "/opencog-demo")
                 (lambda (port)
                   (format port "#!/bin/sh~%exec python3 ~a/app.py \"$@\"~%" share)))
               (chmod (string-append bin "/opencog-demo") #o755)
               #t))))))
    (inputs
     (list python
           python-numpy
           python-pandas
           python-scikit-learn
           python-matplotlib))
    (home-page "https://github.com/rzonedevops/occ")
    (synopsis "OpenCog Collection - Cognitive Computing Research Environment")
    (description
     "This package provides the OpenCog Collection monorepo - a research
environment for cognitive computing and artificial general intelligence (AGI).
Includes a Python machine learning demonstration and the complete OpenCog
source code for development and research.")
    (license license:gpl3+)))

;; Return the package for building
opencog-collection
