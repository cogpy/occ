;;; Guix manifest for OpenCog Collection development environment
;;; Use with: guix shell -m .guix/manifest.scm

(use-modules (gnu)
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
             (gnu packages version-control)
             (gnu packages text-editors)
             (gnu packages base))

;; Development environment manifest with all necessary tools and dependencies
(specifications->manifest
 '(;; Build tools
   "cmake"
   "pkg-config"
   "rust"
   "rust:cargo"
   "gcc-toolchain"
   "make"
   
   ;; Python and scientific computing
   "python"
   "python-pip"
   "python-setuptools"
   "python-wheel"
   "python-numpy"
   "python-pandas"
   "python-scikit-learn"
   "python-matplotlib"
   "python-jupyter"
   "python-ipython"
   
   ;; Guile and Scheme development
   "guile"
   "guile-readline"
   "guile-json"
   
   ;; Core libraries often needed by OpenCog
   "boost"
   "protobuf"
   "gsl"
   "blas"
   "lapack"
   
   ;; Development utilities
   "git"
   "which"
   "grep"
   "sed"
   "coreutils"
   "findutils"
   
   ;; Optional: text editors for development
   "emacs-minimal"
   "vim"))