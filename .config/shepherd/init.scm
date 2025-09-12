(service
 '(opencog-build)
 #:start (make-forkexec-constructor '("guix" "build" "./packaging/opencog.scm"))
 #:stop (make-kill-destructor))