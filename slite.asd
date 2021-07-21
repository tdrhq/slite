(defsystem :slite
  :serial t
  :author "Arnold Noronha <arnold@jipr.io>"
  :license  "Apache License, Version 2.0"
  :version "0.0.1"
  :description "SLIME based Test-runner for FiveAM tests (and possibly others in the future)"
  :depends-on (:str
               :fiveam
               :bordeaux-threads
               :cl-mongo-id
               :trivial-garbage)
  :components ((:file "slite")))

(defsystem :slite/tests
  :serial t
  :depends-on (:slite))
