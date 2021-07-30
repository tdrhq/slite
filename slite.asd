(in-package :asdf-user)

(defsystem :slite
  :serial t
  :author "Arnold Noronha <arnold@jipr.io>"
  :license  "Apache License, Version 2.0"
  :version "0.0.1"
  :description "SLIME based Test-runner for FiveAM tests (and possibly others in the future)"
  :depends-on (:str
               :fiveam)
  :components ((:file "api")
               (:file "slite")
               (:file "fiveam")))

(defsystem :slite/parachute
  :serial t
  :depends-on (:slite
               :parachute)
  :components ((:file "parachute")))

(defsystem :slite/tests
  :serial t
  :depends-on (:slite
               :slite/parachute
               :parachute)
  :components ((:file "test-slite")
               (:file "test-parachute")))
