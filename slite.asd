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
               (:file "fiveam"))
  :in-order-to ((test-op (test-op "slite/tests"))))

(defsystem :slite/parachute
  :serial t
  :depends-on (:slite
               :parachute)
  :components ((:file "parachute")))

(defsystem :slite/lisp-unit2
  :serial t
  :depends-on (:slite
               :lisp-unit2)
  :components ((:file "lisp-unit2")))

(defsystem :slite/tests
  :serial t
  :depends-on (:slite
               :slite/parachute
               :slite/lisp-unit2
               :parachute)
  :components ((:file "test-slite")
               (:file "test-parachute")
               (:file "test-lisp-unit2"))
  :perform (test-op (o c)
                    (unless
                        (symbol-call '#:fiveam '#:run!
                                     :slite/tests)
                      (error "Some tests were failed!"))))
