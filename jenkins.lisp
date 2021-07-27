(load #P "quicklisp.lisp")

(setf quicklisp-quickstart:*home* #P"./")
(quicklisp-quickstart:install)

(push #P "./" asdf:*central-registry*)

(ql:quickload :slite/tests)

(unless (fiveam:run-all-tests)
  (uiop:quit 1))

(uiop:quit 0)
