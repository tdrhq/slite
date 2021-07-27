(load "/root/quicklisp/setup.lisp")

(ql:update-all-dists)

(push #P "./" asdf:*central-registry*)

(ql:quickload :slite/tests)

(unless (fiveam:run-all-tests)
  (uiop:quit 1))

(uiop:quit 0)
