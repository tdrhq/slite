(require "asdf")

(defvar *cwd* (uiop:getcwd))

(asdf:initialize-output-translations `(:output-translations
                                       :inherit-configuration
                                       (,(namestring *cwd*)
                                         ,(format nil "~abuild/asdf-cache/" *cwd*))))

(load "/quicklisp.lisp")

(quicklisp-quickstart:install)

(push #P "./" asdf:*central-registry*)

(ql:quickload :slite/tests)

(unless (fiveam:run-all-tests)
  (uiop:quit 1))

(uiop:quit 0)
