(require "asdf")

(setf (uiop:getenv "HOME") "/cache-dir/")

(defvar *cwd* "/cache-dir/")

(load "/cache-dir/quicklisp/setup.lisp")


(asdf:initialize-output-translations `(:output-translations
                                       :inherit-configuration
                                       (,(namestring *cwd*)
                                         ,(format nil "~abuild/asdf-cache/" *cwd*))))

(push #P "./" asdf:*central-registry*)

(ql:quickload :slite/tests)

(unless (fiveam:run-all-tests)
  (uiop:quit 1))

(uiop:quit 0)
