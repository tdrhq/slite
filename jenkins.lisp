(require "asdf")

(defvar *cwd* (uiop:getcwd))

(asdf:initialize-output-translations `(:output-translations
                                       :inherit-configuration
                                       (,(namestring *cwd*)
                                         ,(format nil "~abuild/asdf-cache/" *cwd*))))

(push #P"third-party/deadbeef/" asdf:*central-registry*)

(asdf:load-system :deadbeef)

(deadbeef:register-external "https://github.com/vindarel/cl-str.git"
                            "master")
(deadbeef:register-external "https://github.com/sionescu/fiveam.git"
                            "master")
(deadbeef:register-external "https://github.com/edicl/cl-ppcre"
                            "master")
(deadbeef:register-external "https://github.com/edicl/cl-unicode"
                            "master")
(deadbeef:register-external "https://github.com/edicl/flexi-streams"
                            "master")
(deadbeef:register-external "https://github.com/trivial-gray-streams/trivial-gray-streams"
                            "master")

(deadbeef:register-external "https://github.com/rudolfochrist/cl-change-case"
                            "master")
(deadbeef:register-external "https://github.com/didierverna/asdf-flv"
                            "master")

(deadbeef:register-external "https://gitlab.common-lisp.net/alexandria/alexandria.git"
                            "master")

(deadbeef:prepare-externals "build/deadbeef/")

(push #P "./" asdf:*central-registry*)

(asdf:load-system :slite/tests)

(unless (fiveam:run-all-tests)
  (uiop:quit 1))

(uiop:quit 0)
