(defpackage :slite/api
  (:use :cl)
  (:export
   #:*framework-guessors*
   #:guess-framework
   #:rem-test
   #:rerun-in-debugger))
(in-package :slite/api)

(defvar *framework-guessors* nil)

(defun guess-framework (result)
  (loop for guessor in *framework-guessors*
        for response = (funcall guessor result)
        if response
          return response))

(defgeneric rem-test (framework name package))

(defgeneric rerun-in-debugger (framework name package))
