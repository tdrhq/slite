(defpackage :slite/fiveam
  (:use #:cl
        #:alexandria)
  (:import-from #:slite
                #:rerun-in-debugger-impl
                #:remove-test
                #:*engine*
                #:engine
                #:test-result
                #:test-name
                #:test-expression
                #:test-message
                #:test-case
                #:test-case-package))
(in-package :slite/fiveam)

(defmethod test-result ((result fiveam::test-passed))
  t)

(defmethod test-result ((result fiveam::test-result))
  nil)

(defmethod test-name ((test-case fiveam::test-case))
  (fiveam::name test-case))

(defmethod test-expression ((result fiveam::test-result))
  ;; To keep the expression concise, let's switch to the package
  ;; before rendering it to a string
  (let ((*package* (test-case-package (test-case result))))
   (format nil "~s" (fiveam::test-expr result))))

(defmethod test-message ((result fiveam::test-result))
  (fiveam::reason result))

(defmethod test-case ((result fiveam::test-result))
  (fiveam::test-case result))

(defmethod test-case-package ((test-case fiveam::test-case))
  (symbol-package (test-name test-case)))


(defclass fiveam-engine (engine)
  ())

(unless *engine*
  (setf *engine* (make-instance 'fiveam-engine)))

(defmethod remove-test ((engine fiveam-engine) name package)
  (declare (ignore engine))
  (cond
    (package
     (fiveam:rem-test (find-symbol name package)))
    (t
     ;; We're most likely looking at an uninterned symbol, like #:foo
     ;; Our best bet is to walk through all the tests and remove all
     ;; tests with the same name but uninterned package.
     (loop for existing being the hash-keys of fiveam::*test*
           if (and
               (string= name existing)
               (not (symbol-package existing)))
             do
                (fiveam:rem-test existing)))))

(defmethod rerun-in-debugger-impl ((engine fiveam-engine) name package)
  (declare (ignore engine))
    (let ((sym (find-symbol name package)))
    (let ((fiveam:*on-error* :debug)
          (fiveam:*on-failure* :debug))
      (let ((result (fiveam:run sym)))
        (cond
          ((every #'test-result result)
           "PASS")
          (t
           "FAIL"))))))

;; modified from fiveam:run-all-tests
(defmethod slite:run-all-fiveam-tests ()
  (loop for suite in (cons nil (sort (copy-list fiveam::*toplevel-suites*) #'string<=))
        for results = (if (fiveam::suite-emptyp suite) nil (fiveam::run suite))
        appending results))
