(defpackage :slite
  (:use :cl
   :alexandria)
  (:export #:test-result
           #:on-pass
           #:*engine*
           #:engine
           #:remove-test
           #:rerun-in-debugger-impl
           #:run-all-fiveam-tests))
(in-package :slite)

(defvar *engine* nil)

(defclass engine ()
  ()
  (:documentation "An abstraction over the test framework to integrate with slite"))

(defgeneric remove-test (engine name package))

(defgeneric rerun-in-debugger-impl (engine name package))

(defvar *last-results* nil
  "Stores the last test result. We'll always store this just before
  rendering so that we can run actions on this, and the test results
  aren't garbage collected in the meantime")

(defgeneric test-result (result))

(defgeneric test-name (result))

(defgeneric test-expression (result))

(defgeneric test-message (result))

(defgeneric test-case (test-result))

(defun serialize-result (result)
  (list
   :expression (test-expression result)
   :success (test-result result)
   :reason (test-message result)))

(defgeneric test-case-package (result))

(defmethod process-results (results)
  (setf *last-results* results)
  (let ((test-case-map nil))
    (loop for result in results do
      (pushnew result (assoc-value test-case-map (test-case result))))
    (flet ((test-case-success-p (results)
             ;; we could do soooo much better
             (every 'test-result results)))
     (let ((case-result-map (stable-sort test-case-map #'string<
                                 ;; "nil" comes before "t"
                                         :key (lambda (x)
                                                (test-case-success-p (cdr x))) )))
       (loop for (test-case . results) in case-result-map
             collect
             (list
              :id
              (list
               :package (let ((package (test-case-package test-case)))
                          (when package
                           (package-name package)))
               :details
               (get-test-case-details test-case)
               :test-name (string (test-name test-case))
               :results
               (mapcar #'serialize-result results))
              :data
              (list (if (test-case-success-p results)
                        "PASS" "FAIL")
                    (string (test-name test-case))
                    (format nil "~a/~a"
                            (length (remove-if-not #'test-result results))
                            (length results)))))))))

(defmethod get-test-case-details (test-case)
  (let* ((results (loop for res in *last-results*
                        if (eql (test-case res) test-case)
                          collect res)))
    (assert test-case)
    (format nil "Test Detail: ~%~a in package ~a (~d checks): ~%"
            (test-name test-case)
            (let ((package (symbol-package (test-name test-case))))
              (when package
               (package-name package)))
            (length results))))

(defun rem-test (name package)
  (remove-test *engine* name package))

(defun rerun-in-debugger (name package)
  (rerun-in-debugger-impl *engine* name package))


(defun on-pass (results &key shell)
  (when (every #'test-result results)
    (uiop:run-program (list "bash" "-c" shell)
                      :output *standard-output*
                      :error-output *error-output*))
  results)
