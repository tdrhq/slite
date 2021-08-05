(defpackage :slite
  (:use :cl
   :alexandria)
  (:export #:test-result
           #:on-pass
           #:*engine*
           #:engine
           #:remove-test
           #:rerun-in-debugger-impl
           #:run-all-fiveam-tests
           #:test-result-list
           #:test-case
           #:test-case-package
           #:test-name
           #:test-expression
           #:test-message
           #:test-result-success-p))
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

(defgeneric test-result-success-p (result))

(defgeneric test-name (test-case))

(defgeneric test-expression (result))

(defgeneric test-message (result))

(defgeneric test-case (test-result))

(defgeneric test-result-list (response))

(defmethod test-result-list ((response list))
  response)

(defun serialize-result (result)
  (list
   :expression (test-expression result)
   :success (test-result-success-p result)
   :reason (test-message result)))

(defgeneric test-case-package (result))

(defun ensure-safe-for-sl* (x)
  "If we send a bad object over the wire, SLY/SLIME can go into a bad
state, so let's get rid of it early"
  (typecase x
    (standard-object
     (error "Unsafe object for SL* wire: ~s" x))
    (atom t)
    (list
     (mapc #'ensure-safe-for-sl* x))
    (t
     (error "Unsafe object for SL* wire: ~S" x)))
  x)

(defmethod process-results (results)
  (ensure-safe-for-sl*
   (let ((framework (slite/api:guess-framework results))
         (results (test-result-list results))
         (test-case-map nil))
     (setf *last-results* results)
     (loop for result in results do
       (pushnew result (assoc-value test-case-map (test-case result))))
     (flet ((test-case-success-p (results)
              ;; we could do soooo much better
              (every 'test-result-success-p results)))
       (let ((case-result-map (stable-sort test-case-map #'string<
                                 ;; "nil" comes before "t"
                                         :key (lambda (x)
                                                (test-case-success-p (cdr x))) )))
       (loop for (test-case . results) in case-result-map
             collect
             (list
              :id
              (list
               :framework framework
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
                            (length (remove-if-not #'test-result-success-p results))
                            (length results))))))))))

(defmethod get-test-case-details (test-case)
  (let* ((results (loop for res in *last-results*
                        if (eql (test-case res) test-case)
                          collect res)))
    (assert test-case)
    (format nil "Test Detail: ~%~a in package ~a (~d checks): ~%"
            (test-name test-case)
            (let ((package (test-case-package test-case)))
              (when package
               (package-name package)))
            (length results))))


(defun on-pass (results &key shell)
  (when (every #'test-result-result-success-p results)
    (uiop:run-program (list "bash" "-c" shell)
                      :output *standard-output*
                      :error-output *error-output*))
  results)

(defmethod slite/api:rem-test :around (framework name package)
  (ensure-safe-for-sl* (call-next-method)))

(defmethod slite/api:rerun-in-debugger :around (framework name package)
  (ensure-safe-for-sl* (call-next-method)))
