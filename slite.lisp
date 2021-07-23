(defpackage :slite
  (:use :cl
   :alexandria)
  (:export #:test-result
           #:run-all-fiveam-tests
           #:on-pass))
(in-package :slite)

(defvar *last-results* nil
  "Stores the last test result. We'll always store this just before
  rendering so that we can run actions on this, and the test results
  aren't garbage collected in the meantime")

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

(defun serialize-result (result)
  (list
   :expression (test-expression result)
   :success (test-result result)
   :reason (test-message result)))

(defmethod test-case-package ((test-case fiveam::test-case))
  (symbol-package (test-name test-case)))

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
               :package (package-name (test-case-package test-case))
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
            (package-name (symbol-package (test-name test-case)))
            (length results))))

;; modified from fiveam:run-all-tests
(defmethod run-all-fiveam-tests ()
  (loop for suite in (cons nil (sort (copy-list fiveam::*toplevel-suites*) #'string<=))
        for results = (if (fiveam::suite-emptyp suite) nil (fiveam::run suite))
        appending results))

(defun rerun-in-debugger (name package)
  (let ((sym (find-symbol name package)))
    (let ((fiveam:*on-error* :debug)
          (fiveam:*on-failure* :debug))
      (let ((result (fiveam:run sym)))
        (cond
          ((every #'test-result result)
           "PASS")
          (t
           "FAIL"))))))


(defun on-pass (results &key shell)
  (when (every #'test-result results)
    (uiop:run-program (list "bash" "-c" shell)
                      :output *standard-output*
                      :error-output *error-output*))
  results)
