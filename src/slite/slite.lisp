(defpackage :slite
  (:use :cl
   :alexandria)
  (:export #:test-result
           #:run-all-fiveam-tests))
(in-package :slite)

(defvar *last-results* nil
  "Stores the last test result. We'll always store this just before
  rendering so that we can run actions on this, and the test results
  aren't garbage collected in the meantime")

(defvar *object-to-id* (trivial-garbage:make-weak-hash-table :weakness :key))
(defvar *id-to-obj* (trivial-garbage:make-weak-hash-table :weakness :value :test 'equal))

(defvar *lock* (bt:make-lock))

(defun get-object-id (obj)
  "Don't use yet. Broken."
  (bt:with-lock-held (*lock*)
    (or
     (gethash obj *object-to-id*)
     (let ((oid (mongoid:oid-str (mongoid:oid))))
       (setf (Gethash obj *object-to-id*) oid)
       (setf (gethash oid *id-to-obj*)  obj)
       oid))))

(defun id-to-object (id)
  (let ((obj (gethash id *id-to-obj*)))
    (unless obj
      (error "No such object exists"))
    obj))

(defmethod test-result ((result fiveam::test-passed))
  t)

(defmethod test-result ((result fiveam::test-result))
  nil)

(defmethod test-name ((test-case fiveam::test-case))
  (fiveam::name test-case))

(defmethod test-expression ((result fiveam::test-result))
  (format nil "~s" (fiveam::test-expr result)))

(defmethod test-message ((result fiveam::test-result))
  (fiveam::reason result))

(defmethod test-case ((result fiveam::test-result))
  (fiveam::test-case result))

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
               :oid
               (get-object-id test-case)
               :details
               (get-test-case-details test-case))
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
    (format nil "Test Detail: ~%~a in package ~a (~d checks): ~%~a"
            (test-name test-case)
            (package-name (symbol-package (test-name test-case)))
            (length results)
            (str:join #\Newline
                      (loop for result in (stable-sort results #'string<
                                                       :key #'test-result)
                            collect
                            (format nil "~s" result))))))

;; modified from fiveam:run-all-tests
(defmethod run-all-fiveam-tests ()
  (loop for suite in (cons nil (sort (copy-list fiveam::*toplevel-suites*) #'string<=))
        for results = (if (fiveam::suite-emptyp suite) nil (fiveam::run suite))
        appending results))
