;; -*- coding: utf-8 -*-
(defpackage #:slite/test-parachute
  (:use #:cl
        #:alexandria
        #:fiveam)
  (:import-from #:slite
                #:test-case
                #:test-result-list)
  (:import-from #:parachute
                #:find-test)
  (:import-from #:slite/parachute
                #:fake-test-result))
(in-package #:slite/test-parachute)

(def-suite* :slite/test-parachute :in :slite)

(parachute:define-test my-suite)

(parachute:define-test foo-bar-2
  :parent my-suite
  (parachute:is = 3 3)
  (parachute:is = 2 4
                "Expected 2 to be 4"))

(parachute:define-test foo-bar-1
  :parent my-suite
  (parachute:is = 5 5))

(parachute:define-test unused)

(parachute:define-test "blah blah"
  :parent unused
  (parachute:is = 5 5))

(def-fixture state ()
  (let ((results (test-result-list (parachute:test 'my-suite
                                     :output (null-stream)))))
    (&body)))

(test preconditions
  (with-fixture state ()
    (is (listp results))
    (is (eql 3 (length results))
        "There should one test-result for foo-bar-1, and two for foo-bar-2")
    (loop for x in results
          do (is (typep x 'fake-test-result)))))

(test find-test
  (with-fixture state ()
   (let ((foo-bar-2 (find-test 'foo-bar-2)))
     (is (not (null foo-bar-2)))
     (let ((test-cases (mapcar 'test-case results)))
       (is (member foo-bar-2
                   test-cases)
           "Expected to see ~S in ~S"
           foo-bar-2
           test-cases)))))

(test test-result
  (with-fixture state ()
    (let ((statuses (mapcar #'slite:test-result-success-p
                            results)))
      (loop for x in statuses
            do
            (is (typep x 'boolean)))
      (is (eql 2
               (count t statuses))))))

(defun only-test-result (result)
  (elt (parachute:results result) 0))

(test test-case-package
  (let ((result (make-instance 'fake-test-result
                               :test-case
                               (only-test-result (parachute:test 'foo-bar-1
                                     :output (null-stream))))))
    (is (equal "SLITE/TEST-PARACHUTE"
              (slite:test-case-package
               (slite:test-case result))))))

(test test-name
  (let ((result (make-instance 'fake-test-result
                               :test-case
                               (only-test-result (parachute:test 'foo-bar-1
                                                   :output (null-stream))))))
    (is (equal 'foo-bar-1
              (slite:test-name
               (slite:test-case result))))))


(test test-expression
  (let* ((result (only-test-result (parachute:test 'foo-bar-1
                                     :output (null-stream))))
         (result (elt (parachute:results result) 0))
         (result (make-instance 'fake-test-result
                                :parachute-result result)))
    (is (equal (format nil "~S" '(parachute:is = 5 5))
               (slite:test-expression result)))))

(defun null-stream ()
  "This improves the logging. But also, on LW, this causes UTF-8 issues for me"
  (make-string-output-stream))

(test test-description
  (let* ((result (only-test-result (parachute:test 'foo-bar-2
                                     :output (null-stream))))
         (result (elt (parachute:results result) 1))
         (result (make-instance 'fake-test-result
                                :parachute-result result)))
    (assert (equal "Expected 2 to be 4"
                   (slite:test-message result))))  )

(test rerun-tests-in-debugger
  (slite/api:rerun-in-debugger :parachute
                               "FOO-BAR-1"
                               "SLITE/TEST-PARACHUTE"))

(test rerun-tests-in-debugger-failing
  (signals simple-error
   (slite/api:rerun-in-debugger :parachute
                                "FOO-BAR-2"
                                "SLITE/TEST-PARACHUTE")))

(def-fixture empty-parachute ()
  (let ((parachute::*test-indexes* (make-hash-table)))
    (&body)))

(test parachute-test-with-strings
  (with-fixture empty-parachute ()
    (parachute:define-test my-suite)
    (is (equal
         nil
         (test-result-list (parachute:test 'my-suite))))
    (parachute:define-test "foo-bar"
      :parent my-suite
      (parachute:is = 5 5))
    (let ((results (test-result-list (parachute:test 'my-suite
                                       :output (null-stream)))))
      (is (equal
           1
           (length results)))
      (finishes
       (slite::process-results results)))))
