(defpackage :slite/tests
  (:use #:cl
        #:fiveam
        #:alexandria)
  (:import-from #:slite
                #:ensure-safe-for-sl*
                #:process-results)
  (:import-from #:fiveam
                #:test-case
                #:test-result))

(in-package :slite/tests)

(def-suite :slite)

(def-suite* :slite/tests :in :slite)

(test process-results-happy-path
  (let* ((test-case (make-instance 'test-case
                                   :name 'foobar))
         (result1 (make-instance 'test-result
                                 :test-expr '(+ 1 2)
                                 :test-case test-case))
         (result2 (make-instance 'test-result
                                 :test-expr '(+ 1 2)
                                 :test-case test-case)))
    (let ((res (process-results (list result1  result2))))
      (is (eql 1 (length res)))
      (let ((first (car res)))
        (is (equal "FOOBAR" (getf (getf first :id) :test-name))))))
  (pass))

(test demo-test
  (is (eql 3 (+  1 2)))
  (is (eql 4 (+ 2 2))))

(defclass dummy () ())

(test ensure-safe-for-sl*
  (is (equal "foo" (ensure-safe-for-sl* "foo")))
  (signals error
    (ensure-safe-for-sl* (make-instance 'dummy)))
  (is (equal 8 (ensure-safe-for-sl* 8)))
  (is (equal 'foo (ensure-safe-for-sl* 'foo)))
  (is (equal (list 8 "foo") (ensure-safe-for-sl* (list 8 "foo"))))
  (signals error
    (ensure-safe-for-sl* (list "foo" (make-instance 'dummy)))))
