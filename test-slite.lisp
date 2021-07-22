(defpackage :slite/tests
  (:use #:cl
        #:fiveam
        #:alexandria)
  (:import-from #:slite
                #:process-results)
  (:import-from #:fiveam
                #:test-case
                #:test-result))

(in-package :slite/tests)

(def-suite* :slite/tests)

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
