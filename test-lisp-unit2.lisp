;; -*- coding: utf-8 -*-
(defpackage :slite/test-lisp-unit2
  (:use #:cl
        #:fiveam)
  (:import-from #:slite
                #:test-result-list)
  (:local-nicknames (#:a #:alexandria)
                    (#:unit #:lisp-unit2)))
(in-package :slite/test-lisp-unit2)

(def-suite* :slite/test-lisp-uni2 :in :slite)

(unit:define-test simple-test
    (:tags '(bar))
  (unit:assert-eql 1 (- 2 1))
  (unit:assert-eql 1 1))

(unit:define-test another-test2
  (:tags '(bar))
  (unit:assert-eql 2 4))

;;(unit:run-tests :tags 'bar)

(def-fixture state ()
  (let ((results (test-result-list (unit:run-tests :tags 'bar))))
   (&body)))

(test preconditions
  (with-fixture state ()
    (is (listp results))
    (is (eql 2 (length results)))
    (loop for x in results
          do (is (typep x 'unit:test-result)))))
