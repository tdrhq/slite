(defpackage :slite/lisp-unit2
  (:use #:cl)
  (:import-from #:slite
                #:test-message
                #:test-expression
                #:test-name
                #:test-case-package
                #:test-result-success-p
                #:test-case
                #:test-result-list)
  (:local-nicknames (#:a #:alexandria)
                    (#:unit #:lisp-unit2)))
(in-package :slite/lisp-unit2)

(defclass fake-test-result ()
  ((test-case :initarg :test-case
              :reader test-case)
   (successp :initarg :successp
             :reader test-result-success-p)
   (expr :initarg :expr
         :reader %test-expression)))

(defmethod print-object ((self fake-test-result) out)
  (with-slots (expr successp) self
    (format out "#<FAKE-TEST-RESULT success:~a expr:~a>" successp expr)))

(defmethod test-result-list ((result-db unit:test-results-db))
  (loop for test-result across (unit:results result-db)
        appending
        (flet ((make-result (successp expr)
           (make-instance 'fake-test-result
                          :test-case
                          (unit:unit-test test-result)
                          :expr expr
                          :successp successp)))
         (append
          (mapcar (a:curry #'make-result nil) (unit::head (unit:errors test-result)))
          (mapcar (a:curry #'make-result nil) (unit::head (unit:failed test-result)))
          (mapcar (a:curry #'make-result t)
                  (loop for x in
                              (unit::head (unit:passed test-result))
                        collect x))))))

(defmethod test-case ((result unit:test-result))
  (unit:unit-test result))


(defmethod test-case-package ((test-case unit:unit-test))
  (unit::eval-package test-case))

(defmethod test-name ((test-case unit:unit-test))
  (unit::name test-case))

(defmethod test-message ((test-result fake-test-result))
  (format nil "Failed: ~a" (test-expression test-result)))

(defmethod test-expression ((test-result fake-test-result))
  (format nil "~a"
   (let ((expr (%test-expression test-result)))
     (cond
       ((listp expr)
        expr)
       (t
        (unit::form expr))))))
