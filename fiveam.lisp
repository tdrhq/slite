(defpackage :slite/fiveam
  (:use #:cl
        #:alexandria)
  (:import-from #:slite
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
