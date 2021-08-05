(defpackage #:slite/parachute
  (:use #:cl
        #:alexandria)
  (:import-from #:parachute
                #:test-result
                #:parent-result
                #:result)
  (:import-from #:slite
                #:test-case
                #:test-result-list))
(in-package #:slite/parachute)

(defclass fake-test-result ()
  ((test-case :accessor parent-test-result
              :initarg :test-case)
   (parachute-result :accessor parachute-result
                     :initarg :parachute-result))
  (:documentation "Simulates TEST-RESULT and TEST-CASE from FiveAM"))

(defmethod test-result-list ((result result))
  nil)

(defmethod test-case ((result fake-test-result))
  (parachute:expression (parent-test-result result)))

(defmethod test-result-list ((result test-result))
  (loop for x across (parachute:results result)
        unless (typep x 'test-result)
        collect (make-instance 'fake-test-result
                               :test-case result
                               :parachute-result x)))

(defmethod test-result-list ((result parent-result))
  (loop for x across (parachute:results result)
        appending (test-result-list x)))


(defmethod slite:test-result-success-p ((result fake-test-result))
  (ecase (parachute:status (parachute-result result))
    (:failed nil)
    (:passed t)))

(defmethod slite:test-case-package ((result parachute:test))
  (package-name (parachute:home result)))

(defmethod slite:test-name ((test parachute:test))
  (parachute:name test))

(defmethod slite:test-expression ((result fake-test-result))
  (format nil "~S"
          (parachute:expression (parachute-result result))))


(defmethod slite:test-message ((fake-result fake-test-result))
  (let ((result (parachute-result fake-result)))
   (or
    (parachute:description result)
    (format nil "Failed: ~S" (slite:test-expression fake-result)))))

(defun guess-parachute (result)
  (when (typep result 'parachute:result)
    :parachute))

(pushnew 'guess-parachute slite/api:*framework-guessors*)

(defmethod slite/api:rerun-in-debugger ((framework (eql :parachute))
                                        name
                                        package)
  (declare (ignore framework))
  ;; name could be either string or symbol, but it looks like
  ;; parachute:find-test can handle both correctly
  (parachute:test (parachute:find-test name package)
    :report 'parachute:interactive)
  (values))

(defmethod slite/api:rem-test ((framework (eql :parachute))
                               name
                               package)
  (parachute:remove-test
   (find-symbol name package)))
