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

(defmethod test-result-list ((result result))
  nil)

(defmethod test-result-list ((result test-result))
  (list result))

(defmethod test-result-list ((result parent-result))
  (loop for x across (parachute:results result)
        appending (test-result-list x)))


(defmethod test-case ((result test-result))
  (parachute:expression result))

(defmethod slite:test-result ((result test-result))
  (ecase (parachute:status result)
    (:failed nil)
    (:passed t)))

(defmethod slite:test-case-package ((result parachute:test))
  (package-name (parachute:home result)))

(defmethod slite:test-name ((test parachute:test))
  (parachute:name test))

(defmethod slite:test-expression ((result result))
  (format nil "~S"
          (parachute:expression result)))


(defmethod slite:test-message ((result result))
  (or
   (parachute:description result)
   (format nil "Failed: ~S" (slite:test-expression result))))
