(defpackage :slite/lispworks
  (:use #:cl)
  (:import-from #:slite/api
                #:rem-test
                #:rerun-in-debugger)
  (:export #:run-tests
           #:display-test-results
           #:results-window))
(in-package :slite/lispworks)

;;; A LispWorks/CAPI client for slite. Unlike the Emacs client, this
;;; runs the tests in the current process and renders the results
;;; directly into a CAPI window, so there's no IPC involved.
;;;
;;; The result items handled here are exactly the plists returned by
;;; SLITE::PROCESS-RESULTS, so the framework-specific behaviour (FiveAM,
;;; Parachute, lisp-unit2) is shared with the Emacs client.

(defun item-id (item)
  (getf item :id))

(defun item-data (item)
  (getf item :data))

(defun item-passed-p (item)
  (equal "PASS" (first (item-data item))))

(defun format-one-line-reason (reason)
  "Collapse REASON to a single line, dropping the noisy \"Unexpected
Error: \" prefix that FiveAM adds."
  (let ((reason (remove #\Newline (or reason ""))))
    (if (and (>= (length reason) 18)
             (string= "Unexpected Error: " reason :end2 18))
        (subseq reason 18)
        reason)))

(defun item-reason (item)
  "The one-line failure reason for ITEM, or an empty string if it passed."
  (let ((id (item-id item)))
    (loop for result in (getf id :results)
          unless (getf result :success)
            do (return (format-one-line-reason (getf result :reason)))
          finally (return ""))))

(defun result-columns (item)
  (destructuring-bind (result name passed) (item-data item)
    (list result name passed (item-reason item))))

(defun result-color (pane item state)
  (declare (ignore pane state))
  (if (item-passed-p item)
      :darkgreen
      :red))

;; * The results window

(capi:define-interface results-window ()
  ((results-thunk :initarg :results-thunk
                  :initform nil
                  :accessor results-thunk
                  :documentation "A function of no arguments returning raw
test results (e.g. from FIVEAM:RUN). Used to rerun all tests."))
  (:panes
   (results-list capi:multi-column-list-panel
                 :reader results-list
                 :columns '((:title "Result" :adjust :left)
                            (:title "Name" :adjust :left)
                            (:title "Passed" :adjust :left)
                            (:title "Reason" :adjust :left))
                 :column-function 'result-columns
                 :color-function 'result-color
                 :visible-min-width 700
                 :visible-min-height 300
                 :action-callback 'describe-result-callback
                 :callback-type :item-interface))
   (:layouts
    (main-layout capi:column-layout '(results-list)))
   (:menu-bar test-menu)
   (:menus
    (test-menu "Tests"
               (("Details" :callback 'describe-result-callback
                           :callback-type :interface
                           :accelerator #\Return)
                ("Rerun in Debugger" :callback 'rerun-in-debugger-callback
                                     :callback-type :interface
                                     :accelerator "r")
                ("Rerun All" :callback 'rerun-all-callback
                             :callback-type :interface
                             :accelerator "g")
                ("Delete Test" :callback 'delete-test-callback
                               :callback-type :interface
                               :accelerator "Delete"))))
   (:default-initargs
    :title "CL Test Results"))

(defun selected-item (interface)
  (capi:choice-selected-item (results-list interface)))

(defmethod set-items ((self results-window) results)
  (setf-title self results)
  (setf (capi:collection-items (results-list self)) results))

(defun setf-title (self results)
  (let ((failures (count-if-not #'item-passed-p results)))
    (setf (capi:interface-title self)
          (if (zerop failures)
              (format nil "CL Test Results: all ~d passed" (length results))
              (format nil "CL Test Results: ~d of ~d failed"
                      failures (length results))))))

;; * Callbacks

(defun describe-result-callback (item interface)
  "Open a details window for ITEM (or the selected item)."
  (let ((item (or item (selected-item interface))))
    (when item
      (capi:display (make-instance 'details-window :item item)))))

(defun rerun-in-debugger-callback (interface)
  (let ((item (selected-item interface)))
    (when item
      (let* ((id (item-id item))
             (framework (getf id :framework))
             (name (getf id :test-name))
             (package (getf id :package)))
        (rerun-in-debugger framework name package)))))

(defun delete-test-callback (interface)
  (let ((item (selected-item interface)))
    (when item
      (let* ((id (item-id item))
             (framework (getf id :framework))
             (name (getf id :test-name))
             (package (getf id :package)))
        (when (capi:confirm-yes-or-no
               "Delete the test ~a in package ~a?" name package)
          (rem-test framework name package)
          (setf (capi:collection-items (results-list interface))
                (remove item (capi:collection-items (results-list interface)))))))))

(defun rerun-all-callback (interface)
  (let ((thunk (results-thunk interface)))
    (cond
      (thunk
       (set-items interface (slite::process-results (funcall thunk))))
      (t
       (capi:display-message "No test expression is associated with this window")))))

;; * The details window

(capi:define-interface details-window ()
  ((item :initarg :item :reader item))
  (:panes
   (text-pane capi:editor-pane
              :reader text-pane
              :visible-min-width 600
              :visible-min-height 400
              :buffer-name :temp
              :enabled :read-only))
  (:layouts
   (main-layout capi:column-layout '(text-pane)))
  (:default-initargs
   :title "Test Case Details"))

(defmethod initialize-instance :after ((self details-window) &key &allow-other-keys)
  (setf (capi:editor-pane-text (text-pane self))
        (details-text (item self))))

(defun details-text (item)
  (with-output-to-string (out)
    (let ((id (item-id item)))
      (write-string (getf id :details) out)
      (terpri out)
      (dolist (result (getf id :results))
        (format out "~a ~a~%~%"
                (if (getf result :success) "PASS" "FAIL")
                (getf result :expression))
        (unless (getf result :success)
          (format out "------------------~%~a~%------------------~%~%"
                  (getf result :reason)))))))

;; * Entry points

(defun display-test-results (results-thunk &key (title "CL Test Results"))
  "Run RESULTS-THUNK, a function of no arguments returning raw test
results (e.g. (lambda () (fiveam:run :my-suite))), and display the
results in a new CAPI window."
  (let* ((results (slite::process-results (funcall results-thunk)))
         (window (make-instance 'results-window
                                :results-thunk results-thunk
                                :title title)))
    (set-items window results)
    (capi:display window)
    window))

(defmacro run-tests (&body body)
  "Evaluate BODY (which should return raw test results, e.g.
  (fiveam:run :my-suite)) and display the results in a CAPI window. BODY
  is re-evaluated when the user chooses \"Rerun All\"."
  `(display-test-results (lambda () ,@body)))
