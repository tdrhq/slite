(defpackage :slite/lispworks
  (:use #:cl)
  (:import-from #:slite/api
                #:rem-test
                #:rerun-in-debugger)
  (:export #:run-tests
           #:display-test-results
           #:run-test-expression
           #:enable-editor-bindings
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
                ("Jump to Test" :callback 'jump-to-test-callback
                                :callback-type :interface
                                :accelerator "j")
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

(defun test-symbol (item)
  "The interned symbol naming ITEM's test, or NIL for an uninterned test."
  (let* ((id (item-id item))
         (name (getf id :test-name))
         (package-name (getf id :package))
         (package (and package-name (find-package package-name))))
    (and package (find-symbol name package))))

(defun jump-to-test-callback (interface)
  "Open the LispWorks editor at the definition of the selected test.

This relies on the test framework registering a LispWorks dspec for each
test (as our custom FiveAM does): CL:ED resolves the definition through
the same dspec source locations that the editor's Find Source (M-.) uses."
  (let ((item (selected-item interface)))
    (when item
      (let ((symbol (test-symbol item)))
        (cond
          ((null symbol)
           (capi:display-message
            "Can't jump: the test ~a has no interned symbol in package ~a."
            (getf (item-id item) :test-name)
            (getf (item-id item) :package)))
          ((null (dspec:find-name-locations dspec:*dspec-classes* symbol))
           (capi:display-message
            "Can't jump: no source location is recorded for ~s. Make sure ~
             the test framework registers a LispWorks dspec for each test."
            symbol))
          (t
           (handler-case
               (ed symbol)
             (error (e)
               (capi:display-message "Couldn't open the editor for ~s: ~a"
                                     symbol e)))))))))

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

;; * LispWorks editor integration
;;
;; This is the equivalent of the Emacs client's C-c v / C-c j bindings,
;; but for the LispWorks editor. C-c v prompts for a test expression and
;; runs it; C-c j compiles the current top-level form first and then does
;; the same.

(defvar *last-test-expression* nil
  "The last expression run by the \"Slite Run\" editor command. Used as
the default the next time the command prompts.")

(defun run-test-expression (expression &key (package *package*))
  "Read and evaluate EXPRESSION (a string) in PACKAGE, treat its value as
raw test results, and display them in a CAPI results window. The tests
run in a separate process so the editor stays responsive, and the
expression is remembered for \"Rerun All\" and the next prompt."
  (setf *last-test-expression* expression)
  (let ((package (or (find-package package) *package*)))
    (mp:process-run-function
     "slite-run-tests" '()
     (lambda ()
       (display-test-results
        (lambda ()
          (let ((*package* package))
            (eval (read-from-string expression))))
        :title (format nil "CL Test Results: ~a" expression))))))

(defun current-editor-package ()
  "The package to read/eval in for the current editor buffer, defaulting
to *PACKAGE* if it can't be determined."
  (or (ignore-errors
        (let ((name (editor::buffer-package-to-use (editor:current-point))))
          (and name (find-package name))))
      *package*))

(defun blankp (string)
  (or (null string)
      (zerop (length (string-trim '(#\Space #\Tab #\Newline) string)))))

(defun %slite-run (p)
  "Prompt for a test expression (defaulting to the last one) and run it."
  (declare (ignore p))
  (let ((package (current-editor-package))
        (expression (editor:prompt-for-string
                     :prompt "Lisp expression for tests: "
                     :default-string (or *last-test-expression* "")
                     :help "A form that returns test results, e.g. (fiveam:run :my-suite)")))
    (unless (blankp expression)
      (editor:message "Running tests: ~a" expression)
      (run-test-expression expression :package package))))

(editor:defcommand "Slite Run" (p)
     "Prompt for a Lisp expression that returns test results, evaluate it,
and show the results in a CAPI window."
     "Prompt for a Lisp expression and run it as tests."
  (%slite-run p))

(editor:defcommand "Slite Compile Defun And Run" (p)
     "Compile the current top-level form, then prompt for a test
expression and run it (like Slite Run)."
     "Compile the current defun and then run tests."
  (editor:compile-defun-command p)
  (%slite-run p))

(defun enable-editor-bindings ()
  "Bind C-c v to \"Slite Run\" and C-c j to \"Slite Compile Defun And
Run\" in the LispWorks editor, mirroring the Emacs client. Called
automatically when this system is loaded."
  (editor:bind-key "Slite Run" #("Control-c" "v"))
  (editor:bind-key "Slite Compile Defun And Run" #("Control-c" "j")))

(handler-case
    (enable-editor-bindings)
  (error (e)
    (warn "slite/lispworks: couldn't install editor key bindings: ~a" e)))
