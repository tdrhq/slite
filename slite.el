;;; slite.el --- Interactively runs your Common Lisp tests  -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Arnold Noronha

;; Author: Arnold Noronha <arnold@tdrhq.com>
;; Homepage: https://github.com/tdrhq/slite
;; Keywords: lisp tools
;; Package-Requires: ((emacs "25.1"))
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Slite stands for SLIme TEst runner.  Slite interactively runs
;; your Common Lisp tests (currently only FiveAM and Parachute are
;; supported).  It allows you to see the summary of test failures,
;; jump to test definitions, rerun tests with debugger all from
;; inside Emacs.

;; You might want to add some key bindings in various Lisp mode maps:
;;
;;     (define-key emacs-lisp-mode-map (kbd "C-c v") #'slite-run)
;;     (define-key lisp-mode-map (kbd "C-c v") #'slite-run)
;;     (define-key lisp-mode-map (kbd "C-c j")
;;       #'slite-compile-defun-and-run-tests)
;;     (with-eval-after-load 'slime
;;       (define-key slime-mode-map (kbd "C-c v") #'slite-run))
;;     (with-eval-after-load 'sly
;;       (define-key sly-mode-map (kbd "C-c v") #'slite-run))

;;; Code:

(require 'cl-lib)

(declare-function sly-compile-defun "sly")
(declare-function sly-edit-definition "sly")
(declare-function sly-eval-async "sly")
(declare-function sly-mode "sly")

(declare-function slime-compile-defun "slime")
(declare-function slime-edit-definition "slime")
(declare-function slime-eval-async "slime")
(declare-function slime-mode "slime")

(defvar slite-results-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET")      #'slite-describe-result)
    (define-key map (kbd "<delete>") #'slite-delete-test)
    (define-key map (kbd "r")        #'slite-rerun-in-debugger)
    (define-key map (kbd "M-.")      #'slite-jump-to-test)
    (define-key map (kbd "g")        #'slite-rerun)
    (define-key map (kbd "C-c v")    #'slite-run)
    map)
  "Keymap for `slite-results-mode'.")

(define-derived-mode slite-results-mode tabulated-list-mode
  "CL Test Results"
  "A tabulated mode to show results from your last Slite run."
  (setq tabulated-list-format
        [("Result" 5 t)
         ("Name" 30 t)
         ("Passed" 10 nil)
         ("Reason" 35 nil)]))

(defvar slite-success-shell-hook nil)

(defvar slite-success-hook 'slite--on-success)

(defvar slite-details-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") #'slite-rerun-in-debugger)
    (define-key map (kbd "q") #'slite-details-quit)
    map))

(define-derived-mode slite-details-mode fundamental-mode
  "Test Results Details"
  "A mode to show details for a specific test case from a Slite run."
  (read-only-mode))

(defvar slite-slime-impl nil
  "Either :slime or :sly.  Keep as nil to auto-detect.")

(defun slite--slime-impl ()
  "Return :sly or :slime depending on which one we're running against."
  (cond
   (slite-slime-impl
    slite-slime-impl)
   ((functionp 'sly)
    :sly)
   ((functionp 'slime)
    :slime)
   (t
    (error "Neither SLIME or SLY could be autodetected"))))

(defun slite--pass ()
  "The constant PASS with font-face."
  #("PASS" 0 4 (face  (:foreground "green"))))

(defun slite--fail ()
  "The constant FAIL with font-face."
  #("FAIL" 0 4 (face (:background  "red"
                                   :foreground  "white"
                                   :weight bold))))

(defun slite--format-pass-fail (msg)
  "Parse the MSG to determine how to render it."
  (cond
   ((equal msg "PASS")
    (slite--pass))
   ((equal msg "FAIL")
    (slite--fail))
   (t msg)))

(defun slite--format-one-line-reason (s)
  "Format S as a one line string."
  (replace-regexp-in-string
   ;; This is common enough and takes up too much screen real estate
   "^Unexpected Error: " ""
   (replace-regexp-in-string "\n" "" s )))

(cl-defun slite--parse-reason (id)
  "Parse the reason for the failure from ID."
  (or
   (let ((results (plist-get id :results)))
     (cl-block inner
      (dolist (test-result results)
        (let ((reason (plist-get test-result :reason)))
          (unless (plist-get test-result :success)
            (cl-return-from inner(slite--format-one-line-reason reason)))))))
   ""))

(defun slite--show-test-results (results buffer)
  "Show the test results from RESULTS in BUFFER."
  (message "Got test results")
  (with-current-buffer buffer
    (slite-results-mode)
    (setq tabulated-list-entries
          (cl-loop for x in results
                collect
                (let ((data (plist-get x :data))
                      (id (plist-get x :id)))
                 (list id
                       (apply 'vector  (slite--format-pass-fail (car data))
                              (append
                               (cdr data)
                               (list
                                (slite--parse-reason id)))) ))))
    (tabulated-list-init-header)
    (tabulated-list-print)
    (display-buffer buffer)
    ;;;; I don't think this is the right behavior:
    ;; (unless (slite--all-tests-passed-p results)
    ;;   (switch-to-buffer buffer))
    ))

(defun slite--all-tests-passed-p (results)
  "Check if all the tests in RESULTS are passing."
  (cl-every (lambda (x)
           (equal "PASS"
                  (car (plist-get x :data))))
         results))

(defvar slite-history nil)

(defun slite--sl*-read-from-minibuffer (&rest args)
  "Call either {slime|sly}-from-minibuffer with the ARGS."
  (apply
   (cl-ecase (slite--slime-impl)
     (:slime 'slime-read-from-minibuffer)
     (:sly 'sly-read-from-minibuffer))
   args))

;; I don't know of a better way to "override" the default value 
(defvar slite-test-expression nil)

(defun slite-run (cmd &optional buffer)
  "Interactively run CL tests using the expression CMD and output the results into BUFFER."
  (interactive
   (list (slite--sl*-read-from-minibuffer "Lisp expression for tests: "
                                          (or
                                           slite-test-expression
                                           (car slite-history))
                                          'slite-history)))

  (slite--run-expr cmd buffer))

(defun slite-run-expression (expr)
  "Run a specific expression interactively. The expression that
will be executed will default to the provided expression."
  (interactive)
  (let ((slite-test-expression expr))
   (call-interactively 'slite-run)))

(defvar slite--last-expression nil)

(defun slite--run-expr (cmd &optional buffer)
  "Non-interactive version of slite-run."
  (unless (bufferp buffer)
    (setq buffer (get-buffer-create "*Test Results*"))
    (with-current-buffer buffer
      (setq slite--last-expression cmd)))
  (message "Waiting for test results...")
  (slite--sl*-eval-async
   `(slite::process-results (cl::eval (cl::read-from-string ,cmd)))
   (lambda (results)
     (when (and
            slite-success-hook
            (slite--all-tests-passed-p results))
       (funcall slite-success-hook))

     (slite--show-test-results results buffer))))

(defun slite-rerun ()
  "Re-run the last expression."
  (interactive)
  (slite--run-expr slite--last-expression))

(defun slite--on-success ()
  "Callback when tests have run successfully."
  (when slite-success-shell-hook
    (save-some-buffers t compilation-save-buffers-predicate)
    (message "running hook: %s" slite-success-shell-hook)
    (shell-command slite-success-shell-hook)))

(make-local-variable 'slite--current-id)

(defun slite-describe-result ()
  "Describe the results at point into a buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "*Test Case Details*")))

    (let* ((id (tabulated-list-get-id))
           (results (plist-get id :results))
           (package (plist-get id :package)))
      (with-current-buffer buffer
        (insert (plist-get
                 id
                 :details))

        ;; now we add each of the test results
        (dolist (result results)
          (cond
           ((plist-get result :success)
            (insert (slite--pass)))
           (t
            (insert (slite--fail))))
          (insert " ")
          (insert (plist-get result :expression))
          (insert "\n\n")

          (unless (plist-get result :success)
            (insert "------------------\n")
            (insert (plist-get result :reason))
            (insert "\n")
            (insert "------------------")
            (insert "\n\n")))
        (setq slite--current-id id)
        (slite-details-mode)
        (slite--sl*-mode)
        (slite--set-buffer-package package)
        (switch-to-buffer-other-window buffer)))))

(defvar-local slime-buffer-package nil)
(defvar-local sly-buffer-package nil)

(defun slite--set-buffer-package (package)
  "Wrapper for {sly|slime}-set-buffer-package."
  (cl-ecase (slite--slime-impl)
    (:slime
     (setq slime-buffer-package package))
    (:sly
     (setq sly-buffer-package package))))

(defun slite--sl*-mode ()
  "Wrappe for {sly|slime}-mode."
  (cl-ecase (slite--slime-impl)
    (:slime
     (slime-mode))
    (:sly
     (sly-mode))))

(defun slite-details-quit ()
  "Simply quits the slite window."
  (interactive)
  (quit-window t))

(defun slite--current-id ()
  "Get the ID of the test we're looking at.

This might either be in the test details view, or in the test
tabulated list."
  (or (tabulated-list-get-id)
      slite--current-id))

(defun slite--get-sly-symbol (name)
  (cl-ecase (slite--slime-impl)
    (:slime
     (intern (format "slime-%s"  name)))
    (:sly
     (intern (format "sly-%s" name)))))

(defun slite--funcall-sly (name &rest args)
  (apply
   (slite--get-sly-symbol name)
   args))

(defun slite--sly-symbol-value (name)
  (symbol-value
   (slite--get-sly-symbol name)))

(defun slite--sl*-eval-async (expn callback)
  "Wrapper for {sly|slime}-eval-async."
  (cl-ecase (slite--slime-impl)
    (:slime
     (slime-eval-async expn callback))
    (:sly
     (sly-eval-async expn callback))))

(defun slite--sl*-compile-defun ()
  "Wrapper for {sly|slime}-compile-defun."
  (cl-ecase (slite--slime-impl)
    (:slime
     (slime-compile-defun))
    (:sly
     (sly-compile-defun 1))))

(defun slite-rerun-in-debugger ()
  "Re-run the test at point in a debugger."
  (interactive)
  (let* ((id (slite--current-id))
         (framework (plist-get id :framework))
         (name (plist-get id :test-name))
         (package (plist-get id :package)))
    (slite--sl*-eval-async
     `(slite/api::rerun-in-debugger ,framework ,name ,package)
      (lambda (x)
        (message "Result of running %s: %s" name x)))))

(defvar-local slite--buffer-expression nil
  "The command used to generate the tests in this buffer.

We'll use this for slite-rerun.")

(defvar slite--last-command-p nil)
(defvar slite--last-read-only-mode nil)

(defun slite-compile-defun-and-run-tests ()
  "Compile the current expression and run tests if the compilation passes."
  (interactive)
  (cond
   (slite--last-command-p
    (message "A compile-defun-and-run-tests is still running (if \
this is incorrect, setq slite--last-command-p to nil"))
   (t
    (setq slite--last-command-p t)
    (setq slite--last-read-only-mode buffer-read-only)
    (setq buffer-read-only t)
    (slite--sl*-compile-defun))))

;; In SLY, we are passed four arguments `succesp notes buffer
;; loadp`. In Slime, we are passed only notes. In both cases a
;; {slime|sly}-last-compilation-result variable is set, so we'll use
;; that instead to get the result.
(defun slite--compilation-finished (&rest args)
  "Callback for a CL compilation."
  (let ((last-command-p slite--last-command-p))
    (setq buffer-read-only slite--last-read-only-mode)
    (setq slite--last-command-p nil)
    (let ((successp
           (slite--funcall-sly 'compilation-result.successp
                               (slite--sly-symbol-value
                                'last-compilation-result))))
      (when (and successp last-command-p)
        (call-interactively 'slite-run)))))

(defun slite-delete-test ()
  "Delete the test at point."
  (interactive)
  (let* ((id (slite--current-id))
         (framework (plist-get id :framework))
         (name (plist-get id :test-name))
         (package (plist-get id :package)))
    (when (y-or-n-p (format "Delete the test %s in package %s?" name package))
     (slite--sl*-eval-async
      `(slite/api::rem-test ,framework ,name ,package)
      (lambda (_) (message "Test deleted"))))))

(add-hook (cl-case (slite--slime-impl)
            (:sly
             'sly-compilation-finished-hook)
            (:slime
             'slime-compilation-finished-hook))
          'slite--compilation-finished)

(defun slite-jump-to-test ()
  "Jump to the test at point.

Currently only supported on Lispworks, and requires a patched
version of FiveAM.  Please see README."
  (interactive)
  (let* ((id (slite--current-id))
         (name (plist-get id :test-name))
         (package (plist-get id :package)))
    (cl-ecase (slite--slime-impl)
      (:slime
       (let ((slime-buffer-package package))
         (slime-edit-definition name)))
      (:sly
       (let ((sly-buffer-package package))
         (sly-edit-definition name))))))

(defun slite--define-keybindings ()
  "Define a set of default keybindings to be used with slite."
  (define-key emacs-lisp-mode-map (kbd "C-c v") #'slite-run)
  (define-key lisp-mode-map (kbd "C-c v") #'slite-run)
  (define-key lisp-mode-map (kbd "C-c j")
    #'slite-compile-defun-and-run-tests)
  (with-eval-after-load 'slime
    (define-key slime-mode-map (kbd "C-c v") #'slite-run))
  (with-eval-after-load 'sly
    (define-key sly-mode-map (kbd "C-c v") #'slite-run)))

(provide 'slite)
;;; slite.el ends here
