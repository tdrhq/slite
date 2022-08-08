;; -*- lexical-binding: t -*-

(provide 'slite)

(require 'cl-lib)

(define-derived-mode slite-results-mode tabulated-list-mode
  "CL Test Results"
  "dfdfd"
  (setq tabulated-list-format
        [("Result" 5 t)
         ("Name" 30 t)
         ("Passed" 10 nil)
         ("Reason" 35 nil)]))

(defvar slite-success-shell-hook nil)

(defvar slite-success-hook 'slite--on-success)

(define-derived-mode slite-details-mode fundamental-mode
  "Test Results Details"
  "dfdfd"
  (read-only-mode))

(defvar slite-slime-impl nil
  "either :slime or :sly. Keep as nil to auto-detect.")

(defun slite--slime-impl ()
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
  #("PASS" 0 4 (face  (:foreground "green"))))

(defun slite--fail ()
  #("FAIL" 0 4 (face (:background  "red"
                                   :foreground  "white"
                                   :weight bold))))

(defun slite--format-pass-fail (msg)
  (cond
   ((equal msg "PASS")
    (slite--pass))
   ((equal msg "FAIL")
    (slite--fail))
   (t msg)))

(defun slite--format-one-line-reason (s)
  (replace-regexp-in-string
   ;; This is common enough and takes up too much screen real estate
   "^Unexpected Error: " ""
   (replace-regexp-in-string "\n" "" s )))


(cl-defun slite--parse-reason (id)
  (or
   (let ((results (plist-get id :results)))
     (cl-block inner
      (dolist (test-result results)
        (let ((reason (plist-get test-result :reason)))
          (unless (plist-get test-result :success)
            (cl-return-from inner(slite--format-one-line-reason reason)))))))
   ""))

(defun slite--show-test-results (results buffer)
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
  (cl-every (lambda (x)
           (equal "PASS"
                  (car (plist-get x :data))))
         results))

(defvar slite-history nil)

(defun slite--sl*-read-from-minibuffer (&rest args)
  (apply
   (cl-ecase (slite--slime-impl)
     (:slime 'slime-read-from-minibuffer)
     (:sly 'sly-read-from-minibuffer))
   args))

(defun slite-run (cmd &optional buffer)
  (interactive
   (list (slite--sl*-read-from-minibuffer "Lisp expression for tests: "
                                          (car slite-history)
                                          'slite-history)))

  (slite--run-expr cmd buffer))

(defun slite--run-expr (cmd &optional buffer)
  (unless (bufferp buffer)
    (setq buffer (get-buffer-create "*Test Results*"))
    (with-current-buffer buffer
      (setq slite--last-expression cmd)))
  (message "Waiting for test results...")
  (slite--sl*-eval-async `(slite::process-results (cl::eval (cl::read-from-string ,cmd)))
                         (lambda (results)
                           (when (and
                                  slite-success-hook
                                  (slite--all-tests-passed-p results))
                             (funcall slite-success-hook))

                           (slite--show-test-results results buffer))))

(defun slite-rerun ()
  (interactive)
  (slite--run-expr slite--last-expression))

(defun slite--on-success ()
  (when slite-success-shell-hook
    (save-some-buffers t compilation-save-buffers-predicate)
    (message "running hook: %s" slite-success-shell-hook)
    (shell-command slite-success-shell-hook)))

(make-local-variable 'slite--current-id)

(defun slite-describe-result ()
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

(defun slite--set-buffer-package (package)
  (cl-ecase (slite--slime-impl)
    (:slime
     (setq slime-buffer-package package))
    (:sly
     (setq sly-buffer-package package))))

(defun slite--sl*-mode ()
  (cl-ecase (slite--slime-impl)
    (:slime
     (slime-mode))
    (:sly
     (sly-mode))))

(defun slite-details-quit ()
  (interactive)
  (quit-window t))

(defun slite--current-id ()
  (or (tabulated-list-get-id)
      slite--current-id))

(defun slite--sl*-eval-async (expn callback)
  (cl-ecase (slite--slime-impl)
    (:slime
     (slime-eval-async expn callback))
    (:sly
     (sly-eval-async expn callback))))

(defun slite--sl*-compile-defun ()
  (cl-ecase (slite--slime-impl)
    (:slime
     (slime-compile-defun))
    (:sly
     (sly-compile-defun 1))))

(defun slite-rerun-in-debugger ()
  (interactive)
  (let* ((id (slite--current-id))
         (framework (plist-get id :framework))
         (name (plist-get id :test-name))
         (package (plist-get id :package)))
    (slite--sl*-eval-async `(slite/api::rerun-in-debugger ,framework ,name ,package)
      (lambda (x)
        (message "Result of running %s: %s" name x)))))

(defvar-local slite--buffer-expression nil
  "The command used to generate the tests in this buffer, we'll
  use this for slite-rerun")

(defvar slite--last-command-p nil)
(defvar slite--last-read-only-mode nil)

(defun slite-compile-defun-and-run-tests ()
  (interactive)
  (cond
   (slite--last-command-p
    (message "A compile-defun-and-run-tests is still running (if this is incorrect, setq slite--last-command-p to nil"))
   (t
    (setq slite--last-command-p t)
    (setq slite--last-read-only-mode buffer-read-only)
    (setq buffer-read-only t)
    (slite--sl*-compile-defun))))

(defun slite--compilation-finished (successp notes buffer loadp)
  (let ((last-command-p slite--last-command-p))
    (setq buffer-read-only slite--last-read-only-mode)
    (setq slite--last-command-p nil)
    (when (and successp last-command-p)
      (call-interactively 'slite-run))))

(defun slite-delete-test ()
  (interactive)
  (let* ((id (slite--current-id))
         (framework (plist-get id :framework))
         (name (plist-get id :test-name))
         (package (plist-get id :package)))
    (when (y-or-n-p (format "Delete the test %s in package %s?" name package))
     (slite--sl*-eval-async
      `(slite/api::rem-test ,framework ,name ,package)
      (lambda (x) (message "Test deleted"))))))


(define-key slite-results-mode-map (kbd "RET")
  'slite-describe-result)

(define-key slite-results-mode-map (kbd "<delete>")
  'slite-delete-test)

(define-key slite-results-mode-map (kbd "r") 'slite-rerun-in-debugger)
(define-key slite-results-mode-map (kbd "M-.") 'slite-jump-to-test)

(define-key slite-details-mode-map (kbd "r") 'slite-rerun-in-debugger)

(define-key slite-results-mode-map (kbd "g")  'slite-rerun)


(define-key slite-details-mode-map (kbd "q")
  'slite-details-quit)

(define-key lisp-mode-map (kbd "C-c v")
  'slite-run)
(define-key lisp-mode-map (kbd "C-c j")
  'slite-compile-defun-and-run-tests)
(define-key slite-results-mode-map (kbd "C-c v")
  'slite-run)



(add-hook (cl-case (slite--slime-impl)
            (:sly
             'sly-compilation-finished-hook)
            (:slime
             'slime-compilation-finished-hook))
          'slite--compilation-finished)

(defun slite-jump-to-test ()
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

(defun slite--slime-mode-map ()
  (cl-case (slite--slime-impl)
    (:slime
     slime-mode-map)
    (:sly
     sly-mode-map)))

(define-key (slite--slime-mode-map)
  (kbd "C-c v")
  'slite-run)

;; helpful while building slite, because I have to switch back and
;; forth between Lisp and elisp
(define-key emacs-lisp-mode-map (kbd "C-c v")
  'slite-run)
