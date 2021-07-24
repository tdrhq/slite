;; -*- lexical-binding: t -*-

(provide 'slite)

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

(defun slite--remove-newlines (s)
  (replace-regexp-in-string "\n" "" s ))


(defun slite--parse-reason (id)
  (or
   (let ((results (plist-get id :results)))
     (dolist (test-result results)
       (let ((reason (plist-get test-result :reason)))
         (unless (plist-get test-result :success)
           (cl-return (slite--remove-newlines reason))))))
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
  (every (lambda (x)
           (equal "PASS"
                  (car (plist-get x :data))))
         results))

(defvar slite-history nil)

(defun slite--sl*-read-from-minibuffer (&rest args)
  (apply
   (ecase (slite--slime-impl)
     (:slime 'slime-read-from-minibuffer)
     (:sly 'sly-read-from-minibuffer))
   args))

(defun slite-run (cmd &optional buffer)
  (interactive
   (list (slite--sl*-read-from-minibuffer "Lisp expression for tests: "
                                          (car slite-history)
                                          'slite-history)))

  (unless (bufferp buffer)
    (setq buffer (get-buffer-create "*Test Results*")))
  (message "Waiting for test results...")
  (slite--sl*-eval-async `(slite::process-results (cl::eval (cl::read-from-string ,cmd)))
    (lambda (results)
      (when (and
             slite-success-hook
             (slite--all-tests-passed-p results))
        (funcall slite-success-hook))

      (slite--show-test-results results buffer))))

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
            (insert (plist-get result :reason))))
        (setq slite--current-id id)
        (slite-details-mode)
        (slite--sl*-mode)
        (slite--set-buffer-package package)
        (switch-to-buffer-other-window buffer)))))

(defun slite--set-buffer-package (package)
  (ecase (slite--slime-impl)
    (:slime
     (setq slime-buffer-package package))
    (:sly
     (setq sly-buffer-package package))))

(defun slite--sl*-mode ()
  (ecase (slite--slime-impl)
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
  (ecase (slite--slime-impl)
    (:slime
     (slime-eval-async expn callback))
    (:sly
     (sly-eval-async expn callback))))

(defun slite--sl*-compile-defun ()
  (ecase (slite--slime-impl)
    (:slime
     (slime-compile-defun))
    (:sly
     (sly-compile-defun))))

(defun slite-rerun-in-debugger ()
  (interactive)
  (let* ((id (slite--current-id))
         (name (plist-get id :test-name))
         (package (plist-get id :package)))
    (slite--sl*-eval-async `(slite::rerun-in-debugger ,name ,package)
      (lambda (x)
        (message "Result of running %s: %s" name x)))))

(defvar slite--last-command-p nil)

(defun slite-compile-defun-and-run-tests ()
  (interactive)
  (setq slite--last-command-p t)
  (slite--sl*-compile-defun))

(defun slite--compilation-finished (successp notes buffer loadp)
  (when (and successp slite--last-command-p)
    (setq slite--last-command-p nil)
    (call-interactively 'slite-run)))


(define-key slite-results-mode-map (kbd "RET")
  'slite-describe-result)

(define-key slite-results-mode-map (kbd "r") 'slite-rerun-in-debugger)
(define-key slite-details-mode-map (kbd "r") 'slite-rerun-in-debugger)


(define-key slite-details-mode-map (kbd "q")
  'slite-details-quit)

(define-key lisp-mode-map (kbd "C-c v")
  'slite-run)
(define-key lisp-mode-map (kbd "C-c j")
  'slite-compile-defun-and-run-tests)
(define-key slite-results-mode-map (kbd "C-c v")
  'slite-run)


(add-hook (case (slite--slime-impl)
            (:sly
             'sly-compilation-finished-hook)
            (:slime
             'slime-compilation-finished-hook))
          'slite--compilation-finished)

(defun slite--slime-mode-map ()
  (case (slite--slime-impl)
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
