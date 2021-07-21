(provide 'slite)

(setq lexical-binding t)

(define-derived-mode slite-results-mode tabulated-list-mode
  "CL Test Results"
  "dfdfd"
  (setq tabulated-list-format
        [("Result" 5 t)
         ("Name" 30 t)
         ("Message" 20 nil)]))

(define-derived-mode slite-details-mode slime-mode
  "Test Results Details"
  "dfdfd"
  (read-only-mode))

(defun slite--pass ()
  #("PASS" 0 4 (face  `(:foreground "green"))))

(defun slite--fail ()
  #("FAIL" 0 4 (face `(:background  "red"
                                    :foreground  "white"
                                    :weight bold))))

(defun slite--format-pass-fail (msg)
  (cond
   ((equal msg "PASS")
    (slite--pass))
   ((equal msg "FAIL")
    (slite--fail))
   (t msg)))

(defun slite--show-test-results (results buffer)
  (message "Showing results")
  (with-current-buffer buffer
    (slite-results-mode)
    (setq tabulated-list-entries
          (loop for x in results
                collect
                (let ((data (plist-get x :data))
                      (id (plist-get x :id)))
                 (list id
                       (apply 'vector  (slite--format-pass-fail (car data))
                              (cdr data)) ))))
    (tabulated-list-init-header)
    (tabulated-list-print)
    (display-buffer buffer)))

(defun slite-run (cmd &optional buffer)
  (interactive
   (list (slime-read-from-minibuffer "Lisp expression for tests: "
                                     (car slite-history)
                                     'slite-history)))

  (unless (bufferp buffer)
    (setq buffer (get-buffer-create "*Test Results*")))
  (message "Waiting for test results...")
  (slime-eval-async `(slite::process-results (cl::eval (cl::read-from-string ,cmd)))
    (lambda (results)
      (slite--show-test-results results buffer))))


(defun slite-jump-to-definition ()
  (interactive)
  (message "Jump to definition not supported yet"))


(defun slite--show-details (details)
)

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

        (setq slime-buffer-package package)
        (slite-details-mode)
        (switch-to-buffer-other-window buffer)))))

(defun slite-details-quit ()
  (interactive)
  (quit-window t))

(define-key slite-results-mode-map (kbd "M-.") 'slite-jump-to-definition)
(define-key slite-results-mode-map (kbd "RET")
  'slite-describe-result)


(define-key slite-details-mode-map (kbd "q")
  'slite-details-quit)

(define-key lisp-mode-map (kbd "C-c v")
  'slite-run)

;; helpful while building slite, because I have to switch back and
;; forth between Lisp and elisp
(define-key emacs-lisp-mode-map (kbd "C-c v")
  'slite-run)
