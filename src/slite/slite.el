(provide 'slite)

(setq lexical-binding t)

(define-derived-mode slite-results-mode tabulated-list-mode
  "CL Test Results"
  "dfdfd"
  (setq tabulated-list-format
        [("Result" 5 t)
         ("Name" 30 t)
         ("Message" 20 nil)]))

(define-derived-mode slite-details-mode fundamental-mode
  "Test Results Details"
  "dfdfd"
  (read-only-mode))

(defun slite--format-pass-fail (msg)
  (cond
   ((equal msg "PASS")
    #("PASS" 0 4 (face  `(:foreground "green"))))
   ((equal msg "FAIL")
    #("FAIL" 0 4 (face `(:background  "red"
                                      :foreground  "white"
                                      :weight bold))))
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
                  (message "got id as %s" id)
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

    (let ((id (tabulated-list-get-id)))
      (with-current-buffer buffer
        (insert (plist-get
                 id
                 :details))
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
