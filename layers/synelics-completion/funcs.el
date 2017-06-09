(defun synelics-completion//completion-all-completions (arg table &rest rest-args)
  (let* ((candidates (completion-all-completions arg table nil (length arg)))
         (last (last candidates))
         (base-size (and (numberp (cdr last)) (cdr last))))
    (when base-size
      (setcdr last nil))
    (if (not (zerop (or base-size 0)))
        (let ((before (substring arg 0 base-size)))
          (mapcar (lambda (candidate)
                    (concat before candidate))
                  candidates))
      candidates)))

(defun synelics-completion//force-company-completion-use-syltes ()
  "Force `all-completions' use `completion-all-completions'"
  (lexical-let (call-style-completion-p)
    (advice-add 'company-call-backend-raw
                :around (lambda (old-fun &rest args)
                          (let ((command (car args)))
                            (if (eq command 'candidates)
                                (setq call-style-completion-p t)))
                          (apply old-fun args)))
    (advice-add 'all-completions
                :around (lambda (old-fun &rest args)
                          (if call-style-completion-p
                              (apply 'synelics-completion//completion-all-completions args)
                            (apply old-fun args))))
    (advice-add 'completion-all-completions
                :around (lambda (old-fun &rest args)
                          (setq call-style-completion-p nil)
                          (apply old-fun args)))))

(defadvice ycmd--handle-goto-response (around synelics-completion//goto-current-project-only)
  (interactive)
  (if (and (assq 'filepath response)
           (string-match-p (projectile-project-root) (cdr (assq 'filepath response))))
      ad-do-it
    (run-hook-with-args 'ycmd-after-exception-hook "goto" (current-buffer) (point) response)))
