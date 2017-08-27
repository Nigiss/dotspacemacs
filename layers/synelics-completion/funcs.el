(defun synelics-completion//force-company-completion-use-styles ()
  "Force `all-completions' use `completion-all-completions'"

  (defun synelics-completion//completion-all-completions (arg table)
    ;; inspired by case `candidate' of `company-capf'
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

  (lexical-let (call-style-completion-p)
    (advice-add 'all-completions
                :around (lambda (old-fun &rest args)
                          (if call-style-completion-p
                              (apply 'synelics-completion//completion-all-completions args)
                            (apply old-fun args))))
    (advice-add 'completion-all-completions
                :around (lambda (old-fun &rest args)
                          (setq call-style-completion-p nil)
                          (apply old-fun args)))
    (advice-add 'company-calculate-candidates
                :around (lambda (old-fun &rest args)
                          (setq call-style-completion-p t)
                          (apply old-fun args)))))

(defadvice ycmd--handle-goto-response (around synelics-completion//goto-current-project-only)
  (interactive)
  (if (and (assq 'filepath response)
           (string-match-p (projectile-project-root) (cdr (assq 'filepath response))))
      ad-do-it
    (run-hook-with-args 'ycmd-after-exception-hook "goto" (current-buffer) (point) response)))
