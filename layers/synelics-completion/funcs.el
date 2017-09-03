(defun synelics-completion//completion-all-completions (arg table &rest rest)
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

(defun synelics-completion//use-styles ()
  "Force `all-completions' use `completion-all-completions', ensure `completion-styles' is available."
  (lexical-let ((force-completion-use-styles-p t))
    (advice-add 'completion-all-completions
                :before (lambda (&rest args)
                          (setq force-completion-use-styles-p nil)))
    (advice-add 'all-completions
                :around (lambda (old-fun &rest args)
                          (if force-completion-use-styles-p
                              (apply 'synelics-completion//completion-all-completions args)
                            (apply old-fun args))))
    (advice-add 'company-calculate-candidates
                :before (lambda (&rest args)
                          (setq force-completion-use-styles-p t)))))

(defun synelics-completion//async-call-next ()
  "Force async company backends use next while complete failed."
  (defun company--fetch-candidates (prefix)
    (let* ((non-essential (not (company-explicit-action-p)))
           (c (if company--manual-action
                  (company-call-backend 'candidates prefix)
                (company-call-backend-raw 'candidates prefix)))
           res)
      (if (not (eq (car c) :async))
          c
        (lexical-let ((buf (current-buffer))
                      (win (selected-window))
                      (tick (buffer-chars-modified-tick))
                      (pt (point))
                      (backend company-backend)
                      (res 'done)
                      (prefix prefix))
          (funcall
           (cdr c)
           (lambda (candidates)
             (if (not (and candidates (eq res 'done)))
                 ;; There's no completions to display,
                 ;; or the fetcher called us back right away.
                 (progn
                   (setq res candidates)
                   ;; Custom
                   (ignore-errors
                     (advice-add 'message
                                 :around (lambda (old-fun &rest args)
                                           (unless (equal (car args) "No completion found")
                                             (apply old-fun args))))
                     (company-other-backend backend)))
               (setq company-backend backend
                     company-candidates-cache
                     (list (cons prefix
                                 (company--preprocess-candidates candidates))))
               (unwind-protect
                   (company-idle-begin buf win tick pt)
                 (unless company-candidates
                   (setq company-backend nil
                         company-candidates-cache nil)))))))
        ;; FIXME: Relying on the fact that the callers
        ;; will interpret nil as "do nothing" is shaky.
        ;; A throw-catch would be one possible improvement.
        (or res
            (progn (setq res 'done) nil))))))
