(defun synelics-core/find-tag ()
  "Find tag without confirm."
  (interactive)
  (let* ((tags-file (concat (projectile-project-root) "TAGS"))
         (current-point (point))
         (inner-symbol (evil-inner-symbol))
         (begin (car inner-symbol))
         (end (car (cdr inner-symbol))))

    (goto-char current-point)

    (unless (file-exists-p tags-file)
      (synelics-core/update-tags-table))

    (visit-tags-table tags-file 'local)

    (find-tag (buffer-substring-no-properties begin end))))

(defun synelics-core/update-tags-table ()
  "Update tags table with shell script."
  (interactive)
  (projectile-with-default-dir (projectile-project-root)
    (shell-command "./gen-tags.sh")))

(defsubst synelics-core/curry (function &rest arguments)
  (lexical-let ((function function)
                (arguments arguments))
        (lambda (&rest more) (apply function (append arguments more)))))

(defun buffer-local-set-key (key func)
  (interactive "KSet key on this buffer: \naCommand: ")
  (let ((name (format "%s-magic" (buffer-name))))
    (eval
     `(define-minor-mode ,(intern name)
        "Automagically built minor mode to define buffer-local keys."))
    (let* ((mapname (format "%s-map" name))
           (map (intern mapname)))
      (unless (boundp (intern mapname))
        (set map (make-sparse-keymap)))
      (eval
       `(define-key ,map ,key func)))
            (funcall (intern name) t)))

(defmacro synelics-core/center-cursor-after-call (fn)
  `(lambda ()
     (interactive)
     (funcall ,fn)
     (evil-scroll-line-to-center (line-number-at-pos))))
