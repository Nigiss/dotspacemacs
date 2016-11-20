(defun synelics-core/find-tag-no-confirm ()
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

(defun synelics-core/find-tag ()
  "Find tag without confirm."
  (interactive)
  (let* ((tags-file (concat (projectile-project-root) "TAGS")))

    (unless (file-exists-p tags-file)
      (synelics-core/update-tags-table))

    (visit-tags-table tags-file 'local)
    (let ((tagname (find-tag-interactive "Find tag: ")))
      (find-tag (car tagname)))))

(defun synelics-core/update-tags-table ()
  "Update tags table with shell script."
  (interactive)
  (projectile-with-default-dir (projectile-project-root)
    (shell-command "bash gen-tags.sh")))

(defsubst synelics-core/curry (function &rest arguments)
  (lexical-let ((function function)
                (arguments arguments))
        (lambda (&rest more) (apply function (append arguments more)))))

;;; Macro
(defmacro synelics-core|center-cursor-after-call (fn)
  `(lambda ()
     (interactive)
     (funcall ,fn)
     (evil-scroll-line-to-center (line-number-at-pos))))

(defmacro synelics-core|remove-from-list (list-var element)
  "Remove element from list."
  `(setq ,list-var (delete ,element ,list-var)))

(defmacro synelics-core|local-set-key (mode map key def)
  "Set key for a map in certain mode."
  `(add-hook (synelics-core|hook-of-mode ,mode)
            (lambda ()
              (add-to-list (make-local-variable ',map)
                           (cons (kbd ,key) ,def)))))

;; hook
(defmacro synelics-core|hook-of-mode (mode)
  "Return hook of a mode."
  `(intern (format "%s-hook"(symbol-name ,mode))))

(defmacro synelics-core|add-hook (mode &rest body)
  `(synelics-core||add-hook-base ,mode nil nil ,@body))

(defmacro synelics-core|add-hooks (modes &rest body)
  `(dolist (mode ,modes)
    (synelics-core|add-hook mode ,@body)))

(defmacro synelics-core|add-hook-local (mode &rest body)
  `(synelics-core||add-hook-base ,mode nil t ,@body))

(defmacro synelics-core|add-hooks-local (modes &rest body)
  `(dolist (mode ,modes)
     (synelics-core|add-hook-local mode ,@body)))

(defmacro synelics-core||add-hook-base (mode &optional append local &rest body)
  "Custom add hook."
  `(dolist (fn ',body)
     (add-hook (synelics-core|hook-of-mode ,mode)
               (car (cdr fn))
               ;; (if (eq (car fn) 'quote)
               ;;     (car (cdr fn))
               ;;   fn)
               ,append
               ,local)))

;; (synelics-core|add-hook 'prog-mode (lambda () (set-input-method default-input-method)))

;; (progn
;;   (set 'ttt-mode-hook nil)
;;   (synelics-core|add-hook 'ttt-mode
;;                           'spacemacs/toggle-fullscreen-frame
;;                           (lambda () ())))
