(defsubst synelics-core/curry (function &rest arguments)
  (lexical-let ((function function)
                (arguments arguments))
        (lambda (&rest more) (apply function (append arguments more)))))

(defsubst synelics-core/curry-interactive (function &rest arguments)
  (lexical-let ((function function)
                (arguments arguments))
    (lambda (&rest more) (interactive) (apply function (append arguments more)))))

(defun synelics-core/shell-command (cmd &optional background)
  (projectile-with-default-dir (projectile-project-root)
    (if background
        (save-window-excursion
          (with-editor-async-shell-command cmd))
      (with-editor-async-shell-command cmd))))

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

;; add list
(defmacro synelics-core|add-to-list-local (local-buffer-p list-var element &optional append compare-fn)
  "Add ELEMENT to the value of LIST-VAR for local buffer or mode."
  (if local-buffer-p
      `(add-to-list (make-variable-buffer-local ,list-var) ,element &optional append compare-fn)
    `(add-to-list (make-local-variable ,list-var) ,element &optional append compare-fn)))

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
  `(dolist (fn-list ',body)
     (let ((fn (car (cdr fn-list))))
       (if fn
           (add-hook (synelics-core|hook-of-mode ,mode)
                     fn
                     ,append
                     ,local)))))
