;; ;;; packages.el --- synelics-completion Layer packages File for Spacemacs
;; ;;
;; ;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;; ;;
;; ;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; ;; URL: https://github.com/syl20bnr/spacemacs
;; ;;
;; ;; This file is not part of GNU Emacs.
;; ;;
;; ;;; License: GPLv3

(setq synelics-html-packages
      '(
        sgml-mode
        css-mode
        ))

(defun synelics-html/init-sgml-mode ()
  (use-package sgml-mode
    :defer t
    :mode ("\\.tpl\\'" . sgml-mode)
    :init
    (add-to-list 'spacemacs-jump-handlers-sgml-mode #'synelics-jump/find-tag-no-confirm)
    (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'sgml-mode-hook 'yas-minor-mode)
    (add-hook 'after-save-hook
              (lambda ()
                (and
                 (string-equal (file-name-extension (buffer-file-name)) "tpl")
                 (shell-command (concat "~/Works/dk-reader/frontend/kits/bin/tpl " buffer-file-name " > /dev/null")))))
    (add-hook 'sgml-mode-hook
              (lambda ()
                (setq sgml-basic-offset 4)

                ;; https://www.emacswiki.org/emacs/EmacsSyntaxTable
                (modify-syntax-entry ?: ".")
                (modify-syntax-entry ?. ".")
                (modify-syntax-entry ?' ".")
                (modify-syntax-entry ?- ".")
                (modify-syntax-entry ?= ".")))))

(defun synelics-html/post-init-css-mode ()
  (use-package css-mode
    :defer t
    :config
    (progn
      (defun synelics-html//syntax-color ()
        "Syntax color text of the form 「#ff1100」 and 「#abc」 in current buffer.
URL `http://ergoemacs.org/emacs/emacs_CSS_colors.html'
Version 2016-07-04"
        (interactive)
        (font-lock-add-keywords
         nil
         '(("#[ABCDEFabcdef[:digit:]]\\{3\\}"
            (0 (put-text-property
                (match-beginning 0)
                (match-end 0)
                'face (list :background
                            (let* (
                                   (ms (match-string-no-properties 0))
                                   (r (substring ms 1 2))
                                   (g (substring ms 2 3))
                                   (b (substring ms 3 4)))
                              (concat "#" r r g g b b))))))
           ("#[ABCDEFabcdef[:digit:]]\\{6\\}"
            (0 (put-text-property
                (match-beginning 0)
                (match-end 0)
                'face (list :background (match-string-no-properties 0)))))))
        (font-lock-fontify-buffer))

      (add-hook 'css-mode-hook 'synelics-html//syntax-color)
      (add-hook 'css-mode-hook
                (lambda ()
                  (let ((lint-exec (and (projectile-project-p)
                                       (concat (projectile-project-root) "node_modules/stylelint/bin/stylelint.js"))))
                    (when (file-exists-p lint-exec)
                      (setq-local flycheck-enabled-checkers '(css-stylelint))
                      (setq-local flycheck-css-stylelint-executable lint-exec)
                      (flycheck-mode 1)))))
      (add-hook 'css-mode-hook
                (lambda ()
                  (set (make-variable-buffer-local 'company-backends)
                       '(company-css
                         company-capf
                         (company-dabbrev-code company-keywords)
                         company-files company-dabbrev)))
                'append))))
