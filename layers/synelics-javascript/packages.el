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

(setq synelics-javascript-packages
      '(
        js2-mode
        company-ycmd
        flycheck-mode
        ))

(defun synelics-javascript/post-init-js2-mode ()
  (use-package js2-mode
    :defer t
    :mode "\\.js\\'"
    :init
    (progn
      (setq js-indent-level 2)
      (setq tags-case-fold-search nil)

      (synelics-core|add-hook 'js2-mode
                              'subword-mode)

      (synelics-core|add-hook 'js2-mode
                              (lambda ()
                                (setq mode-name "JS")
                                (js2-mode-hide-warnings-and-errors)))

      (synelics-core|add-hook 'js2-mode
                              (lambda ()
                                (let ((eslint-exec (and (projectile-project-p)
                                                       (concat (projectile-project-root) "node_modules/eslint/bin/eslint.js"))))
                                  (when (file-exists-p eslint-exec)
                                    (flycheck-mode 1)
                                    (set (make-variable-buffer-local 'flycheck-enabled-checkers) '(javascript-eslint))
                                    (set (make-variable-buffer-local 'flycheck-javascript-eslint-executable) eslint-exec)))))

      (synelics-core|add-hook 'js2-mode
                              (lambda ()
                                (setq company-backends-js2-mode
                                      '((company-tern company-etags)
                                        (company-dabbrev-code company-gtags company-keywords)
                                        company-files company-dabbrev))

                                (define-key evil-normal-state-local-map (kbd "C-]") 'synelics-javascript//js-goto-definition)
                                (define-key evil-normal-state-local-map (kbd "C-t") 'pop-tag-mark))))
    :config
    (spacemacs/set-leader-keys-for-major-mode 'js2-mode
      "w" 'js2-mode-toggle-warnings-and-errors
      "zc" 'js2-mode-hide-element
      "zo" 'js2-mode-show-element
      "zr" 'js2-mode-show-all
      "ze" 'js2-mode-toggle-element
      "zF" 'js2-mode-toggle-hide-functions
      "zC" 'js2-mode-toggle-hide-comments)))
