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
        rjsx-mode
        ))

(defun synelics-javascript/post-init-js2-mode ()
  (use-package js2-mode
    :defer t
    :init
    (progn
      (setq js-indent-level 2)
      (synelics-core|add-hook 'js2-mode
                              (lambda ()
                                (setq mode-name "JS")
                                (js2-mode-hide-warnings-and-errors)
                                (evil-matchit-mode +1)
                                (synelics-javascript//find-definition-use-xref-marker)))

      (add-hook 'js2-mode-hook
                (lambda ()
                  (let ((standard-exec (and (projectile-project-p)
                                           (concat (projectile-project-root) "node_modules/standard/bin/cmd.js")))
                        (eslint-exec (and (projectile-project-p)
                                          (concat (projectile-project-root) "node_modules/eslint/bin/eslint.js"))))
                    (if (file-exists-p standard-exec)
                        (progn
                          (setq-local flycheck-enabled-checkers '(javascript-standard))
                          (setq-local flycheck-javascript-standard-executable standard-exec)
                          (flycheck-mode 1))
                      (when (file-exists-p eslint-exec)
                        (setq-local flycheck-enabled-checkers '(javascript-eslint))
                        (setq-local flycheck-javascript-eslint-executable eslint-exec)
                        (flycheck-mode 1)))))))))

(defun synelics-javascript/init-rjsx-mode ()
  (use-package rjsx-mode
    :defer t
    ;; :mode ("\\.js\\'" . rjsx-mode)
    ))
