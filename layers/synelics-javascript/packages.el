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
        ycmd
        company-ycmd
        flycheck
        ))

(defun synelics-javascript/post-init-js2-mode ()
  (use-package js2-mode
    :defer t
    :mode "\\.js\\'"
    :init
    (progn
      (setq js-indent-level 2)

      (synelics-core|add-hook 'js2-mode
                              'ycmd-mode
                              'paredit-mode
                              'subword-mode
                              'flycheck-mode
                              'evil-matchit-mode)

      (synelics-core|add-hook 'js2-mode
                              (lambda ()
                                (setq mode-name "JavaScript")
                                (js2-mode-hide-warnings-and-errors)))

      (synelics-core|add-hook 'js2-mode
                              (lambda ()
                                (define-key evil-normal-state-local-map
                                  (kbd "C-]")
                                  (synelics-core|center-cursor-after-call 'synelics//js-goto-definition))
                                (define-key evil-normal-state-local-map
                                  (kbd "C-t")
                                  (synelics-core|center-cursor-after-call 'pop-tag-mark)))))
    :config
    (spacemacs/set-leader-keys-for-major-mode 'js2-mode
      "w" 'js2-mode-toggle-warnings-and-errors
      "zc" 'js2-mode-hide-element
      "zo" 'js2-mode-show-element
      "zr" 'js2-mode-show-all
      "ze" 'js2-mode-toggle-element
      "zF" 'js2-mode-toggle-hide-functions
      "zC" 'js2-mode-toggle-hide-comments)))

(defun synelics-javascript/post-init-ycmd ()
  (use-package ycmd
    :defer t
    :init
    (spacemacs|add-company-hook js2-mode)))

(defun synelics-javascript/post-init-flycheck ()
  (use-package flycheck
    :defer t
    :init
    (synelics-core|add-hook 'js2-mode
                            (lambda () (setq-default flycheck-enabled-checkers '(javascript-standard))))))

(defun synelics-javascript/post-init-company-ycmd ()
  (use-package company-ycmd
    :defer t
    :if (and (configuration-layer/package-usedp 'company)
             (configuration-layer/package-usedp 'ycmd))
    :init
    (push 'company-ycmd company-backends-js2-mode)))

