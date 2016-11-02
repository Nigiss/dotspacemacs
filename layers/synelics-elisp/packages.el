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

(setq synelics-elisp-packages
      '(
        emacs-lisp
        ))

(defun synelics-elisp/post-init-emacs-lisp ()
  (use-package emacs-lisp
    :defer t
    :init
    (progn
      (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
      (add-hook 'emacs-lisp-mode-hook
                (lambda ()
                  (set (make-variable-buffer-local 'company-idle-delay) .2)
                  (define-key evil-normal-state-local-map (kbd "C-]") 'elisp-slime-nav-find-elisp-thing-at-point))))))
