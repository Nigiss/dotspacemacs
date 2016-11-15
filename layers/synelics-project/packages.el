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

(setq synelics-project-packages
      '(
        projectile
        nameframe
        window-numbering
        ))

(defun synelics-project/post-init-projectile ()
  (use-package projectile
    :defer t
    :config
    (progn
      (evil-leader/set-key
        "fr" '(lambda ()
                (interactive)
                (revert-buffer nil 'noconfirm))
        "fm" 'counsel-recentf
        "jt" 'synelics-core/find-tag
        "fF" '(lambda ()
                (interactive)
                (counsel-find-file)
                (split-window-below)
                (spacemacs/previous-useful-buffer)
                (evil-window-down 1))
        "fp" 'projectile-find-file
        "px" 'projectile-run-shell
        "sp" 'spacemacs/search-project-auto-region-or-symbol
        "sP" 'spacemacs/search-project-auto))))

(defun synelics-project/init-nameframe ()
  (use-package nameframe
    :defer t
    :init
    (progn
      (evil-leader/set-key
        "pe" 'nameframe-create-frame
        "ps" 'nameframe-switch-frame))))

(defun synelics-project/post-init-window-numbering ()
  (use-package window-numbering
    :defer t
    :init
    (dotimes (i 10)
      (define-key window-numbering-keymap
        (kbd (format "s-%s" i))
        (intern (format "select-window-%s" i))))))
