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
        "fm" 'ivy-recentf
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
