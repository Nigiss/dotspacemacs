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
        workgroups2
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

(defun synelics-project/init-workgroups2 ()
  (use-package workgroups2
    :defer t
    :init
    (progn
      ;; Change workgroups session file
      (setq wg-session-file "~/.emacs.d/.emacs_workgroups")

      ;; What to do on Emacs exit / workgroups-mode exit?
      (setq wg-emacs-exit-save-behavior           'save)      ; Options: 'save 'ask nil
      (setq wg-workgroups-mode-exit-save-behavior 'save)      ; Options: 'save 'ask nil

      ;; Mode Line changes
      ;; Display workgroups in Mode Line?
      (setq wg-mode-line-display-on t)          ; Default: (not (featurep 'powerline))
      (setq wg-flag-modified t)                 ; Display modified flags as well
      (setq wg-mode-line-decor-left-brace "["
            wg-mode-line-decor-right-brace "]"  ; how to surround it
            wg-mode-line-decor-divider ":")

      (add-hook 'kill-emacs-hook
                (lambda ()
                  (wg-save-session)))

      ;; Speed up buffer operations

      (add-hook 'workgroups-mode-hook
                (lambda ()
                  (wg-disable-all-advice)
                  (wg-add-or-remove-workgroups-hooks t)))

      (add-hook 'wg-before-switch-to-workgroup-hook
                (lambda ()
                  (condition-case nil
                      (synelics-wg//set-marker-ring (synelics-wg//get-wg-marker-ring-symbol) xref--marker-ring)
                    (error nil))))

      (add-hook 'wg-after-switch-to-workgroup-hook
                (lambda ()
                  (condition-case nil
                      (synelics-wg//set-marker-ring 'xref--marker-ring (symbol-value (synelics-wg//get-wg-marker-ring-symbol)))
                    (error
                     (xref-clear-marker-stack)))
                  (setq tags-table-list nil)))

      ;; keymap
      (dotimes (i 10)
        (define-key window-numbering-keymap
          (kbd (format "M-%s" i))
          (intern (format "wg-switch-to-workgroup-at-index-%s" (- i 1)))))

      (spacemacs/declare-prefix "pw" "work group")
      (evil-leader/set-key
        "pwc" 'wg-create-workgroup
        "pws" 'wg-switch-to-workgroup
        "pwr" 'wg-rename-workgroup
        "pww" 'wg-save-session
        "pwk" 'wg-kill-workgroup))))
