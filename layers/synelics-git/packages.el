;; ;;; packages.el --- synelics-git Layer packages File for Spacemacs
;; ;;
;; ;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;; ;;
;; ;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; ;; URL: https://github.com/syl20bnr/spacemacs
;; ;;
;; ;; This file is not part of GNU Emacs.
;; ;;
;; ;;; License: GPLv3

(setq synelics-git-packages
      '(
        magit
        ))

(defun synelics-git/post-init-magit ()
  (use-package magit
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys
        "gc" 'magit-checkout
        "gp" 'magit-push-current-to-pushremote
        "gP" (lambda ()
               (interactive)
               (magit-push-current-to-pushremote '("--force-with-lease")))
        "gf" 'magit-pull-from-pushremote))
    :config
    (progn
      (evil-define-key 'normal magit-status-mode-map (kbd "L") 'magit-log-popup)
      (evil-define-key 'visual magit-status-mode-map (kbd "L") 'magit-log-popup)
      (evil-define-key 'motion magit-status-mode-map (kbd "L") 'magit-log-popup)
      (evil-define-key 'operator magit-status-mode-map (kbd "L") 'magit-log-popup)

      (evil-define-key 'normal magit-status-mode-map (kbd "L") 'magit-log-popup)
      (evil-define-key 'visual magit-status-mode-map (kbd "L") 'magit-log-popup)
      (evil-define-key 'motion magit-status-mode-map (kbd "L") 'magit-log-popup)
      (evil-define-key 'operator magit-status-mode-map (kbd "L") 'magit-log-popup)

      (evil-define-key 'normal magit-status-mode-map (kbd "H") 'magit-dispatch-popup)
      (evil-define-key 'visual magit-status-mode-map (kbd "H") 'magit-dispatch-popup)
      (evil-define-key 'motion magit-status-mode-map (kbd "H") 'magit-dispatch-popup)
      (evil-define-key 'operator magit-status-mode-map (kbd "H") 'magit-dispatch-popup)

      (evil-define-key 'normal magit-status-mode-map (kbd "E") 'magit-ediff-dwim)
      (evil-define-key 'visual magit-status-mode-map (kbd "E") 'magit-ediff-dwim)
      (evil-define-key 'motion magit-status-mode-map (kbd "E") 'magit-ediff-dwim)
      (evil-define-key 'operator magit-status-mode-map (kbd "E") 'magit-ediff-dwim)

      (evil-define-key 'normal magit-status-mode-map (kbd "W") 'magit-am-popup)
      (evil-define-key 'visual magit-status-mode-map (kbd "W") 'magit-am-popup)
      (evil-define-key 'motion magit-status-mode-map (kbd "W") 'magit-am-popup)
      (evil-define-key 'operator magit-status-mode-map (kbd "W") 'magit-am-popup)

      (evil-define-key 'normal magit-status-mode-map (kbd "B") 'magit-branch-popup)
      (evil-define-key 'visual magit-status-mode-map (kbd "B") 'magit-branch-popup)
      (evil-define-key 'motion magit-status-mode-map (kbd "B") 'magit-branch-popup)
      (evil-define-key 'operator magit-status-mode-map (kbd "B") 'magit-branch-popup)

      ;; (evilified-state-evilify-map magit-status-mode-map
      ;;   :mode magit-status-mode
      ;;   :bindings
      ;;   (kbd "L") 'magit-log-popup
      ;;   (kbd "H") 'magit-dispatch-popup
      ;;   (kbd "E") 'magit-ediff-dwim
      ;;   (kbd "W") 'magit-am-popup
      ;;   (kbd "B") 'magit-branch-popup

      ;;   (kbd "l") 'evil-forward-char
      ;;   (kbd "h") 'evil-backward-char
      ;;   (kbd "e") 'evil-forward-word-end
      ;;   (kbd "w") 'evil-forward-word-begin
      ;;   (kbd "b") 'evil-backward-word-begin

      ;;   )
      )))
