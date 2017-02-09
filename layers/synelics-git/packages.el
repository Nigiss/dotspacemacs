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
      (add-hook 'magit-mode-hook (lambda ()
                                   (visual-line-mode 1)))
      (spacemacs/set-leader-keys
        "gk" 'magit-checkout
        "gx" 'magit-reset-hard
        "gX" (lambda ()
               (interactive)
               (magit-reset-hard (magit-get-current-branch)))
        "gS" (lambda ()
               (interactive)
               (magit-stage-modified 'all))
        "gc" 'magit-commit
        "gd" 'magit-diff-unstaged
        "gD" 'magit-diff-staged
        "gp" 'magit-push-current-to-pushremote
        "go" 'magit-log-current
        "gy" 'magit-show-refs-popup
        "gP" (lambda ()
               (interactive)
               (magit-push-current-to-pushremote '("--force-with-lease")))
        "gf" 'magit-pull-from-pushremote))
    :config
    (progn
      (evil-define-key 'normal magit-status-mode-map (kbd "q") (lambda ()
                                                                 (interactive)
                                                                 (magit-mode-bury-buffer 'kill-buffer)))

      (dolist (git-map (list magit-status-mode-map magit-log-mode-map magit-diff-mode-map))
        (evil-define-key 'normal git-map (kbd "<escape>") 'evil-force-normal-state)
        (evil-define-key 'visual git-map (kbd "<escape>") 'evil-force-normal-state)
        (evil-define-key 'motion git-map (kbd "<escape>") 'evil-force-normal-state)
        (evil-define-key 'operator git-map (kbd "<escape>") 'evil-force-normal-state)

        (evil-define-key 'normal git-map (kbd "0") 'mwim-beginning-of-code-or-line)
        (evil-define-key 'visual git-map (kbd "0") 'mwim-beginning-of-code-or-line)
        (evil-define-key 'motion git-map (kbd "0") 'mwim-beginning-of-code-or-line)
        (evil-define-key 'operator git-map (kbd "0") 'mwim-beginning-of-code-or-line)

        (evil-define-key 'normal git-map (kbd "L") 'magit-log-popup)
        (evil-define-key 'visual git-map (kbd "L") 'magit-log-popup)
        (evil-define-key 'motion git-map (kbd "L") 'magit-log-popup)
        (evil-define-key 'operator git-map (kbd "L") 'magit-log-popup)

        (evil-define-key 'normal git-map (kbd "l") 'evil-forward-char)
        (evil-define-key 'visual git-map (kbd "l") 'evil-forward-char)
        (evil-define-key 'motion git-map (kbd "l") 'evil-forward-char)
        (evil-define-key 'operator git-map (kbd "l") 'evil-forward-char)

        (evil-define-key 'visual git-map (kbd "H") 'magit-dispatch-popup)
        (evil-define-key 'normal git-map (kbd "H") 'magit-dispatch-popup)
        (evil-define-key 'motion git-map (kbd "H") 'magit-dispatch-popup)
        (evil-define-key 'operator git-map (kbd "H") 'magit-dispatch-popup)

        (evil-define-key 'visual git-map (kbd "h") 'evil-backward-char)
        (evil-define-key 'normal git-map (kbd "h") 'evil-backward-char)
        (evil-define-key 'motion git-map (kbd "h") 'evil-backward-char)
        (evil-define-key 'operator git-map (kbd "h") 'evil-backward-char)

        (evil-define-key 'normal git-map (kbd "E") 'magit-ediff-dwim)
        (evil-define-key 'visual git-map (kbd "E") 'magit-ediff-dwim)
        (evil-define-key 'motion git-map (kbd "E") 'magit-ediff-dwim)
        (evil-define-key 'operator git-map (kbd "E") 'magit-ediff-dwim)

        (evil-define-key 'normal git-map (kbd "e") 'evil-forward-word-end)
        (evil-define-key 'visual git-map (kbd "e") 'evil-forward-word-end)
        (evil-define-key 'motion git-map (kbd "e") 'evil-forward-word-end)
        (evil-define-key 'operator git-map (kbd "e") 'evil-forward-word-end)

        (evil-define-key 'normal git-map (kbd "W") 'magit-am-popup)
        (evil-define-key 'visual git-map (kbd "W") 'magit-am-popup)
        (evil-define-key 'motion git-map (kbd "W") 'magit-am-popup)
        (evil-define-key 'operator git-map (kbd "W") 'magit-am-popup)

        (evil-define-key 'normal git-map (kbd "w") 'evil-forward-word-begin)
        (evil-define-key 'visual git-map (kbd "w") 'evil-forward-word-begin)
        (evil-define-key 'motion git-map (kbd "w") 'evil-forward-word-begin)
        (evil-define-key 'operator git-map (kbd "w") 'evil-forward-word-begin)
        (evil-define-key 'normal git-map (kbd "B") 'magit-branch-popup)
        (evil-define-key 'visual git-map (kbd "B") 'magit-branch-popup)
        (evil-define-key 'motion git-map (kbd "B") 'magit-branch-popup)
        (evil-define-key 'operator git-map (kbd "B") 'magit-branch-popup)

        (evil-define-key 'normal git-map (kbd "b") 'evil-backward-word-begin)
        (evil-define-key 'visual git-map (kbd "b") 'evil-backward-word-begin)
        (evil-define-key 'motion git-map (kbd "b") 'evil-backward-word-begin)
        (evil-define-key 'operator git-map (kbd "b") 'evil-backward-word-begin)

        (evil-define-key 'normal git-map (kbd "v") 'evil-visual-char)
        (evil-define-key 'visual git-map (kbd "v") 'evil-visual-char)
        (evil-define-key 'motion git-map (kbd "v") 'evil-visual-char)
        (evil-define-key 'operator git-map (kbd "v") 'evil-visual-char)

        ;; (evil-define-key 'normal git-map (kbd "i") 'undefined)
        ;; (evil-define-key 'visual git-map (kbd "i") 'undefined)
        ;; (evil-define-key 'motion git-map (kbd "i") 'undefined)
        ;; (evil-define-key 'operator git-map (kbd "i") 'undefined)

        ;; (evil-define-key 'normal git-map (kbd "a") 'undefined)
        ;; (evil-define-key 'visual git-map (kbd "a") 'undefined)
        ;; (evil-define-key 'motion git-map (kbd "a") 'undefined)
        ;; (evil-define-key 'operator git-map (kbd "a") 'undefined)

        (evil-define-key 'normal git-map (kbd "Y") 'magit-show-refs-popup)
        (evil-define-key 'visual git-map (kbd "Y") 'magit-show-refs-popup)
        (evil-define-key 'motion git-map (kbd "Y") 'emagit-show-refs-popup)
        (evil-define-key 'operator git-map (kbd "Y") 'magit-show-refs-popup)

        (evil-define-key 'normal git-map (kbd "y") 'evil-yank)
        (evil-define-key 'visual git-map (kbd "y") 'evil-yank)
        (evil-define-key 'motion git-map (kbd "y") 'evil-yank)
        (evil-define-key 'operator git-map (kbd "y") 'evil-yank)))))
