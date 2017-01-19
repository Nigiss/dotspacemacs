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

(setq synelics-org-packages
      '(
        org
        ))

(defun synelics-org/post-init-org ()
  (use-package org
    :defer t
    :init
    (progn
      ;; (add-hook 'org-mode-hook 'spacemacs/toggle-spelling-checking-off)
      (add-hook 'org-mode-hook 'auto-fill-mode)
      (spacemacs|disable-company 'org-mode)
      (spacemacs|disable-company 'org-agenda-mode)
      (add-hook 'org-agenda-mode-hook (lambda () (company-mode -1)))
      (add-hook 'org-pomodoro-started-hook (lambda () (org-todo "DOING")))
      (add-hook 'org-pomodoro-killed-hook (lambda () (org-todo "TODO")))
      (add-hook 'org-after-todo-state-change-hook
                (lambda ()
                  (if (string-equal org-state "DONE")
                      (org-pomodoro-finished))))
      (add-hook 'org-mode-hook
                (lambda ()
                  (define-key evil-normal-state-local-map (kbd "o")
                    (lambda ()
                      (interactive)
                      (evil-append-line 1)
                      (org-return)))
                  (define-key evil-insert-state-local-map (kbd "TAB") 'tab-to-tab-stop)))
      (setq org-log-done t
            org-edit-timestamp-down-means-later t
            org-archive-mark-done nil
            org-catch-invisible-edits 'show
            org-export-coding-system 'utf-8
            org-fast-tag-selection-single-key 'expert
            org-html-validation-link nil
            org-export-kill-product-buffer-when-displayed t
            org-tags-column 80)


      ;; org priority
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "r" 'org-priority)
      (setq org-highest-priority ?A)
      (setq org-lowest-priority ?E)
      (setq org-default-priority ?C)
      (setq org-priority-faces '((?A . (:foreground "#F0DFAF" :weight bold))
                                 (?B . (:foreground "#FFA800" :weight bold))
                                 (?C . (:foreground "LightSteelBlue"))
                                 (?D . (:foreground "OliveDrab"))
                                 (?E . (:foreground "brightgreen"))))

      (setq org-todo-keywords
            (quote ((sequence "TODO(t)"
                              "DOING(n)"
                              "PENDING(e)"
                              "REVIEW(r)"
                              "TEST(s)"
                              "PUBLISH(p)"
                              "|"
                              "DONE(d!/!)"
                              "DONE-WITH-LOG(o@/!)"
                              "DELEGATED(l!)"
                              "CANCELLED(c@/!)")))
            org-todo-repeat-to-state "NEXT")

      (setq org-todo-keyword-faces
            (quote (("NEXT" :inherit warning)
                    ("PROJECT" :inherit font-lock-string-face))))

      ;; capture
      (setq org-default-notes-file "~/Documents/org/notes.org")
      (setq org-capture-templates
            '(("t" "Todo" entry (file org-default-notes-file)
               "* TODO \n  CAPTURED: %u")
              ("d" "Diary" entry (file "~/Documents/org/diary.org")
               "* %?\n" :clock-in t :clock-resume t)
              ("i" "Idea" entry (file org-default-notes-file)
               "* %? :IDEA:")))

      ;; agenda
      (setq org-agenda-files '("~/Documents/org/"))
      (setq org-agenda-start-on-weekday 1)
      (setq org-agenda-custom-commands
            '(("w" "Works in last week."
               ((tags (let* ((time (current-time))
                             (beg (synelics-org//beginning-of-week time))
                             (end (synelics-org//time-add (synelics-org//end-of-week time) 'day 1))
                             (format-string "[%.4d-%.2d-%.2d]"))
                        (format "TODO=\"DONE\"+CLOSED>=\"%s\"+CLOSED<=\"%s\""
                                (synelics-org//format-time beg format-string)
                                (synelics-org//format-time end format-string))))))
              ("t" "All todos."
               ((tags-todo "LEVEL=1")))))

      (evil-leader/set-key
        "aow" (synelics-core/curry-interactive #'org-agenda nil "w")
        "aot" (synelics-core/curry-interactive #'org-agenda nil "t")))
    :config
    (progn
      )))
