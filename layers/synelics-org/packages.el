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
        calfw
        calfw-org
        calfw-ical
        ))

(defun synelics-org/post-init-org ()
  (use-package org
    :defer t
    :init
    (setq-default org-use-sub-superscripts nil)
    (setq org-src-fontify-natively t)
    (setq org-startup-indented t)

    (with-eval-after-load 'org
      (spacemacs|disable-company 'org-mode)
      (evil-define-key 'normal org-mode-map (kbd "o")
        #'(lambda ()
            (interactive)
            (evil-append-line 1)
            (org-return)))
      (evil-define-key 'normal org-mode-map (kbd "TAB") 'tab-to-tab-stop)
      (evil-define-key 'normal org-mode-map (kbd "t") 'org-todo)
      (evil-define-key 'normal org-agenda-mode-map (kbd "t") 'org-todo)

      (add-hook 'org-mode-hook 'auto-fill-mode)
      (add-hook 'org-mode-hook
                (lambda () (pangu-spacing-mode -1)))
      (add-hook 'org-after-todo-state-change-hook
                (lambda ()
                  (if (string-equal org-state "DONE")
                      (org-pomodoro-finished))))

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
                              "NEXT(n)"
                              "PENDING(e)"
                              "REVIEW(r)"
                              "TEST(s)"
                              "|"
                              "REDUNDANCY(y!)"
                              "DONE(d!/!)"
                              "DONE-WITH-LOG(o@/!)"
                              "DELEGATED(l!)"
                              "CANCELLED(c@/!)")))
            org-todo-repeat-to-state "NEXT")

      (setq org-todo-keyword-faces
            (quote (("NEXT" :inherit warning)
                    ("PROJECT" :inherit font-lock-string-face))))

      ;; capture
      (setq org-default-notes-file "~/Library/Mobile Documents/com~apple~CloudDocs/org/notes/2019-Q4.org")
      (setq org-capture-templates
            '(("t" "Todo" entry (file "~/Library/Mobile Documents/com~apple~CloudDocs/org/notes/2019-Q4.org")
               "* TODO \n  CAPTURED: %u")
              ("d" "Diary" entry (file "~/Library/Mobile Documents/com~apple~CloudDocs/org/diary.org")
               "* %?\n" :clock-in t :clock-resume t)
              ("w" "Wiki" entry (file "~/Library/Mobile Documents/com~apple~CloudDocs/org/wiki.org")
               "* %?\n" :clock-in t :clock-resume t)
              ("i" "Idea" entry (file "~/Library/Mobile Documents/com~apple~CloudDocs/org/idea.org")
               "* %? :IDEA:")
              ("e" "Dev" entry (file "~/Library/Mobile Documents/com~apple~CloudDocs/org/dev.org")
               "* %?\n  CAPTURED: %u")))

      ;; agenda
      (setq org-agenda-files '("~/Library/Mobile Documents/com~apple~CloudDocs/org/notes"
                               "~/Library/Mobile Documents/com~apple~CloudDocs/org/communication"
                               "~/Library/Mobile Documents/com~apple~CloudDocs/org/summary"
                               "~/Library/Mobile Documents/com~apple~CloudDocs/org/meeting"
                               ))
      (setq org-agenda-start-on-weekday 1)
      (defun syenlics//work-of-week-match (tags offset-of-week)
        (interactive)
        (let* ((time (synelics-org//time-add (current-time) 'day (* offset-of-week 7)))
               (beg (synelics-org//beginning-of-week time))
               (end (synelics-org//time-add (synelics-org//end-of-week time) 'day 1))
               (format-string "[%.4d-%.2d-%.2d]"))
          (format "TODO=\"DONE\"+CLOSED>=\"%s\"+CLOSED<=\"%s\"%s"
                  (synelics-org//format-time beg format-string)
                  (synelics-org//format-time end format-string)
                  tags)))
      (defun syenlics//last-week-work-match (tags)
        (syenlics//work-of-week-match tags -1))
      (defun syenlics//current-week-work-match (tags)
        (syenlics//work-of-week-match tags 0))
      (setq org-agenda-custom-commands
            '(("w" "Works in last week."
               ((tags (syenlics//current-week-work-match "-FIX-OPT")
                      ((org-agenda-overriding-header "Feature: ")))
                (tags (syenlics//current-week-work-match "+OPT")
                      ((org-agenda-overriding-header "Opt: ")))
                (tags (syenlics//current-week-work-match "+FIX")
                      ((org-agenda-overriding-header "Fix: ")))))
              ("W" "Works in last week."
               ((tags (syenlics//last-week-work-match "-FIX-OPT")
                      ((org-agenda-overriding-header "Feature: ")))
                (tags (syenlics//last-week-work-match "+OPT")
                      ((org-agenda-overriding-header "Opt: ")))
                (tags (syenlics//last-week-work-match "+FIX")
                      ((org-agenda-overriding-header "Fix: ")))))
              ("t" "All high level todos."
               ((tags-todo "LEVEL=1+PRIORITY<=\"B\"" ((org-agenda-overriding-header "Todo: ")))))
              ("T" "All todos."
               ((tags-todo "LEVEL=1" ((org-agenda-overriding-header "Todo: ")))))))

      (advice-add 'org-agenda-goto :after
                  (lambda ()
                    (org-narrow-to-subtree)))

      (spacemacs|disable-company 'org-agenda-mode)
      (add-hook 'org-agenda-mode-hook (lambda () (company-mode -1)))

      ;; Fix editing org-table' bug
      ;; org-self-insert-command: Invalid function: org-table-with-shrunk-field
      ;; (require 'org-macs)

      ;; keymap
      (spacemacs/declare-prefix "aox" "Customed")
      (evil-leader/set-key
        "aoxc" 'cfw:open-org-calendar
        "aoxw" (synelics-core/curry-interactive #'org-agenda nil "w")
        "aoxW" (synelics-core/curry-interactive #'org-agenda nil "W")
        "aoxt" (synelics-core/curry-interactive #'org-agenda nil "t")
        "aoxT" (synelics-core/curry-interactive #'org-agenda nil "T"))
      )))

(defun synelics-org/init-calfw ()
  (use-package calfw))

(defun synelics-org/init-calfw-org ()
  (use-package calfw-org
    :config
    (define-key cfw:calendar-mode-map (kbd "c") 'org-capture)
    (define-key cfw:calendar-mode-map (kbd "j") 'next-line)
    (define-key cfw:calendar-mode-map (kbd "k") 'previous-line)
    (define-key cfw:calendar-mode-map (kbd "C-]") 'synelics-org/jump-item)))
