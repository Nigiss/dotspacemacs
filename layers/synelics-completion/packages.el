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

(setq synelics-completion-packages
      '(
        company
        company-flx

        ycmd

        yasnippet
        ))

(defun synelics-completion/post-init-company ()
  (use-package company
    :defer t
    :init
    (progn
      (setq tab-always-indent t

            ;; base
            company-idle-delay 0
            company-minimum-prefix-length 1
            completion-cycle-threshold 5
            company-echo-delay 1

            ;; etags
            company-etags-ignore-case t

            ;; sensitive
            completion-ignore-case t
            read-file-name-completion-ignore-case t
            read-buffer-completion-ignore-case t)

      (setq company-backends '(company-capf
                               company-files
                               (company-dabbrev-code company-keywords)
                               company-oddmuse company-dabbrev))

      (add-hook 'after-init-hook 'global-company-mode)

      (synelics-core|add-hooks '(git-commit-mode org-mode)
                               (lambda ()
                                 (set (make-local-variable 'company-idle-delay) nil)))

      (synelics-completion//force-company-completion-use-syltes))
    :config
    (progn

      (let ((map company-active-map))
        (define-key map (kbd "C-w") 'paredit-backward-kill-word)
        (define-key map (kbd "<return>") 'newline-and-indent)
        (define-key map (kbd "<tab>") 'company-complete-selection)))))

(defun synelics-completion/init-company-flx ()
  (use-package company-flx
    :defer t
    :init
    (progn
      (with-eval-after-load 'company
        (company-flx-mode +1)
        (add-to-list 'completion-styles 'fuzzy 'append)))))

(defun synelics-completion/post-init-yasnippet()
  (use-package yasnippet
    :defer t
    :config
    (progn
      (defun synelics-completion//do-yas-expand ()
        (let ((yas/fallback-behavior 'return-nil))
          (yas/expand)))

      (defun synelics-completion//tab-indent-or-complete ()
        (interactive)
        (if (minibufferp)
            (minibuffer-complete)
          (if (or (not yas/minor-mode)
                  (null (synelics-completion//do-yas-expand)))
              (indent-for-tab-command))))
        (define-key evil-insert-state-map (kbd "TAB") 'synelics-completion//tab-indent-or-complete))))

(defun synelics-completion/post-init-ycmd ()
  (use-package ycmd
    :defer t
    :init
    (progn
      (setq ycmd-server-command '("python"
                                  "/Users/synelics/.spacemacs.d/layers/synelics-completion/local/ycmd/ycmd")
            ycmd-force-semantic-completion t)
      (spacemacs|diminish ycmd-mode " Ⓜ" " M"))
    :config
    (progn
      (ad-enable-advice 'ycmd--handle-goto-success 'around 'synelics-completion//goto-current-project-only))))

(defun synelics-evil/init-paredit ()
  (use-package paredit
    :defer t
    :config
    (progn
      (define-key evil-insert-state-map (kbd "'")
        (lambda ()
          (interactive)
          (self-insert-command 1)))
      (define-key evil-insert-state-map (kbd "\"")
        (lambda ()
          (interactive)
          (self-insert-command 1)))
      (define-key evil-insert-state-map (kbd "(")
        (lambda ()
          (interactive)
          (self-insert-command 1)))
      (define-key evil-insert-state-map (kbd ")")
        (lambda ()
          (interactive)
          (self-insert-command 1)))
      (define-key evil-insert-state-map (kbd "[")
        (lambda ()
          (interactive)
          (self-insert-command 1)))
      (define-key evil-insert-state-map (kbd "]")
        (lambda ()
          (interactive)
          (self-insert-command 1)))
      (define-key evil-insert-state-map (kbd "{")
        (lambda ()
          (interactive)
          (self-insert-command 1)))
      (define-key evil-insert-state-map (kbd ";")
        (lambda ()
          (interactive)
          (self-insert-command 1)))
      (define-key evil-insert-state-map (kbd "}")
        (lambda ()
          (interactive)
          (self-insert-command 1))))))

(defun synelics-evil/init-paredit-everywhere ()
  (use-package paredit-everywhere
    :defer t
    :config
    (progn
      (paredit-everywhere-mode t))))
