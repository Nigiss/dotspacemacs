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

            ;; sensitive
            ;; company-etags-ignore-case t
            completion-ignore-case t
            read-file-name-completion-ignore-case t
            read-buffer-completion-ignore-case t
            ;; tags-case-fold-search nil
            )

      (add-hook 'after-init-hook (lambda ()
                                   (interactive)
                                   (synelics-completion//use-styles)
                                   (synelics-completion//async-call-next)
                                   (global-company-mode)))

      (synelics-core|add-hooks '(git-commit-mode org-mode)
                               (lambda ()
                                 (set (make-local-variable 'company-idle-delay) nil))))
    :config
    (let ((map company-active-map))
      (define-key map (kbd "C-w") 'backward-kill-word)
      (define-key map (kbd "<return>") 'newline-and-indent)
      (define-key map (kbd "<tab>") (lambda ()
                                      (interactive)
                                      (if (or (not yas/minor-mode)
                                              (null (synelics-completion//do-yas-expand)))
                                          (company-complete-selection)))))))

(defun synelics-completion/init-company-flx ()
  (use-package company-flx
    :if (and (configuration-layer/package-used-p 'company))
    :defer t
    :init
    (company-flx-mode +1)

    ;; optimize completion for `emacs-lisp-mode'
    (advice-remove 'company-capf #'company-flx-company-capf-advice)
    (synelics-core|remove-from-list company-transformers 'company-flx-transformer)

    (setq completion-styles '(fuzzy))))

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
