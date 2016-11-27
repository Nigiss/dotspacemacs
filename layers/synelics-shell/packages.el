;; ;;; packages.el --- synelics-shell Layer packages File for Spacemacs
;; ;;
;; ;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;; ;;
;; ;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; ;; URL: https://github.com/syl20bnr/spacemacs
;; ;;
;; ;; This file is not part of GNU Emacs.
;; ;;
;; ;;; License: GPLv3

(setq synelics-shell-packages
      '(
        term
        (shell :location built-in)
        eshell
        xterm-color
        with-editor
        ))

(defun synelics-shell/post-init-term ()
  (use-package term
    :init
    (progn
      (evil-define-key 'normal term-raw-map "p" 'term-paste)
      (evil-define-key 'normal term-raw-map (kbd "RET") 'term-send-return)

      (evil-define-key 'insert term-raw-map (kbd "'") 'term-send-raw)
      (evil-define-key 'insert term-raw-map (kbd "C-w") 'term-send-raw)
      (evil-define-key 'insert term-raw-map (kbd "C-k") 'term-send-raw)
      (evil-define-key 'insert term-raw-map (kbd "C-p") 'term-send-raw)
      (evil-define-key 'insert term-raw-map (kbd "C-n") 'term-send-raw)
      (evil-define-key 'insert term-raw-map (kbd "C-a") 'term-send-raw)
      (evil-define-key 'insert term-raw-map (kbd "C-e") 'term-send-raw)
      (evil-define-key 'insert term-raw-map (kbd "C-d") 'term-send-raw)
      (evil-define-key 'insert term-raw-map (kbd "C-f") 'term-send-raw)
      (evil-define-key 'insert term-raw-map (kbd "C-b") 'term-send-raw)
      (evil-define-key 'insert term-raw-map (kbd "C-r") 'term-send-raw)
      (evil-define-key 'insert term-raw-map (kbd "C-s") 'term-send-raw)
      (evil-define-key 'insert term-raw-map (kbd "C-x") 'term-send-raw)
      (evil-define-key 'insert term-raw-map (kbd "M-DEL") 'term-send-raw)
      (evil-define-key 'insert term-raw-map (kbd "TAB") 'term-send-raw)
      (evil-define-key 'insert term-raw-map (kbd "DEL") 'term-send-raw))))

(defun shell/post-init-shell ()
  (synelics-core|local-set-key 'shell-mode company-active-map "RET" 'comint-send-input))

(defun synelics-shell/post-init-eshell ()
  (use-package eshell
    :init
    (progn
      )
    :config
    (progn
      (setq-default dotspacemacs-configuration-layers
                    '((shell :variables shell-default-shell 'eshell)
                      (shell :variables shell-enable-smart-eshell t)))

      (setq-default dotspacemacs-configuration-layers
                    '((shell :variables shell-enable-smart-eshell t))))))

(defun synelics-shell/post-init-xterm-color ()
  (use-package xterm-color
    :init
    (progn
      (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
      (setq comint-output-filter-functions
            (remove 'ansi-color-process-output comint-output-filter-functions))
      (add-hook 'comint-preoutput-filter-functions
                (lambda (string)
                  "Remove default bg-color in shell."
                  (replace-regexp-in-string ";40" "" string))))
    :config
    (progn
      (setq font-lock-unfontify-region-function 'xterm-color-unfontify-region))))

(defun synelics-shell/init-with-editor ()
  (use-package with-editor
    :init
    (progn
      (add-hook 'shell-mode-hook 'with-editor-export-git-editor))
    :config
    (progn
      (with-eval-after-load 'projectile
        (lambda ()
          (define-key evil-normal-state-map (kbd "px") 'projectile-run-shell)))

      (define-key (current-global-map)
        [remap async-shell-command] 'with-editor-async-shell-command)
      (define-key (current-global-map)
        [remap shell-command] 'with-editor-shell-command))))
