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
        xterm-color
        with-editor
        ))

(defun synelics-shell/post-init-term ()
  (use-package term
    :init
    (progn
      (evil-define-key 'normal term-raw-map "p" 'term-paste)
      (evil-define-key 'normal term-raw-map (kbd "RET") 'term-send-return))

    :config
    (mapcar (lambda (char)
              (evil-define-key 'emacs term-raw-map (kbd char) 'term-send-raw)
              (evil-define-key 'emacs term-raw-map (kbd (concat "C-" char)) 'term-send-raw))

            ;; [a-z]
            (cl-loop for char-code-of-a from 97 to (+ 97 25)
                     collect (make-string 1 char-code-of-a)))

    (evil-define-key 'emacs term-raw-map (kbd "C-z") 'evil-normal-state)
    (evil-define-key 'emacs term-raw-map (kbd "ESC") 'term-send-raw)
    (evil-define-key 'emacs term-raw-map (kbd "<backspace>") (lambda ()
                                                               "Backward kill char in term mode."
                                                               (interactive)
                                                               (term-send-raw-string "\C-h")))))

(defun shell/post-init-shell ()
  (evil-define-key 'insert comint-mode-map (kbd "RET") 'comint-send-input)
  (evil-define-key 'insert comint-mode-map (kbd "<tab>") 'company-complete-selection)
  (evil-define-key 'insert comint-mode-map (kbd "C-l") 'company-complete-selection)
  (evil-define-key 'insert comint-mode-map (kbd "C-j") 'company-select-next)
  (evil-define-key 'insert comint-mode-map (kbd "C-k") 'company-select-previous))

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
      (define-key (current-global-map)
        [remap async-shell-command] 'with-editor-async-shell-command)
      (define-key (current-global-map)
        [remap shell-command] 'with-editor-shell-command))))
