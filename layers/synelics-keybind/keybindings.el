;;; config.el --- OSX Layer keybindings File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(when (spacemacs/system-is-mac)
  ;; this is only applicable to GUI mode
  (when (display-graphic-p)
    ;; Keybindings
    (global-set-key (kbd "H-j") 'evil-avy-goto-char-2)
    (global-set-key (kbd "H-s") 'save-buffer)
    (global-set-key (kbd "H-g") 'goto-line)
    (global-set-key (kbd "H-k") 'kill-this-buffer)
    (global-set-key (kbd "H-o") 'wg-switch-to-workgroup)
    (global-set-key (kbd "H-i") 'wg-switch-to-previous-workgroup)
    (global-set-key (kbd "H-m") 'spacemacs/toggle-maximize-buffer)
    (global-set-key (kbd "H-f") 'counsel-find-file)
    (global-set-key (kbd "H-p") 'projectile-find-file)
    (global-set-key (kbd "H-x") 'counsel-M-x)))

(define-key minibuffer-local-shell-command-map (kbd "C-p") 'previous-line-or-history-element)
(define-key minibuffer-local-shell-command-map (kbd "C-n") 'next-line-or-history-element)
(define-key minibuffer-local-shell-command-map (kbd "C-k") 'previous-line-or-history-element)
(define-key minibuffer-local-shell-command-map (kbd "C-j") 'next-line-or-history-element)
(define-key read-expression-map (kbd "C-p") 'previous-line-or-history-element)
(define-key read-expression-map (kbd "C-n") 'next-line-or-history-element)
(define-key read-expression-map (kbd "C-k") 'previous-line-or-history-element)
(define-key read-expression-map (kbd "C-j") 'next-line-or-history-element)

