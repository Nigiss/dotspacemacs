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

(setq synelics-keybind-packages '(
                               ivy
                              ))

(defun synelics-keybind/post-init-ivy ()
  (use-package ivy
    :defer t
    :config
    (progn
      (define-key ivy-minibuffer-map (kbd "ESC ESC") 'evil-escape)

      (define-key ivy-minibuffer-map (kbd "C-t") 'spacemacs/ivy-transient-state/body)
      (define-key ivy-minibuffer-map (kbd "C-o") 'ivy-call)
      (define-key ivy-minibuffer-map (kbd "C-d") 'ivy-scroll-up-command)
      (define-key ivy-minibuffer-map (kbd "C-u") 'ivy-scroll-down-command))))
