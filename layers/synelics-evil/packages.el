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

(setq synelics-evil-packages '(
                               evil
                               paredit
                               paredit-everywhere
                               ))

(defun synelics-evil/post-init-evil ()
  (progn
    ;; Initialization
    (setq-default evil-escape-key-sequence "C-c C-g")

    ;; Edit
    (define-key evil-insert-state-map (kbd "C-w") 'evil-delete-backward-word)
    (define-key evil-insert-state-map (kbd "C-h") 'paredit-backward-delete)
    (define-key evil-visual-state-map (kbd "Q") 'anzu-query-replace)

    ;; Move
    (define-key evil-normal-state-map (kbd "0") 'evil-first-non-blank)
    (define-key evil-motion-state-map (kbd "0") 'evil-first-non-blank)
    (define-key evil-visual-state-map (kbd "0") 'evil-first-non-blank)
    (define-key evil-operator-state-map (kbd "0") 'evil-first-non-blank)

    (define-key evil-normal-state-map (kbd "^") 'evil-beginning-of-line)
    (define-key evil-motion-state-map (kbd "^") 'evil-beginning-of-line)
    (define-key evil-visual-state-map (kbd "^") 'evil-beginning-of-line)
    (define-key evil-operator-state-map (kbd "^") 'evil-beginning-of-line)

    (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
    (define-key evil-motion-state-map (kbd "j") 'evil-next-visual-line)
    (define-key evil-visual-state-map (kbd "j") 'evil-next-visual-line)
    (define-key evil-operator-state-map (kbd "j") 'evil-next-visual-line)

    (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
    (define-key evil-motion-state-map (kbd "k") 'evil-previous-visual-line)
    (define-key evil-visual-state-map (kbd "k") 'evil-previous-visual-line)
    (define-key evil-operator-state-map (kbd "k") 'evil-previous-visual-line)

    (define-key evil-normal-state-map (kbd "gj") 'evil-next-line)
    (define-key evil-motion-state-map (kbd "gj") 'evil-next-line)
    (define-key evil-visual-state-map (kbd "gj") 'evil-next-line)
    (define-key evil-operator-state-map (kbd "gj") 'evil-next-lin)

    (define-key evil-normal-state-map (kbd "gk") 'evil-previous-line)
    (define-key evil-motion-state-map (kbd "gk") 'evil-previous-line)
    (define-key evil-visual-state-map (kbd "gk") 'evil-previous-line)
    (define-key evil-operator-state-map (kbd "gk") 'evil-previous-line)

    (define-key evil-normal-state-map (kbd "{") 'evil-backward-section-begin)
    (define-key evil-motion-state-map (kbd "{") 'evil-backward-section-begin)
    (define-key evil-visual-state-map (kbd "{") 'evil-backward-section-begin)
    (define-key evil-operator-state-map (kbd "{") 'evil-backward-section-begin)

    (define-key evil-normal-state-map (kbd "}") 'evil-forward-section-end)
    (define-key evil-motion-state-map (kbd "}") 'evil-forward-section-end)
    (define-key evil-visual-state-map (kbd "}") 'evil-forward-section-end)
    (define-key evil-operator-state-map (kbd "}") 'evil-forward-section-end)

    (define-key evil-normal-state-map (kbd "g]") 'find-tag)
    (define-key evil-visual-state-map (kbd "g]") 'find-tag)

    ;; comment
    (define-key evil-normal-state-map (kbd "gm") 'evilnc-comment-operator)
    (define-key evil-visual-state-map (kbd "gm") 'evilnc-comment-operator)

    ;; register
    (define-key evil-normal-state-map (kbd "'") 'evil-use-register)
    (define-key evil-motion-state-map (kbd "'") 'evil-use-register)
    (define-key evil-visual-state-map (kbd "'") 'evil-use-register)
    (define-key evil-operator-state-map (kbd "'") 'evil-use-register)

    (define-key evil-normal-state-map (kbd "\"") 'evil-goto-mark-line)
    (define-key evil-motion-state-map (kbd "\"") 'evil-goto-mark-line)
    (define-key evil-visual-state-map (kbd "\"") 'evil-goto-mark-line)
    (define-key evil-operator-state-map (kbd "\"") 'evil-goto-mark-line)

    ;; Custom line object
    (defun evil-line-range (count beg end type &optional inclusive)
      (if inclusive
          (evil-range (line-beginning-position) (line-end-position))
        (let ((start (save-excursion
                       (back-to-indentation)
                       (point)))
              (end (save-excursion
                     (goto-char (line-end-position))
                     (skip-syntax-backward " " (line-beginning-position))
                     (point))))
          (evil-range start end))))

    (evil-define-text-object evil-a-line (count &optional beg end type)
      "Select range between a character by which the command is followed."
      (evil-line-range count beg end type t))
    (evil-define-text-object evil-inner-line (count &optional beg end type)
      "Select inner range between a character by which the command is followed."
      (evil-line-range count beg end type))

    (define-key evil-outer-text-objects-map "l" 'evil-a-line)
    (define-key evil-inner-text-objects-map "l" 'evil-inner-line)

    (define-key evil-outer-text-objects-map "k" 'evil-a-bracket)
    (define-key evil-inner-text-objects-map "k" 'evil-inner-bracket)

    (define-key evil-outer-text-objects-map "u" 'evil-a-curly)
    (define-key evil-inner-text-objects-map "u" 'evil-inner-curly)

    (define-key evil-outer-text-objects-map "m" 'evil-a-symbol)
    (define-key evil-inner-text-objects-map "m" 'evil-inner-symbol)

    (define-key evil-outer-text-objects-map "o" 'evilmi-outer-text-object)
    (define-key evil-inner-text-objects-map "o" 'evilmi-inner-text-object)))
