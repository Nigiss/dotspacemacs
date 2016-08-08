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

(setq synelics-html-packages
      '(
        sgml-mode
        evil-matchit
        ))

(defun synelics-html/init-sgml-mode ()
  (use-package sgml-mode
    :defer t
    :mode ("\\.tpl\\'" . html-mode)
    :init
    (progn
      (setq-default sgml-basic-offset 4)
      (add-hook 'html-mode-hook 'evil-matchit-mode)

      ;; https://www.emacswiki.org/emacs/EmacsSyntaxTable
      (add-hook 'html-mode-hook
                (lambda ()
                  (modify-syntax-entry ?. ".")
                  (modify-syntax-entry ?= ".")))
      (add-hook 'html-mode-hook 'spacemacs/toggle-spelling-checking-off)
      (add-hook 'html-mode-hook
                (lambda ()
                  (add-hook 'after-save-hook
                            (lambda ()
                              (and
                               (string-equal (file-name-extension (buffer-file-name)) "tpl")
                               (shell-command (concat "~/browser-fe/common/build/tpl.py " buffer-file-name " > /dev/null"))))))))))

(defun synelics-html/init-evil-matchit ()
  (use-package evil-matchit
    :defer t
    :init
    (progn
      (add-hook 'html-mode-hook 'evil-matchit-mode))))
