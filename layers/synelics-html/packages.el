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
        css-mode
        evil-matchit
        ))

(defun synelics-html/init-sgml-mode ()
  (use-package sgml-mode
    :defer t
    :mode ("\\.tpl\\'" . html-mode)
    :init
    (progn
      (setq-default sgml-basic-offset 4)

      ;; https://www.emacswiki.org/emacs/EmacsSyntaxTable
      (add-hook 'html-mode-hook
                (lambda ()
                  (modify-syntax-entry ?. ".")
                  (modify-syntax-entry ?= ".")))

      (add-hook 'sgml-mode-hook 'paredit-mode)
      ;; (add-hook 'html-mode-hook 'spacemacs/toggle-spelling-checking-off)
      (add-hook 'html-mode-hook
                (lambda ()
                  (add-hook 'after-save-hook
                            (lambda ()
                              (and
                               (string-equal (file-name-extension (buffer-file-name)) "tpl")
                               (shell-command (concat "~/browser-fe/common/build/tpl.py " buffer-file-name " > /dev/null"))))))))))

(defun synelics-html/post-init-css-mode ()
  (use-package css-mode
    :defer t
    :init
    (progn
      (add-hook 'css-mode-hook 'paredit-mode))))

(defun synelics-html/post-init-evil-matchit ()
  (use-package evil-matchit
    :defer t
    :init
    (progn
      (spacemacs/add-to-hooks 'turn-on-evil-matchit-mode '(html-mode-hook))
      (spacemacs/add-to-hooks 'turn-off-evil-matchit-mode '(css-mode-hook)))))
