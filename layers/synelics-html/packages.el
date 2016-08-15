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
      (with-eval-after-load 'company-css
        (defun synelics-css//all-completions (arg table)
          (let* ((candidates (completion-all-completions arg table nil (length arg)))
                 (last (last candidates))
                 (base-size (and (numberp (cdr last)) (cdr last))))
            (when base-size
              (setcdr last nil))
            (if (not (zerop (or base-size 0)))
                (let ((before (substring arg 0 base-size)))
                  (mapcar (lambda (candidate)
                            (concat before candidate))
                          candidates))
              candidates)))
        (defun company-css (command &optional arg &rest ignored)
          "Overwrite default."
          (interactive (list 'interactive))
          (cl-case command
            (interactive (company-begin-backend 'company-css))
            (prefix (and (or (derived-mode-p 'css-mode)
                             (and (derived-mode-p 'web-mode)
                                  (string= (web-mode-language-at-pos) "css")))
                         (or (company-grab company-css-tag-regexp 1)
                             (company-grab company-css-pseudo-regexp 1)
                             (company-grab company-css-property-value-regexp 2)
                             (company-css-grab-property))))
            (candidates
             (cond
              ((company-grab company-css-tag-regexp 1)
               (synelics-css//all-completions arg company-css-html-tags))
              ((company-grab company-css-pseudo-regexp 1)
               (synelics-css//all-completions arg company-css-pseudo-classes))
              ((company-grab company-css-property-value-regexp 2)
               (synelics-css//all-completions arg
                                              (company-css-property-values
                                               (company-grab company-css-property-value-regexp 1))))
              ((company-css-grab-property)
               (synelics-css//all-completions arg company-css-property-alist))))
            (sorted t)))))))

(defun synelics-html/post-init-evil-matchit ()
  (use-package evil-matchit
    :defer t
    :init
    (progn
      (spacemacs/add-to-hooks 'turn-on-evil-matchit-mode '(css-mode-hook
                                                           html-mode-hook)))))
