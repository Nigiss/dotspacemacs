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

(setq dk-reader-packages
      '(
        sgml-mode
        projectile
        polymode
        js2-mode
        ))

(defun dk-reader/post-init-sgml-mode ()
  (use-package sgml-mode
    :defer t
    :mode ("\\.tpl\\'" . sgml-mode)
    :init
    (progn
      (synelics-core|add-hook 'after-save
                              (lambda ()
                                (and
                                 (synelics-work/in-directory-p "dk-reader")
                                 (string-equal (file-name-extension (buffer-file-name)) "tpl")
                                 (shell-command (concat "~/kits/bin/tpl " buffer-file-name " > /dev/null")))))

      (synelics-core|add-hook 'sgml-mode
                              (lambda ()
                                (and
                                 (synelics-work/in-directory-p "dk-reader")
                                 (set (make-variable-buffer-local 'sgml-basic-offset) 4)))))))

(defun dk-reader/post-init-projectile ()
  (use-package projectile
    :defer t
    :config
    (progn
      (spacemacs/declare-prefix "mx" "work commands")
      (with-eval-after-load 'evil
        (evil-leader/set-key
          "mxu" 'synelics-core/update-tags-table

          "mxt" 'synelics-work/phone-test
          "mxs" 'synelics-work/phone-sync
          "mxa" 'synelics-work/phone-alpha
          "mxp" 'synelics-work/phone-publish
          "mxh" 'synelics-work/phone-hotfix
          "mxd" 'synelics-work/phone-dev

          "mxi" 'synelics-work/server-start-staging
          "mxv" 'synelics-work/server-start-preview)))))

(defun dk-reader/init-polymode ()
  (use-package polymode
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.vp" . poly-vp-mode)))
    :config
    (progn
      (defcustom work/vp-host
        (pm-bchunkmode "JS mode"
                       :mode 'js2-mode)
        "Html host innermode"
        :group 'hostmodes
        :type 'object)

      (defcustom  work/vp-inner-html
        (pm-hbtchunkmode "Vp"
                         :head-reg "^[ \t]*{{#[^}]+}}"
                         :tail-reg "^[ \t]*{{#}}"
                         :head-mode 'host
                         :tail-mode 'host
                         :mode 'sgml-mode
                         :indent-offset 0
                         :font-lock-narrow t)
        "Vp typical chunk."
        :group 'innermodes
        :type 'object)

      (defcustom  work/vp-inner-css
        (pm-hbtchunkmode "Vp"
                         :head-reg "^[ \t]*{{![^}]+}}"
                         :tail-reg "^[ \t]*{{!}}"
                         :head-mode 'host
                         :tail-mode 'host
                         :mode 'css-mode
                         :indent-offset 0
                         :font-lock-narrow t)
        "Vp typical chunk."
        :group 'innermodes
        :type 'object)

      (defcustom work/vp-poly
        (pm-polymode-multi "Vp"
                           :hostmode 'work/vp-host
                           :innermodes '(work/vp-inner-html work/vp-inner-css))
        "Vp typical configuration"
        :group 'polymodes
        :type 'object)

      (define-polymode poly-vp-mode work/vp-poly))))

(defun dk-reader/post-init-js2-mode ()
  (use-package js2-mode
    :defer t
    :init
    (add-hook 'js2-mode-hook
              (lambda ()
                (if (synelics-work/in-directory-p "dk-reader")
                    (progn
                      (set (make-variable-buffer-local 'js-indent-level) 4)
                      (set (make-variable-buffer-local 'flycheck-disabled-checkers) '(javascript-standard))
                      ))
                (if (synelics-work/in-directory-p "dk-reader/app-active-2016-10-01")
                    (set (make-variable-buffer-local 'flycheck-enabled-checkers) '(javascript-eslint))))
              'append)))
