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
        js2-mode
        polymode
        (js-mode :location built-in)
        (tern :toggle (spacemacs//tern-detect))
        company-tern :requires company
        ))

(defun dk-reader/init-sgml-mode ()
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
                                 (shell-command (concat "~/Works/dk-reader/frontend/kits/bin/tpl " buffer-file-name " > /dev/null")))))

      (add-hook 'sgml-mode-hook
                (lambda ()
                  (setq sgml-basic-offset 4)

                  ;; https://www.emacswiki.org/emacs/EmacsSyntaxTable
                  (modify-syntax-entry ?: ".")
                  (modify-syntax-entry ?. ".")
                  (modify-syntax-entry ?' ".")
                  (modify-syntax-entry ?- ".")
                  (modify-syntax-entry ?= ".")

                  (subword-mode 1))))))

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

(defun dk-reader/post-init-js2-mode ()
  (use-package js2-mode
    :defer t
    :init

    (add-hook 'js2-mode-hook
              (lambda ()
                (when (and (projectile-project-p)
                           (file-exists-p (concat (projectile-project-root) ".eslintrc.js")))
                  (set (make-variable-buffer-local 'js-indent-level) 4)
                  (poly-vp-mode +1))))))

(defun dk-reader/init-js-mode ()
  (use-package js-mode
    :defer t
    :init

    (synelics-core|add-hook 'js-mode
                            'subword-mode
                            'evil-matchit-mode)

    (add-hook 'js-mode-hook
              (lambda ()
                (let ((eslint-exec (and (projectile-project-p)
                                        (concat (projectile-project-root) "node_modules/eslint/bin/eslint.js"))))
                  (when (file-exists-p eslint-exec)
                    (set (make-variable-buffer-local 'js-indent-level) 4)
                    (flycheck-mode 1)
                    (set (make-variable-buffer-local 'flycheck-enabled-checkers) '(javascript-eslint))
                    (set (make-variable-buffer-local 'flycheck-javascript-eslint-executable) eslint-exec)))

                (define-key evil-normal-state-local-map (kbd "C-]") 'tern-find-definition)
                (define-key evil-normal-state-local-map (kbd "C-t") 'tern-pop-find-definition)))))

(defun dk-reader/post-init-company-tern ()
  (use-package company-tern
    :if (and (configuration-layer/package-used-p 'company)
             (configuration-layer/package-used-p 'tern))
    :defer t
    :init (spacemacs|add-company-backends
            :backends company-tern
            :modes js-mode)))

(defun dk-reader/post-init-tern ()
  (use-package tern
    :defer t
    :init (add-hook 'js-mode-hook 'tern-mode)))

(defun dk-reader/init-polymode ()
  (use-package polymode
    :init
    (add-to-list 'auto-mode-alist '("\\.mix$" . poly-mix-mode))
    (add-to-list 'auto-mode-alist '("\\.vue$" . poly-mix-mode))
    :config
    ;; vp
    (defcustom dk-reader//vp-host
      (pm-bchunkmode "JS mode"
                     :mode 'js2-mode)
      "Html host innermode"
      :group 'hostmodes
      :type 'object)

    (defcustom dk-reader//vp-inner-html
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

    (defcustom dk-reader//vp-inner-css
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

    (defcustom dk-reader//vp-poly
      (pm-polymode-multi "Vp"
                         :hostmode 'dk-reader//vp-host
                         :innermodes '(dk-reader//vp-inner-html dk-reader//vp-inner-css))
      "Vp typical configuration"
      :group 'polymodes
      :type 'object)

    ;; mix or vue
    (defcustom dk-reader//mix-host
      (pm-bchunkmode "mix" :mode 'fundamental-mode)
      "mix"
      :group 'hostmodes
      :type 'object)

    (defcustom dk-reader//mix-inner-html
      (pm-hbtchunkmode "mix"
                       :head-reg "^<template[^>]*>"
                       :tail-reg "^</template>"
                       :head-mode 'host
                       :tail-mode 'body
                       :mode 'sgml-mode
                       :indent-offset 0
                       :font-lock-narrow t)
      "Mix typical chunk."
      :group 'innermodes
      :type 'object)

    (defcustom dk-reader//mix-inner-css
      (pm-hbtchunkmode "mix"
                       :head-reg "^<style[^>]*>"
                       :tail-reg "^</style>"
                       :head-mode 'host
                       :tail-mode 'body
                       :mode 'css-mode
                       :indent-offset 0
                       :font-lock-narrow t)
      "Mix typical chunk."
      :group 'innermodes
      :type 'object)

    (defcustom dk-reader//mix-inner-js
      (pm-hbtchunkmode "mix"
                       :head-reg "^<script[^>]*>"
                       :tail-reg "^</script>"
                       :head-mode 'host
                       :tail-mode 'body
                       :mode 'js-mode
                       :indent-offset 0
                       :font-lock-narrow t)
      "Mix typical chunk."
      :group 'innermodes
      :type 'object)

    (defcustom dk-reader//mix-poly
      (pm-polymode-multi "mix"
                         :hostmode 'dk-reader//mix-host
                         :innermodes '(dk-reader//mix-inner-html
                                       dk-reader//mix-inner-css
                                       dk-reader//mix-inner-js))
      "Mix typical configuration"
      :group 'polymodes
      :type 'object)

    (define-polymode poly-vp-mode dk-reader//vp-poly)
    (define-polymode poly-mix-mode dk-reader//mix-poly)
    (define-polymode poly-vue-mode dk-reader//mix-poly)))
