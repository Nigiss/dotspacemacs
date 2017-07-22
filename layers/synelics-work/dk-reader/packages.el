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
                  (turn-on-evil-matchit-mode)
                  (subword-mode 1)
                  (emmet-mode 1)
                  (paredit-mode 1)

                  ;; https://www.emacswiki.org/emacs/EmacsSyntaxTable
                  (modify-syntax-entry ?: ".")
                  (modify-syntax-entry ?. ".")
                  (modify-syntax-entry ?' ".")
                  (modify-syntax-entry ?- ".")
                  (modify-syntax-entry ?= ".")

                  (setq sgml-basic-offset 4))))))

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

    (synelics-core|add-hook 'js2-mode
                            'ycmd-mode
                            'poly-vp-mode
                            'paredit-mode
                            'subword-mode
                            'evil-matchit-mode)

    (add-hook 'js2-mode-hook
              (lambda ()
                (when (synelics-work/in-directory-p "dk-reader")
                  (setq js-indent-level 4))

                (when (and (file-exists-p (concat (projectile-project-root) ".eslintrc.js")))
                  (flycheck-mode 1)
                  (setq flycheck-enabled-checkers '(javascript-eslint))
                  (setq flycheck-javascript-eslint-executable
                        (concat (projectile-project-root) "node_modules/eslint/bin/eslint.js")))))

    (add-hook 'js2-mode-hook
              (lambda ()
                (define-key evil-normal-state-local-map
                  (kbd "C-]")
                  (synelics-core|center-cursor-after-call 'synelics-work//js-goto-definition))
                (define-key evil-normal-state-local-map
                  (kbd "C-t")
                  (synelics-core|center-cursor-after-call 'pop-tag-mark))))))

(defun dk-reader/init-polymode ()
  (use-package polymode
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
