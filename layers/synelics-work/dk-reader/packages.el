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
        with-editor
        js2-mode
        polymode
        company
        anaconda-mode
        (tramp :built-in)
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
                                 (string-equal (file-name-extension (buffer-file-name)) "tpl")
                                 (shell-command (concat "~/Works/dk-reader/frontend/kits/bin/tpl " buffer-file-name " > /dev/null"))))))))

(defun dk-reader/post-init-with-editor ()
  (use-package with-editor
    :defer t
    :init
    (synelics-work||define-server-run-func "start" "staging" "staging:local" "preview" "preview:local")
    (synelics-work||define-workflow-func ("sync" nil nil)
                                         ("alpha" nil nil)
                                         ("publish" nil nil)
                                         ("dev" (list (read-string "dev type: ")
                                                      (read-string "dev usage: ")) t))

    (spacemacs/declare-prefix "mm" "Custom commands")
    (spacemacs/declare-prefix "mmw" "Workflow")
    (spacemacs/declare-prefix "mmr" "run server")
    (evil-leader/set-key
      "mmwd" 'synelics-work/workflow-with-dev
      "mmws" 'synelics-work/workflow-with-sync
      "mmwa" 'synelics-work/workflow-with-alpha
      "mmwp" 'synelics-work/workflow-with-publish

      "mmrs" 'synelics-work/run-server-with-start
      "mmri" 'synelics-work/run-server-with-staging
      "mmrI" 'synelics-work/run-server-with-staging:local
      "mmrp" 'synelics-work/run-server-with-preview
      "mmrP" 'synelics-work/run-server-with-preview:local)))

(defun dk-reader/post-init-js2-mode ()
  (use-package js2-mode
    :defer t
    :init

    (add-hook 'js2-mode-hook
              (lambda ()
                (when (and (projectile-project-p)
                           (file-exists-p (concat (projectile-project-root) ".eslintrc.js")))
                  (setq-local js2-basic-offset 4)
                  (if (string-match "^[ \t]*{{#[^}]+}}" (buffer-string))
                      (poly-vp-mode 1)))))))

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
                       :mode 'js2-mode
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

(defun dk-reader/post-init-company ()
  (use-package company
    :defer t
    :init
    (advice-add 'company-etags--candidates
                :around (lambda (old-fn prefix)
                          (mapcar (lambda (candidate)
                                    (replace-regexp-in-string "{{# *\\([a-zA-Z0-9_]+\\)" "\\1" candidate))
                                  (funcall old-fn prefix))))))

(defun dk-reader/post-init-anaconda-mode ()
  (use-package anaconda-mode
    :defer t
    :init
    (add-hook 'wg-after-switch-to-workgroup-hook
              (lambda ()
                (let* ((rep-dir (projectile-project-root))
                       (python-path (concat (or rep-dir "") "src/")))
                  (if (and (file-exists-p python-path)
                          (file-exists-p (concat python-path "start.py")))
                      (setq python-shell-extra-pythonpaths (list python-path))
                    (setq python-shell-extra-pythonpaths nil)))))))

(defun dk-reader/init-tramp ()
  (use-package tramp
    :defer t
    :init
    (setq tramp-default-method "ssh")
    (with-eval-after-load 'tramp-sh
      (advice-add 'tramp-maybe-open-connection :before 'dk-reader//tramp-compute-token-adv)
      (add-to-list 'tramp-actions-before-shell '(tramp-password-prompt-regexp dk-reader//tramp-action-password))
      ;; (add-to-list 'tramp-actions-before-shell '(dk-reader//tramp-token-prompt-regexp tramp-action-password))
      (add-to-list 'tramp-actions-before-shell '(dk-reader//tramp-token-prompt-regexp dk-reader//tramp-action-token))
      (add-to-list 'tramp-actions-before-shell '(dk-reader//tramp-host-prompt-regexp dk-reader//tramp-action-login-host)))))
