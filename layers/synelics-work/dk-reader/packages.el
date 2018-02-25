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
        polymode
        company
        anaconda-mode
        (tramp :built-in)
        (js-mode :location built-in)
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
    (synelics-work||define-server-run-func "staging" "preview" "preview-local")
    (synelics-work||define-workflow-func ("sync" nil nil)
                                         ("alpha" nil nil)
                                         ("alpha-specific" (list (ivy-read "route name: " '("index"
                                                                                        "book-detail"
                                                                                        "secondary"
                                                                                        "user-center"
                                                                                        "activity"
                                                                                        "browser"
                                                                                        "v3"))) nil)
                                         ("publish" nil nil)
                                         ("publish-upgrade" nil nil)
                                         ("dev" (list (read-string "dev type: ")
                                                      (read-string "dev usage: ")) t))

    (spacemacs/declare-prefix "mm" "Custom commands")
    (spacemacs/declare-prefix "mmw" "Workflow")
    (spacemacs/declare-prefix "mmd" "dev")

    (global-set-key (kbd "C-H-s") 'synelics-work//build-raw)
    (evil-leader/set-key
      "mmwd" 'synelics-work/workflow-with-dev
      "mmws" 'synelics-work/workflow-with-sync
      "mmwa" 'synelics-work/workflow-with-alpha
      "mmwA" 'synelics-work/workflow-with-alpha-specific
      "mmwp" 'synelics-work/workflow-with-publish
      "mmwP" 'synelics-work/workflow-with-publish-upgrade

      "mmdb" 'synelics-work//build-debug
      "mmdB" '(lambda (route-name)
                (interactive (read-string "route name: "))
                (synelics-work//build-debug route-name))

      "mmds" 'synelics-work/dev-with-staging
      "mmdp" 'synelics-work/dev-with-preview
      "mmdP" 'synelics-work/dev-with-preview-local)))

(defun dk-reader/init-js-mode ()
  (use-package js-mode
    :defer t
    :init
    (setq js-indent-level 2)

    (spacemacs|add-company-backends :backends company-tern :modes js-mode)
    (spacemacs//set-tern-key-bindings 'js-mode)
    (add-hook 'js-mode-hook 'tern-mode)

    (synelics-core|add-hook 'js-mode
                            (lambda ()
                              (evil-matchit-mode +1)
                              (advice-add 'tern-show-definition
                                          :around (lambda (old-fn data)
                                                    (if data
                                                        (progn
                                                          (xref-push-marker-stack)
                                                          (funcall old-fn data))
                                                      (user-error (message "No definition found.")))))))

    (add-hook 'js-mode-hook
              (lambda ()
                (let ((eslint-exec (and (projectile-project-p)
                                        (concat (projectile-project-root) "node_modules/eslint/bin/eslint.js"))))
                  (when (file-exists-p eslint-exec)
                    (setq-local flycheck-enabled-checkers '(javascript-eslint))
                    (setq-local flycheck-javascript-eslint-executable eslint-exec)
                    (flycheck-mode 1)))))))

(defun dk-reader/init-polymode ()
  (use-package polymode
    :init
    (add-to-list 'auto-mode-alist '("\\.mix$" . poly-mix-mode))
    (add-to-list 'auto-mode-alist '("\\.vue$" . poly-mix-mode))
    :config
    ;; vp
    (defcustom dk-reader//vp-host
      (pm-bchunkmode "JS mode"
                     :mode 'js-mode)
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
      (pm-bchunkmode "mix" :mode 'sgml-mode)
      "mix"
      :group 'hostmodes
      :type 'object)

    (defcustom dk-reader//mix-inner-html
      (pm-hbtchunkmode "mix"
                       :head-reg "^<template[^>]*>"
                       :tail-reg "^</template>"
                       :head-mode 'host
                       :tail-mode 'host
                       :mode 'sgml-mode
                       :indent-offset 2
                       :font-lock-narrow t)
      "Mix typical chunk."
      :group 'innermodes
      :type 'object)

    (defcustom dk-reader//mix-inner-css
      (pm-hbtchunkmode "mix"
                       :head-reg "^<style[^>]*>"
                       :tail-reg "^</style>"
                       :head-mode 'host
                       :tail-mode 'host
                       :mode 'css-mode
                       :indent-offset 2
                       :font-lock-narrow t)
      "Mix typical chunk."
      :group 'innermodes
      :type 'object)

    (defcustom dk-reader//mix-inner-js
      (pm-hbtchunkmode "mix"
                       :head-reg "^<script[^>]*>"
                       :tail-reg "^</script>"
                       :head-mode 'host
                       :tail-mode 'host
                       :mode 'js-mode
                       :indent-offset 2
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

;; (defun dk-reader/init-tramp ()
;;   (use-package tramp
;;     :defer t
;;     :init
;;     (setq tramp-default-method "ssh")
;;     (with-eval-after-load 'tramp-sh
;;       ;; (advice-add 'tramp-maybe-open-connection :before 'dk-reader//tramp-compute-token-adv)
;;       (add-to-list 'tramp-actions-before-shell '(dk-reader//tramp-token-prompt-regexp dk-reader//tramp-action-token))
;;       (add-to-list 'tramp-actions-before-shell '(dk-reader//tramp-vpn-prompt-regexp dk-reader//tramp-action-login-host)))))
