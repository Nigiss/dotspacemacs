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

(setq work-packages
      '(
        evil
        projectile
        polymode
        ))

(defun work/post-init-projectile ()
  (use-package projectile
    :defer t
    :config
    (progn
      (with-eval-after-load 'evil
        (spacemacs/declare-prefix "mx" "work commands")
        (evil-leader/set-key
          "mxu" 'synelics-core/update-tags-table

          "mxt" 'synelics-work/phone-test
          "mxs" 'synelics-work/phone-sync
          "mxa" 'synelics-work/phone-alpha
          "mxp" 'synelics-work/phone-publish
          "mxh" 'synelics-work/phone-hotfix
          "mxd" 'synelics-work/phone-dev

          "mxr" 'synelics-work/server-start)))))

(defun work/init-polymode ()
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
                         ;; :head-mode 'host
                         ;; :tail-mode 'host
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
                         ;; :head-mode 'host
                         ;; :tail-mode 'host
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
