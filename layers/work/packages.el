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
        ))

(defun work/post-init-projectile ()
  (use-package projectile
    :defer t
    :config
    (progn
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

            "mxr" 'synelics-work/server-start))))))
