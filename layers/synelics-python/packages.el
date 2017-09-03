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

(setq synelics-python-packages
      '(
        anaconda-mode
        ))

(defun synelics-python/post-init-anaconda-mode ()
  (use-package anaconda-mode
    :defer t
    :init
    (add-to-list 'spacemacs-jump-handlers-python-mode
                 '(anaconda-mode-find-assignments :async t))
    (add-hook 'python-mode-hook 'synelics-python//find-definition-use-xref-marker)))
