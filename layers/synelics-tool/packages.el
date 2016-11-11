;; ;;; packages.el --- synelics-tool Layer packages File for Spacemacs
;; ;;
;; ;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;; ;;
;; ;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; ;; URL: https://github.com/syl20bnr/spacemacs
;; ;;
;; ;; This file is not part of GNU Emacs.
;; ;;
;; ;;; License: GPLv3

(setq synelics-tool-packages
      '(
        engine-mode
        deft
        ))

(defun synelics-tool/post-init-engine-mode ()
  (use-package engine-mode
    :config
    (progn
      (with-eval-after-load 'evil
        (evil-leader/set-key "an" nil)
        (spacemacs/declare-prefix "an" "search engine")
        (evil-leader/set-key
          "ang" (lambda () (interactive) (call-interactively 'engine/search-google))
          "ani" (lambda () (interactive) (call-interactively 'engine/search-github)))))))
