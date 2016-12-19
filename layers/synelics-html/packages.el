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
        ))

;; (defun synelics-html/init-sgml-mode ()
;;   (use-package sgml-mode
;;     :defer t
;;     :init
;;     (progn
;;       (add-hook 'html-mode-hook 'paredit-mode)
;;       (add-hook 'html-mode-hook 'yas-minor-mode)
;;       (add-hook 'html-mode-hook 'evil-matchit-mode)
;;       (add-hook 'html-mode-hook 'subword-mode)
;;       (add-hook 'html-mode-hook 'emmet-mode))))

(defun synelics-html/post-init-css-mode ()
  (use-package css-mode
    :defer t
    :config
    (progn
      (add-hook 'css-mode-hook 'paredit-mode)
      (add-hook 'css-mode-hook
                (lambda ()
                  (set (make-variable-buffer-local 'company-backends)
                       '(company-css
                         company-capf
                         (company-dabbrev-code company-keywords)
                         company-files company-dabbrev)))
                'append))))
