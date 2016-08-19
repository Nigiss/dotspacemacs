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
        company
        company-ycmd
        evil
        evil-matchit
        flycheck
        ;; ac-js2
        ;; js2-refactor
        ))

(defun synelics-python/init-anaconda-mode ()
  (use-package anaconda-mode
    :defer t
    :init
    (progn
      (setq anaconda-mode-installation-directory
            (concat spacemacs-cache-directory "anaconda-mode"))
      (add-hook 'python-mode-hook 'paredit-mode)
      (add-hook 'python-mode-hook 'anaconda-mode)
      (add-hook 'python-mode-hook 'evil-matchit-mode)
      (add-hook 'python-mode-hook
                (lambda ()
                  (define-key evil-normal-state-local-map (kbd "C-t") 'synelics//py-go-back)
                  (define-key evil-normal-state-local-map (kbd "C-]") 'synelics//py-goto-definition))))
    :config
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'python-mode
        "hh" 'anaconda-mode-show-doc
        "gg" 'anaconda-mode-find-definitions
        "ga" 'anaconda-mode-find-assignments
        "gb" 'anaconda-mode-go-back
        "gu" 'anaconda-mode-find-references)

      (evilified-state-evilify anaconda-mode-view-mode anaconda-mode-view-mode-map
        (kbd "q") 'quit-window)


      (spacemacs|hide-lighter anaconda-mode))))

(defun python/post-init-company ()
  (spacemacs|add-company-hook python-mode))

(defun synelics-python/post-init-company-ycmd ()
  (use-package company-ycmd
    :defer t
    :init
    (progn
      (push 'company-ycmd company-backends-python-mode))))

;; (defun python/post-init-evil ()
;;   (add-hook 'python-mode-hook #'(lambda ()
;;                                     (local-set-key (kbd "C-]") 'synelics//goto-definition)
;;                                     (local-set-key (kbd "C-t") 'synelics//go-back))))

(defun synelics-python/post-init-evil-matchit ()
  (add-hook 'python-mode-hook 'evil-matchit-mode))

(defun synelics-python/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'python-mode))
