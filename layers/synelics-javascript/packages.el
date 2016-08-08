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

(setq synelics-javascript-packages
      '(
        js2-mode
        ycmd
        company-ycmd
        ;; ac-js2
        ;; js2-refactor
        ))

(defun synelics-javascript/init-js2-mode ()
  (use-package js2-mode
    :defer t
    :mode "\\.js\\'"
    :init
    (progn
      (setq-default js2-basic-offset 4)
      (add-hook 'js2-mode-hook (lambda () (setq mode-name "JavaScript")))
      (add-hook 'js2-mode-hook 'evil-matchit-mode)
      (add-hook 'js2-mode-hook 'subword-mode)
      (add-hook 'js2-mode-hook 'spacemacs/toggle-spelling-checking-off)
      (add-hook 'js2-mode-hook 'js2-mode-hide-warnings-and-errors)
      (add-hook 'js2-mode-hook
                (lambda ()
                  (define-key evil-normal-state-local-map (kbd "C-]") 'synelics//js-goto-definition))))
    :config
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode
        "zo" 'js2-mode-toggle-element))))

;; (defun synelics-javascript/post-init-company-ycmd ()
;;   (use-package
;;     :init
;;     (progn
;;       (with-eval-after-load 'company
;;         (add-hook 'js2-mode-hook
;;                   #'(lambda ()
;;                       (add-to-list (make-local-variable 'company-backends) company-ycmd)))))))

;; (defun synelics-javascript/post-init-company ()
;;   (spacemacs|add-company-hook js2-mode))

;; (defun synelics-javascript/init-ac-js2 ()
;;   (use-package ac-js2
;;     :defer t
;;     :if (and (configuration-layer/package-usedp 'company)
;;              (configuration-layer/package-usedp 'ac-js2))
;;     :init
;;     (progn
;;       (add-hook 'js2-mode-hook 'ac-js2-mode)
;;       (add-hook 'js2-mode-hook
;;                 (lambda ()
;;                   (add-to-list (make-local-variable 'company-backends) 'ac-js2-company))))
;;     :config
;;     (progn
;;       (defun synelics/js-goto-definition ()
;;         "Use default first, if failed, then use TAGS."
;;         (interactive)
;;         (condition-case nil
;;             (ac-js2-jump-to-definition)
;;           (error
;;            (condition-case nil
;;                (js2-jump-to-definition)
;;              (error
;;               (find-tag))))))

;;       (add-hook 'ac-js2-mode-hook
;;                 (lambda ()
;;                   (define-key evil-visual-state-map (kbd "C-i") 'synelics/js-goto-definition)
;;                   (define-key evil-normal-state-map (kbd "C-i") 'synelics/js-goto-definition)))

;;       (setq completion-at-point-functions
;;             (delete 'ac-js2-completion-function completion-at-point-functions)))))
