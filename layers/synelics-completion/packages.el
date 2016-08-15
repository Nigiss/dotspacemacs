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

(setq synelics-completion-packages
      '(
        ;; ivy
        company
        ycmd
        company-ycmd
        flycheck-ycmd
        company-etags
        company-flx
        yasnippet
        evil
        ))

(defun synelics-completion/post-init-company ()
  (use-package company
    :defer t
    :init
    (progn
      (setq tab-always-indent 'complete)
      (setq company-idle-delay 0.3
            company-minimum-prefix-length 2
            completion-cycle-threshold 5

            ;; etags
            company-etags-everywhere t
            company-etags-ignore-case t)
      (add-hook 'after-init-hook 'global-company-mode))
    :config
    (progn
      ;; (setq company-backends '(company-capf
      ;;                          company-etags
      ;;                          (company-keywords company-dabbrev-code)
      ;;                          company-files
      ;;                          company-dabbrev))
      (let ((map company-active-map))
        (define-key map (kbd "C-w")   'clean-aindent--bsunindent)
        (define-key map (kbd "RET") 'newline-and-indent)
        (define-key map (kbd "TAB") 'company-complete-selection)))))

(defun synelics-completion/post-init-yasnippet()
  (use-package yasnippet
    :defer t
    :config
    (progn
      (defun check-expansion ()
        (save-excursion
          (if (looking-at "\\_>") t
            (backward-char 1)
            (if (looking-at "\\.") t
              (backward-char 1)
              (if (looking-at "->") t nil)))))

      (defun do-yas-expand ()
        (let ((yas/fallback-behavior 'return-nil))
          (yas/expand)))

      (defun tab-indent-or-complete ()
        (interactive)
        (if (minibufferp)
            (minibuffer-complete)
          (if (or (not yas/minor-mode)
                  (null (do-yas-expand)))
              (indent-for-tab-command))))
        (define-key evil-insert-state-map (kbd "TAB") 'tab-indent-or-complete))))

(defun synelics-completion/init-company-flx ()
  (use-package company-flx
    :defer t
    :init
    (progn
      (setq completion-styles '(basic substring initials))
      (with-eval-after-load 'company
        (company-flx-mode +1)
        (add-to-list 'completion-styles 'fuzzy 'append)))))

(defun synelics-completion/init-ycmd ()
  (use-package ycmd
    ;; :defer t
    :init
    (progn
      (setq ycmd-server-command '("python" "/home/vagrant/.spacemacs.d/layers/synelics-completion/local/ycmd/ycmd")
            ycmd-force-semantic-completion t)
      (add-hook 'after-init-hook #'global-ycmd-mode)
      (spacemacs|diminish ycmd-mode " Ⓜ" " M")
      ;; (setq ycmd-force-semantic-completion t)
      ;; (ycmd-global-config nil)
;; (set-variable 'ycmd-global-config "/home/chris/Code/ycmd/cpp/ycm/.ycm_extra_conf.py")
      ;; (set-variable 'ycmd-extra-conf-whitelist '("/home/chris/Code/daily_report_system/v1.0/*"))
      )))

;; (use-package company-ycmd
;;   :ensure t
;;   :init
;;   (defvar venv-location "~/.env")     ; This variable is originally in package virtualenvwrapper

;;   (use-package ycmd
;;     :ensure t
;;     :init
;;     (setq ycmd-server-command '("/usr/local/bin/python3" "/usr/local/ycmd/ycmd")
;;           ycmd-global-config "~/.ycmd_global_conf.py"
;;           ycmd-force-semantic-completion t)

;;     (defun ycmd//use-virtualenv ()
;;       (hack-local-variables)
;;       (when (boundp 'project-venv-name)
;;         (setq ycmd-python-binary-path
;;               (expand-file-name "python" (expand-file-name "bin" (expand-file-name project-venv-name venv-location))))))

;;     (add-hook 'python-mode-hook
;;               #'(lambda ()
;;                   (set (make-local-variable 'company-backends)
;;                        '(company-ycmd))))
;;     (add-hook 'python-mode-hook #'ycmd-mode)
;;     (add-hook 'python-mode-hook #'ycmd//use-virtualenv)

;;     :config
;;     (global-ycmd-mode 1)))


(defun synelics-completion/init-company-ycmd ()
  (use-package company-ycmd
    :config
    (progn
      (company-ycmd-setup))))

;; (defun synelics-completion/init-flycheck-ycmd ()
;;   (use-package company-flx
;;     ;; :defer t
;;     :config
;;     (progn
;;       (flycheck-ycmd-setup))))

;;   (use-package helm-company
;;     :if (configuration-layer/package-usedp 'company)
;;     :defer t
;;     :init
;;     (progn
;;       (defun synelics/ivy-company ()
;;         "Select `company-complete' candidates by `ivy'."
;;         (interactive)

;;         ;; Start complete
;;         (let ((complete-start-pos (point))    ; Since company will auto complete common part
;;               candidate-prefix candidates)

;;           ;; Company first
;;           (unless company-candidates
;;             (company-complete))

;;           ;; Then record states, and quit default frontend
;;           (when company-point
;;             (setq candidate-prefix company-prefix)
;;             (setq candidates company-candidates)
;;             (company-abort))

;;           ;; Show as ido
;;           (when candidates
;;             (let ((pos (point)))
;;               (insert (ivy-read "Compeletion: " candidates nil nil candidate-prefix))
;;               (if (> (length candidate-prefix) 0)
;;                   (delete-region (- complete-start-pos (length candidate-prefix)) pos))))))

;;       (add-to-list 'completion-at-point-functions 'synelics/ivy-company))))
