(setq synelics-core-packages
      '(
        flycheck
        ))

(defun synelics-core/init-flycheck ()
  (use-package flycheck
    :defer t
    :init
    (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))))
