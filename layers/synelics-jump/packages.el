(setq synelics-jump-packages
      '(
        (etags :built-in)
        (xref :built-in)
        ))

(defun synelics-jump/init-etags ()
  (use-package etags
    :defer t
    :init
    (progn
      (setq company-etags-ignore-case t
            tags-case-fold-search nil
            tags-add-tables nil))))

(defun synelics-jump/init-xref ()
  (use-package xref
    :defer t
    :init
    (progn
      (evil-define-key 'normal xref--xref-buffer-mode-map (kbd "q") 'quit-window))))
