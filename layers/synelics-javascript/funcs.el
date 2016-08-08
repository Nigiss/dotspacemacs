(defun synelics//js-goto-definition ()
  "Use default first, if failed, then use TAGS."
  (interactive)
  (condition-case nil
      (js2-jump-to-definition)
    (error
     (synelics-core/find-tag))))
