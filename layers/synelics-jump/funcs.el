(defun synelics-jump/find-tag (&optional no-confirm tag-name)
  (interactive)
  (let ((tag-file (concat (projectile-project-root) "TAGS")))

    ;; build tags while no tags table exists
    (unless (file-exists-p tag-file)
      (synelics-jump//build-tags-table))

    (when (file-exists-p tag-file)
      (visit-tags-table tag-file 'local)
      (condition-case nil
          (syenlics-core//find-definitions tag-name no-confirm)

        ;; retry after update tags table
        (user-error (progn
                      (synelics-jump//build-tags-table)
                      (syenlics-core//find-definitions tag-name no-confirm)))))))

(defun syenlics-core//find-definitions (tag-name no-confirm)
  (let ((tag-name (or tag-name (symbol-name (symbol-at-point)))))
    (if no-confirm
        (xref-find-definitions tag-name)
      (call-interactively 'xref-find-definitions tag-name))))

(defun synelics-jump//build-tags-table ()
  "Update tags table with shell script."
  (interactive)
  (synelics-core/shell-command "bash gen-tags.sh" 'background))
