(defun synelics-jump/find-tag-no-confirm (&optional tag-name)
  (interactive)
  (synelics-jump/find-tag 'no-confirm tag-name))

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
                      (pop-tag-mark)
                      (synelics-jump//build-tags-table)
                      (syenlics-core//find-definitions (car find-tag-history) t)))))))

(defun syenlics-core//find-definitions (tag-name no-confirm)
  (let ((tag-name (or tag-name (symbol-name (symbol-at-point)))))
    (if no-confirm
        (find-tag tag-name)
      (call-interactively 'find-tag tag-name))))

(defun synelics-jump//build-tags-table ()
  "Update tags table with shell script."
  (evil-write-all nil)
  (projectile-with-default-dir (projectile-project-root)
    (shell-command "bash gen-tags.sh")))
