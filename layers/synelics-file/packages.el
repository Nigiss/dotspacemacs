(setq synelics-file-packages
      '(
        counsel
        ))

(defun synelics-file/post-init-counsel ()
  (use-package counsel
    :defer t
    :init
    (setq confirm-nonexistent-file-or-buffer nil)

    (advice-add 'find-file :around 'synelics-file//find-file-advice)))

(defun synelics-file//find-file-advice (old-fn filename &rest args)
  (let ((file-size (or (and (file-exists-p filename)
                            (length (with-temp-buffer
                                      (insert-file-contents-literally filename)
                                      (buffer-string))))
                       0)))
    (if (> file-size 10000)
        (find-file-literally filename)
      (funcall old-fn filename args))))
