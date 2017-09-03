(defun synelics-javascript//find-definition-use-xref-marker ()
  (advice-add 'tern-show-definition
              :around (lambda (old-fn data)
                       (if data
                           (progn
                             (xref-push-marker-stack)
                             (funcall old-fn data))
                         (condition-case nil
                             (synelics-jump/find-tag 'no-confirm)
                           (user-error (message "No definition found.")))))))
