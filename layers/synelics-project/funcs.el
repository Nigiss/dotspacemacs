(defun synelics-wg//get-wg-marker-ring-symbol ()
  (intern (format "wg-%s" (wg-workgroup-name (wg-current-workgroup)))))

(defun synelics-wg//set-marker-ring (sym val)
  (set sym (cons (car val)
                 (cons (cadr val)
                       (copy-sequence (cddr val))))))
