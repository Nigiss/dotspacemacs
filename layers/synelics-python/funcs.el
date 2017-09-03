(defun synelics-python//find-definition-use-xref-marker ()
  (mapcar (lambda (method)
            (let ((callback-func (intern (format "anaconda-mode-find-%s-callback" method))))
              (advice-add callback-func
                          :around (lambda (old-fn result)
                                    (if result
                                        (xref-push-marker-stack))
                                    (funcall old-fn result)
                                    (evil-scroll-line-to-center (line-number-at-pos))))))
          '("definitions" "assignments" "references")))
