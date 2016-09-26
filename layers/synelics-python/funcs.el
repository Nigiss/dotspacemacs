(defun synelics//py-goto-definition ()
  "Find definition first, then assignment, if failed, last use TAGS."
  (interactive)
  (anaconda-mode-call "goto_definitions"
                      (lambda (result)
                        (if result
                            (anaconda-mode-definitions-view result)
                          (anaconda-mode-call "goto_assignments"
                                              (lambda (result)
                                                (if result
                                                    (anaconda-mode-definitions-view result)
                                                  (synelics-core/find-tag-no-confirm))))))))

(defun synelics//py-go-back ()
  "Go back."
  (interactive)
  (if anaconda-mode-go-back-definitions
      (anaconda-mode-find-file-no-record-definition (pop anaconda-mode-go-back-definitions))
    (pop-tag-mark)))
