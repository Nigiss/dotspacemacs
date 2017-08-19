(defun synelics-tool//toggle-input-method ()
  (interactive)
  (unless default-input-method
    (setq default-input-method "chinese-pyim"))
  (toggle-input-method))

(defun synelics-tool//pyim-convert-code-at-point ()
  (interactive)
  (unless (equal input-method-function 'pyim-input-method)
    (synelics-tool//toggle-input-method))
  (let* ((case-fold-search nil)
         (pyim-force-input-chinese t)
         (pyim-english-input-switch-functions nil)
         (string (if mark-active
                     (buffer-substring-no-properties
                      (region-beginning) (region-end))
                   (buffer-substring (point) (line-beginning-position))))
         code length)
    (if (pyim-string-match-p "[[:punct:]]" (pyim-char-before-to-string 0))
        ;; 当光标前的一个字符是标点符号时，半角/全角切换。
        (call-interactively 'pyim-punctuation-translate-at-point)
      ;; 不匹配“'”
      (and (string-match "[a-z-]+ *$" string)
          (setq code (match-string 0 string))
          (setq length (length code))
          (setq code (replace-regexp-in-string " +" "" code)))
      (when (and length (> length 0))
        (delete-char (- 0 length))
        (insert (mapconcat #'char-to-string
                           (pyim-input-method code) "")))))
  (synelics-tool//toggle-input-method))
