;; ;;; packages.el --- synelics-tool Layer packages File for Spacemacs
;; ;;
;; ;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;; ;;
;; ;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; ;; URL: https://github.com/syl20bnr/spacemacs
;; ;;
;; ;; This file is not part of GNU Emacs.
;; ;;
;; ;;; License: GPLv3

(setq synelics-tool-packages
      '(
        engine-mode
        pretty-mode
        chinese-pyim
        ))

(defun synelics-tool/post-init-engine-mode ()
  (use-package engine-mode
    :config
    (progn
      (with-eval-after-load 'evil
        (evil-leader/set-key "an" nil)
        (spacemacs/declare-prefix "an" "search engine")
        (evil-leader/set-key
          "ang" (lambda () (interactive) (call-interactively 'engine/search-google))
          "ani" (lambda () (interactive) (call-interactively 'engine/search-github)))))))

(defun synelics-tool/init-pretty-mode ()
  (use-package pretty-mode :defer t))

(defun synelics-tool/post-init-chinese-pyim ()
  (use-package chinese-pyim
    :defer t
    :init
    (define-key pyim-mode-map (kbd ".") 'pyim-page-next-page)
    (define-key pyim-mode-map (kbd ",") 'pyim-page-previous-page)
    (define-key pyim-mode-map (kbd "C-n") 'pyim-page-next-word)
    (define-key pyim-mode-map (kbd "C-p") 'pyim-page-previous-page)
    (define-key pyim-mode-map (kbd "C-w") 'pyim-quit-clear)

    :config
    (setq pyim-default-scheme 'quanpin)

    ;; 开启拼音搜索功能
    (setq pyim-isearch-enable-pinyin-search t)

    ;; 使用 pupup-el 来绘制选词框
    (setq pyim-page-tooltip 'popup)
    (setq pyim-page-style 'one-line)

    ;; 选词框显示5个候选词
    (setq pyim-page-length 5)

    (setq pyim-dicts (let ((dir pyim-dicts-directory))
                       (mapcar (lambda (x) `(:name ,(file-name-base x) :file ,(concat dir x)))
                               (remove-if-not (lambda (x) (equal (file-name-extension x) "pyim"))
                                              (and (file-exists-p dir) (directory-files dir))))))

    :bind
    (("C-;" . synelics-tool//pyim-convert-code-at-point)
     ("C-'" . synelics-tool//toggle-input-method))))
