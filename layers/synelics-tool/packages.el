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
        chinese-pyim
        chinese-pyim-basedict
        chinese-pyim-greatdict
        deft
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
    (setq default-input-method "chinese-pyim")

    (with-eval-after-load 'chinese-pyim
      (synelics-core|add-hook 'prog-mode (lambda () (set-input-method default-input-method))))

    ;; 使用全拼
    (setq pyim-default-scheme 'quanpin)

    ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
    ;; 我自己使用的中英文动态切换规则是：
    ;; 1. 光标只有在注释里面时，才可以输入中文。
    ;; 2. 光标前是汉字字符时，才能输入中文。
    ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
    (setq-default pyim-english-input-switch-functions
                  '(pyim-probe-dynamic-english
                    pyim-probe-isearch-mode
                    pyim-probe-program-mode
                    pyim-probe-org-structure-template))

    (setq-default pyim-punctuation-half-width-functions
                  '(pyim-probe-punctuation-line-beginning
                    pyim-probe-punctuation-after-punctuation))

    ;; 开启拼音搜索功能
    (setq pyim-isearch-enable-pinyin-search t)

    ;; 使用 pupup-el 来绘制选词框
    (setq pyim-page-tooltip 'popup)
    (setq pyim-page-style 'one-line)

    ;; 选词框显示5个候选词
    (setq pyim-page-length 5)

    ;; 让 Emacs 启动时自动加载 pyim 词库
    (add-hook 'emacs-startup-hook #'(lambda () (pyim-restart-1 t)))

    :bind
    (("C-;" . pyim-convert-code-at-point) ;与 pyim-probe-dynamic-english 配合
     ;; ("C-;" . pyim-delete-word-from-personal-buffer)
     )))

(defun synelics-tool/init-chinese-pyim-basedict ()
  (use-package chinese-pyim-basedict
    :config (chinese-pyim-basedict-enable)))

(defun synelics-tool/init-chinese-pyim-greatdict ()
  (use-package chinese-pyim-greatdict
   :config (chinese-pyim-greatdict-enable)))
