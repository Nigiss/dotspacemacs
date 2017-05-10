;;; config.el --- OSX Layer keybindings File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(when (spacemacs/system-is-mac)
  ;; this is only applicable to GUI mode
  (when (display-graphic-p)
    ;; Keybindings
    (global-set-key (kbd "s-g") 'goto-line)
    (global-set-key (kbd "s-o") 'wg-switch-to-workgroup)
    (global-set-key (kbd "s-m") 'counsel-recentf)
    (global-set-key (kbd "s-f") 'counsel-find-file)
    (global-set-key (kbd "s-p") 'projectile-find-file)
    (global-set-key (kbd "s-x") 'counsel-M-x)))
