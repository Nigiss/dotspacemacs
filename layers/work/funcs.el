;; web or store-hybrid
(defun synelics-work/server-start ()
  (interactive)
  (synelics-work||exec-command-with-shell "bash run.sh"))

;; phone test
(defun synelics-work/phone-test ()
  "Test for phone module"
  (interactive)
  (synelics-work//exec-command-with-term "~/kits/bin/test-phone-v3")
  (evil-window-up 1))

;; phone cmd
(defun synelics-work/phone-dev ()
  "Sync to origin/master"
  (interactive)
  (synelics-work||exec-command-with-shell (concat (synelics-work//hybrid-command "dev" "phone")
                                     " "
                                     (read-string "dev type: ")
                                     " "
                                     (read-string "dev usage: "))
                             'sync))

(defun synelics-work/phone-sync ()
  "Sync to origin/master"
  (interactive)
  (synelics-work||exec-command-with-shell (synelics-work//hybrid-command "sync")))

(defun synelics-work/phone-hotfix ()
  "Sync to origin/master"
  (interactive)
  (synelics-work||exec-command-with-shell (synelics-work//hybrid-command "hotfix") 'sync))

(defun synelics-work/phone-alpha ()
  "Sync to origin/master"
  (interactive)
  (synelics-work||exec-command-with-shell (synelics-work//hybrid-command "alpha")))

(defun synelics-work/phone-publish ()
  "Sync to origin/master"
  (interactive)
  (synelics-work||exec-command-with-shell (synelics-work//hybrid-command "publish")))


;;; helper
(cl-defun synelics-work//hybrid-command (type &optional (project "phone"))
  (concat "bash cmd " type " " project))

(defmacro synelics-work||exec-command-with-shell (cmd &optional sync)
  `(projectile-with-default-dir (projectile-project-root)
     (if ,sync
         (shell-command ,cmd)
       (delete-other-windows)
       (split-window-below-and-focus)
       (shell)
       (comint-send-string "*shell*" (concat ,cmd "\n")))))


(defun synelics-work//exec-command-with-term (cmd)
  (delete-other-windows)
  (split-window-below-and-focus)
  (let ((term-buffer (multi-term)))
    (term-send-string term-buffer (concat cmd "\n"))))
