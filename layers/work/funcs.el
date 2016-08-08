;; web or store-hybrid
(defun synelics-work/server-start ()
  (interactive)
  (synelics-work||exec-shell "bash run.sh"))

;; phone test
(defun synelics-work/phone-test ()
  "Test for phone module"
  (interactive)
  (split-window-below-and-focus)
  (ansi-term "/usr/bin/zsh")
  (term-send-raw-string "~/kits/bin/test-phone-v3\n")
  (evil-window-up 1))

;; phone cmd
(defun synelics-work/phone-dev ()
  "Sync to origin/master"
  (interactive)
  (synelics-work||exec-shell (concat (synelics-work//hybrid-command "dev" "phone")
                                     " "
                                     (read-string "dev type: ")
                                     " "
                                     (read-string "dev usage: "))
                             'sync))

(defun synelics-work/phone-sync ()
  "Sync to origin/master"
  (interactive)
  (synelics-work||exec-shell (synelics-work//hybrid-command "sync")))

(defun synelics-work/phone-hotfix ()
  "Sync to origin/master"
  (interactive)
  (synelics-work||exec-shell (synelics-work//hybrid-command "hotfix") 'sync))

(defun synelics-work/phone-alpha ()
  "Sync to origin/master"
  (interactive)
  (synelics-work||exec-shell (synelics-work//hybrid-command "alpha")))

(defun synelics-work/phone-publish ()
  "Sync to origin/master"
  (interactive)
  (synelics-work||exec-shell (synelics-work//hybrid-command "publish")))


;;; helper
(cl-defun synelics-work//hybrid-command (type &optional (project "phone"))
  (concat "bash cmd " type " " project))

(defmacro synelics-work||exec-shell (cmd &optional sync)
  `(projectile-with-default-dir (projectile-project-root)
     (if ,sync
         (shell-command ,cmd)
       (split-window-below-and-focus)
       (shell)
       (comint-send-string "*shell*" (concat ,cmd "\n")))))
