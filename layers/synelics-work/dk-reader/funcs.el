(defun synelics-work/in-directory-p (work-directory)
  (string-match-p (concat "/Works/" work-directory "/") (buffer-file-name)))

;; start server
(defun synelics-work/server-start-preview ()
  (interactive)
  (synelics-work||exec-command-with-shell "bash run.sh"))

(defun synelics-work/server-start-staging ()
  (interactive)
  (synelics-work||exec-command-with-shell "bash in.run.sh"))

;; phone test
(defun synelics-work/phone-test ()
  "Test for phone module"
  (interactive)
  (synelics-work//exec-command-with-term "~/kits/bin/test-phone-v3"))

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
  (concat "~/Works/dk-reader/frontend/kits/bin/workflow " type))

(defvar synelics-work||shell-index 0)  ; There are multiple work shells
(defmacro synelics-work||exec-command-with-shell (cmd &optional sync)
  `(projectile-with-default-dir (projectile-project-root)
     (if ,sync
         (shell-command ,cmd)
       (let ((shell-buffer (format "*work<%s>*" (incf synelics-work||shell-index))))
         (shell shell-buffer)
         (comint-send-string shell-buffer (concat ,cmd "\n"))))))

(defun synelics-work//exec-command-with-term (cmd)
  (let ((term-buffer (multi-term)))
    (term-send-string term-buffer (concat cmd "\n"))))

(defun synelics-work//js-goto-definition ()
  "Use default first, if failed, then use TAGS."
  (interactive)
  (add-hook 'ycmd-after-exception-hook
            (lambda (&rest args)
              (synelics-core/find-tag-no-confirm))
            nil
            'local)
  (ycmd-goto))
