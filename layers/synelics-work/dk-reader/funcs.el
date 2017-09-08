(defun synelics-work/in-directory-p (work-directory)
  (string-match-p (concat "/Works/" work-directory "/") (buffer-file-name)))

;; start server
(defun synelics-work/server-start-preview ()
  (interactive)
  (synelics-core/shell-command "npm run preview"))

(defun synelics-work/server-start-staging ()
  (interactive)
  (synelics-core/shell-command "npm run staging"))

;; phone cmd
(defun synelics-work/phone-dev ()
  (interactive)
  (synelics-core/shell-command (concat (synelics-work//hybrid-command "dev" "phone")
                                     " "
                                     (read-string "dev type: ")
                                     " "
                                     (read-string "dev usage: "))
                             'background))

(defun synelics-work/phone-sync ()
  (interactive)
  (synelics-core/shell-command (synelics-work//hybrid-command "sync")))

(defun synelics-work/phone-alpha ()
  (interactive)
  (synelics-core/shell-command (synelics-work//hybrid-command "alpha")))

(defun synelics-work/phone-publish ()
  (interactive)
  (synelics-core/shell-command (synelics-work//hybrid-command "publish")))

(cl-defun synelics-work//hybrid-command (type &optional (project "phone"))
  (concat "~/Works/dk-reader/frontend/kits/bin/workflow " type))


;;; tramp
(defconst synelics-tool//tramp-token-prompt-regexp ".*\\(token\\)\.*: *")

(defconst synelics-tool//tramp-host-prompt-regexp ".*\\(relay-shell\\).*> *")

(defun synelics-tool//tramp-action-login-host ()
  (let ((host-name (synelics-tool//get-relay-host)))
    (tramp-message vec 3 "Sending host name `%s'" host-name)
    (tramp-send-string vec (concat host-name tramp-local-end-of-line))))

(defun synelics-tool//get-relay-host ()
  (ivy-completing-read "Choose host: " (split-string "c3-dk-yd-log01.bj") nil t))
