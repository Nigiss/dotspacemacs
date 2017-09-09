;; start server
(defmacro synelics-work||define-server-run-func (&rest cmd-list)
  `(progn ,@(mapcar (lambda (cmd)
                      `(defun ,(intern (format "synelics-work/run-server-with-%s" cmd)) ()
                         (interactive)
                         (synelics-core/shell-command (format "npm run %s" ,cmd))))
                    cmd-list)))

;; workflow
(defmacro synelics-work||define-workflow-func (&rest workflow-list)
  `(progn ,@(mapcar (lambda (workflow)
                      (let ((type (nth 0 workflow))
                            (background-p (nth 2 workflow)))
                        `(defun ,(intern (format "synelics-work/workflow-with-%s" type)) ()
                           (interactive)
                           (synelics-core/shell-command (synelics-work//hybrid-command ,type
                                                                                       (mapconcat 'identity
                                                                                                  ;; args may read interactive
                                                                                                  ;; can't descruct out of defun
                                                                                                   ,(nth 1 workflow)
                                                                                                   " "))
                                                        ,background-p))))
                    workflow-list)))

(defun synelics-work//hybrid-command (type args)
  (concat "~/Works/dk-reader/frontend/kits/bin/workflow " (concat type " " args)))

;;; tramp
(defconst dk-reader//tramp-token-prompt-regexp ".*\\(token\\)\.*: *")

(defconst dk-reader//tramp-host-prompt-regexp ".*\\(relay-shell\\).*> *")

(defun dk-reader//tramp-action-login-host ()
  (let ((host-name (dk-reader//get-relay-host)))
    (tramp-message vec 3 "Sending host name `%s'" host-name)
    (tramp-send-string vec (concat host-name tramp-local-end-of-line))))

(defun dk-reader//get-relay-host ()
  (ivy-completing-read "Choose host: " (split-string "c3-dk-yd-log01.bj") nil t))
