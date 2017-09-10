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

(defconst dk-reader//tramp-mirelay-host-list "
10.57.23.203                c3-dk-yd-webadmin00.bj      c4-miui-l7-read00.bj        help                        lg-dk-yd-web00.bj
10.57.23.220                c3-dk-yd-webadmin01.bj      c4-miui-l7-read01.bj        lg-dk-com-fe00.bj           lg-dk-yd-web01.bj
c3-dk-yd-cache00.bj         c3-dk-yd-webadmin02.bj      c4-miui-l7-read02.bj        lg-dk-com-fe01.bj           lg-dk-yd-web02.bj
c3-dk-yd-cache01.bj         c3-dkyd-set1-mgdb00.bj      c4-miui-read-cache00.bj     lg-dk-com-fe02.bj           lg-dk-yd-web03.bj
c3-dk-yd-fe02.bj            c3-dkyd-set1-mgdb01.bj      c4-miui-read-fe00.bj        lg-dk-com-fe03.bj           lg-dk-yd-web04.bj
c3-dk-yd-fe03.bj            c3-dkyd-set1-mgdb02.bj      c4-miui-read-fe01.bj        lg-dk-yd-activities00.bj    lg-dkyd-rep4-mgdb00.bj
c3-dk-yd-fe04.bj            c3-dkyd-set2-mgdb00.bj      c4-miui-read-log00.bj       lg-dk-yd-activities01.bj    lg-miui-l7-read00.bj
c3-dk-yd-fictionwap00.bj    c3-dkyd-set2-mgdb01.bj      c4-miui-read-log01.bj       lg-dk-yd-activities02.bj    lg-miui-l7-read01.bj
c3-dk-yd-fictionwap01.bj    c3-dkyd-set2-mgdb02.bj      c4-miui-read-mga00.bj       lg-dk-yd-book00.bj          lg-miui-l7-read02.bj
c3-dk-yd-fictionwap02.bj    c3-dkyd-set3-mgdb00.bj      c4-miui-read-mga01.bj       lg-dk-yd-fictionwap00.bj    lg-miui-read-fe00.bj
c3-dk-yd-fictionwap03.bj    c3-dkyd-set3-mgdb01.bj      c4-miui-read-mgb00.bj       lg-dk-yd-fictionwap01.bj    lg-miui-read-fe01.bj
c3-dk-yd-log00.bj           c3-dkyd-set3-mgdb02.bj      c4-miui-read-mgb01.bj       lg-dk-yd-fictionwap02.bj    lg-miui-read-store00.bj
c3-dk-yd-log01.bj           c3-miui-l7-read00.bj        c4-miui-read-mgc00.bj       lg-dk-yd-fictionwap03.bj    or1-dkyd-sc00.awsus
c3-dk-yd-misc00.bj          c3-miui-l7-read01.bj        c4-miui-read-mgc01.bj       lg-dk-yd-fictionwap04.bj    sgpaws-dk-yd-nginx00.mias
c3-dk-yd-misc01.bj          c3-miui-l7-read02.bj        c4-miui-read-se00.bj        lg-dk-yd-fictionwap05.bj    sgpaws-dk-yd-nginx01.mias
c3-dk-yd-se00.bj            c3-miui-read-fe00.bj        c4-miui-read-se01.bj        lg-dk-yd-log00.bj           shell
c3-dk-yd-se01.bj            c3-miui-read-fe01.bj        c4-miui-read-store00.bj     lg-dk-yd-log01.bj           tj1-miui-read-stage00.kscn
c3-dk-yd-store00.bj         c3-miui-read-preview00.bj   c4-miui-read-store01.bj     lg-dk-yd-mail00.bj          tj1-miui-read-stage01.kscn
c3-dk-yd-store01.bj         c3-miui-read-preview01.bj   c4-miui-read-store02.bj     lg-dk-yd-store01.bj         tj1-miui-read-stage02.kscn
c3-dk-yd-store02.bj         c3-miui-read-web00.bj       c4-miui-read-web00.bj       lg-dk-yd-store02.bj         tj1-miui-read-stage03.kscn
c3-dk-yd-web00.bj           c3-miui-read-web01.bj       c4-miui-read-web01.bj       lg-dk-yd-store03.bj         tj1-miui-read-stage04.kscn
c3-dk-yd-web01.bj           c3-miui-read-web02.bj       c4-miui-read-web02.bj       lg-dk-yd-store05.bj
c3-dk-yd-web02.bj           c3-miui-read-web03.bj       exit                        lg-dk-yd-store06.bj
")

(defvar dk-reader//tramp-token nil)
(defun dk-reader//tramp-compute-token-adv (&rest args)
  (setq dk-reader//tramp-token (substring (shell-command-to-string "python ~/Learns/totp/decode-token.py") 0 -1)))

;; (defadvice tramp-check-for-regexp (around dk-reader//tramp-compute-token-adv disable)
;;   (if (string-equal (substring "asdfasdf\\'" -1) "'"))
;;   )

(defun dk-reader//tramp-action-password (proc vec)
  (with-current-buffer (process-buffer proc)
    (let ((enable-recursive-minibuffers t)
          (case-fold-search t))
      (unless (tramp-get-connection-property vec "first-password-request" nil)
        (tramp-clear-passwd vec))
      (tramp-message vec 3 "Sending password")
      (process-send-string proc (concat "Lyr04110228" tramp-local-end-of-line))
      (narrow-to-region (point-max) (point-max)))))

(defun dk-reader//tramp-action-token (proc vec)
  (with-current-buffer (process-buffer proc)
    (let ((enable-recursive-minibuffers t)
          (case-fold-search t))
      (tramp-message vec 3 "Sending token")
      (process-send-string proc (concat dk-reader//tramp-token tramp-local-end-of-line))
      (narrow-to-region (point-max) (point-max)))))

(defun dk-reader//tramp-action-login-host (_proc _vec)
  (let ((host-name (dk-reader//get-relay-host)))
    (tramp-message vec 3 "Sending host name `%s'" host-name)
    (tramp-send-string vec (concat host-name tramp-local-end-of-line))))

(defun dk-reader//get-relay-host ()
  (ivy-completing-read "Choose host: " (split-string dk-reader//tramp-mirelay-host-list) nil t))
