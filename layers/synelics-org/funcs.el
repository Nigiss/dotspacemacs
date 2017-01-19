(defun synelics-org//time-to-date (time)
  (let* ((decode (decode-time time)))
    (list (nth 4 decode) (nth 3 decode) (nth 5 decode))))

(defun synelics-org//time-add (time &optional type offset)
  (cond ((eq type 'day) (time-add time (* offset 24 3600)))
        (t (current-time))))

(defun synelics-org//day-of-week (time)
  (let ((day (calendar-day-of-week (synelics-org//time-to-date time))))
    (mod (- day calendar-week-start-day) 7)))

(defun synelics-org//beginning-of-week (time)
  (synelics-org//time-add time 'day (- 0 (synelics-org//day-of-week time))))

(defun synelics-org//end-of-week (time)
  (synelics-org//time-add time 'day (- 6 (synelics-org//day-of-week time))))

(defun synelics-org//format-time (time format-string)
  (let ((date (synelics-org//time-to-date time)))
    (format format-string (caddr date) (car date) (cadr date))))
