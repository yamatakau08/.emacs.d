;;; https://gist.github.com/takaxp/e2929aed1c33fdff2e01a708fbd5fde8
;;;
;;; this function is made by takaxp for my request
;;; to generate Daily report in descending, org-clock-report
;;; to enable descending set the following
;;; (setq org-clocktable-steps-reverse-p t)

(setq org-clocktable-steps-reverse-p t)

(with-eval-after-load "org-clock"
  (defvar org-clocktable-steps-reverse-p nil)
  (defun ad:org-clocktable-steps (params)
    "Create one or more clock tables, according to PARAMS.
Step through the range specifications in plist PARAMS to make
a number of clock tables."
    (let* ((ignore-empty-tables (plist-get params :stepskip0))
           (step (plist-get params :step))
           (step-header
            (pcase step
              (`day "Daily report: ")
              (`week "Weekly report starting on: ")
              (`month "Monthly report starting on: ")
              (`year "Annual report starting on: ")
              (_ (user-error "Unknown `:step' specification: %S" step))))
           (week-start (or (plist-get params :wstart) 1))
           (month-start (or (plist-get params :mstart) 1))
           (range
            (pcase (plist-get params :block)
              (`nil nil)
              (range
               (org-clock-special-range range nil t week-start month-start))))
           ;; For both START and END, any number is an absolute day
           ;; number from Agenda.  Otherwise, consider value to be an Org
           ;; timestamp string.  The `:block' property has precedence
           ;; over `:tstart' and `:tend'.
           (start
            (pcase (if range (car range) (plist-get params :tstart))
              ((and (pred numberp) n)
               (pcase-let ((`(,m ,d ,y) (calendar-gregorian-from-absolute n)))
                 (apply #'encode-time (list 0 0 org-extend-today-until d m y))))
              (timestamp
               (seconds-to-time
                (org-matcher-time (or timestamp
                                      ;; The year Org was born.
                                      "<2003-01-01 Thu 00:00>"))))))
           (end
            (pcase (if range (nth 1 range) (plist-get params :tend))
              ((and (pred numberp) n)
               (pcase-let ((`(,m ,d ,y) (calendar-gregorian-from-absolute n)))
                 (apply #'encode-time (list 0 0 org-extend-today-until d m y))))
              (timestamp (seconds-to-time (org-matcher-time timestamp)))))
           (p (when org-clocktable-steps-reverse-p (point))))

      (while (time-less-p start end)
        (unless (bolp) (insert "\n"))
        (when p
          (goto-char (1- p))
          (insert "\n"))
        ;; Insert header before each clock table.
        (insert "\n"
                step-header
                (format-time-string (org-time-stamp-format nil t) start)
                "\n")
        ;; Compute NEXT, which is the end of the current clock table,
        ;; according to step.
        (let* ((next
                (apply #'encode-time
                       (pcase-let
                           ((`(,_ ,_ ,_ ,d ,m ,y ,dow . ,_) (decode-time start)))
                         (pcase step
                           (`day (list 0 0 org-extend-today-until (1+ d) m y))
                           (`week
                            (let ((offset (if (= dow week-start) 7
                                            (mod (- week-start dow) 7))))
                              (list 0 0 org-extend-today-until (+ d offset) m y)))
                           (`month (list 0 0 0 month-start (1+ m) y))
                           (`year (list 0 0 org-extend-today-until 1 1 (1+ y)))))))
               (table-begin (if p p (line-beginning-position 0)))
               (step-time
                ;; Write clock table between START and NEXT.
                (org-dblock-write:clocktable
                 (org-combine-plists
                  params (list :header ""
                               :step nil
                               :block nil
                               :tstart (format-time-string
                                        (org-time-stamp-format t t)
                                        start)
                               :tend (format-time-string
                                      (org-time-stamp-format t t)
                                      ;; Never include clocks past END.
                                      (if (time-less-p end next) end next))))))
               (table-end 0))
          (when p
            (unless (ignore-errors (let ((case-fold-search t)) (re-search-forward step-header)))
              (let ((case-fold-search t)) (re-search-forward "^[ \t]*#\\+END:")))
            (end-of-line 0)
            (setq table-end (point)))
          (let ((case-fold-search t)) (re-search-forward "^[ \t]*#\\+END:"))
          ;; Remove the table if it is empty and `:stepskip0' is
          ;; non-nil.
          (when (and ignore-empty-tables (equal step-time 0))
            (delete-region (if p table-end (line-beginning-position)) table-begin))
          (setq start next))
        (end-of-line 0))))
  (advice-add 'org-clocktable-steps :override #'ad:org-clocktable-steps))
