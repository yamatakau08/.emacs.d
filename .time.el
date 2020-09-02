(use-package time
  :custom
  (display-time-string-forms
   '((substring year -2)
     "/"  (if (= (length month) 1) (concat "0" month) month)
     "/"  day ;
     " " 24-hours ":" minutes ":" seconds
     (if time-zone " (") time-zone (if time-zone ")")
     ;; 有効無効に関わらず windows環境で 実時間 11:30 に対して表示時間 03:30になる
     ;; → TZ設定がないのが原因
     (if mail " Mail" "")))
  :config
  (display-time) ; 時間表示を行なう
  )
