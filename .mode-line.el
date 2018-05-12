;;; for model-line
(setq display-time-mail-icon t)
(setq display-time-string-forms
  '((substring year -2)
    "/"  (if (= (length month) 1) (concat "0" month) month)
    "/" day
    " " 24-hours ":" minutes ; ":" seconds
    (if time-zone " (") time-zone (if time-zone ")") ; 有効無効に関わらず windows環境で 実時間 11:30 に対して表示時間 03:30 になる
    (if mail " Mail" "")))

(display-time) ; 時間表示を行なう

;;; for line,column number
(setq line-number-mode t)
(setq column-number-mode t)
