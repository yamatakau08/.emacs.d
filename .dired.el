; dired にて、windows に関連付けられたファイルを起動する。
; http://uenox.ld.infoseek.co.jp/elisp/index.html
(defun uenox-dired-winstart ()
  "Type '\\[uenox-dired-winstart]': win-start the current line's file."
  (interactive)
  (if (eq major-mode 'dired-mode)
      (let ((fname (dired-get-filename)))
	(w32-shell-execute "open" fname)
	(message "win-started %s" fname))))

(add-hook 'dired-mode-hook
	  (lambda ()
	    (define-key dired-mode-map "z" 'uenox-dired-winstart)))
