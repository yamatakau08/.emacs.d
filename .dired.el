; dired にて、windows に関連付けられたファイルを起動する。
; http://uenox.ld.infoseek.co.jp/elisp/index.html (site disappear)
(defun uenox-dired-winstart ()
  "Type '\\[uenox-dired-winstart]': win-start the current line's file."
  (interactive)
  (if (eq major-mode 'dired-mode)
      (let ((fname (dired-get-filename)))
	(cond
	 ((eq system-type 'windows-nt)
	  (w32-shell-execute "open" fname))
	 ((eq system-type 'darwin)
	  (shell-command-to-string (format "open %s" fname))))
	(message "open %s" fname))))

(add-hook 'dired-mode-hook
	  (lambda ()
	    (define-key dired-mode-map "z" 'uenox-dired-winstart)))
