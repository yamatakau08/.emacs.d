; dired にて、windows に関連付けられたファイルを起動する。
; http://uenox.ld.infoseek.co.jp/elisp/index.html (site disappear)
(defun uenox-dired-winstart ()
  "Type '\\[uenox-dired-winstart]': win-start the current line's file."
  (interactive)
  (if (eq major-mode 'dired-mode)
      (let ((fname (dired-get-filename)))
	(cond
	 ((eq system-type 'windows-nt)
	  (message "%s" fname)
	  (message "%s" (convert-standard-filename fname))
;	  (w32-shell-execute "open" fname))
	  (my-app-open-file (convert-standard-filename fname)))
	 ((eq system-type 'darwin)
;	  (shell-command-to-string (format "open %s" fname))))
	  (start-process "open-with-default-app" nil "open" fname))
	(message "open %s" fname)))))

(add-hook 'dired-mode-hook
	  (lambda ()
	    (define-key dired-mode-map "z" 'uenox-dired-winstart)))
