;; the following message appears when execute C-x C-f or dired to open directory on Mac
;; ls does not support --dired; see ‘dired-use-ls-dired’ for more details.

;; dired-listing-switches
;; original value "-al"
;; (setq dired-listing-switches "-al")
;; switch to "-Alhv --group-directories-first"
;; have an error on Mac
;; insert-directory: Listing directory failed but ‘access-file’ worked
;(setq dired-listing-switches "-Alhv --group-directories-first")
;; on Windows
;  c:/yama/.emacs.d:
;  total used in directory 160 available 134740560
;    51509920738105335     4k drwxrwxrwx  1 0000910700 Domain Users     4k 12-23 17:10 .git
;     1125899907103619     2k -rw-rw-rw-  1 0000910700 Domain Users   1.6k 12-23 17:10 window-setting.el

;;; dired にて、windows に関連付けられたファイルを起動する。
;;; http://uenox.ld.infoseek.co.jp/elisp/index.html (site disappear)
(defun uenox-dired-winstart ()
  "Type '\\[uenox-dired-winstart]': win-start the current line's file."
  (interactive)
  (if (eq major-mode 'dired-mode)
      (let ((fname (dired-get-filename)))
	(cond
	 ((eq system-type 'windows-nt)
	  ;;(message "%s %s" fname (convert-standard-filename fname)) ; for debug
	  ;;(w32-shell-execute "open" fname))
	  ;;(my-app-open-file (convert-standard-filename fname)))
	  (my-app-open-file fname))
	 ((eq system-type 'darwin)
	  ;;(shell-command-to-string (format "open %s" fname))))
	  (start-process "open-with-default-app" nil "open" fname))
	 ))))

(add-hook 'dired-mode-hook
	  (lambda ()
	    (define-key dired-mode-map "z" 'uenox-dired-winstart)))
