(defun my-w32-open-file (file-path)
  "open the file-path with Windows explore or application associated the file suffix"
  ;; *important* Do not check file-exist-p, because return nil even if actual file exists
  (interactive "fFile-path: ")
  (cond ((file-directory-p file-path)
	 (w32-shell-execute "explore" file-path "/e,/select,"))
	((string= (file-name-extension file-path) "mp4")
	 ;;(w32-shell-execute "open" file-path)) ; not available on  Windows 8.1
	 (shell-command-to-string (string-join `("start" ,file-path) " ")))
	(t
	 (w32-shell-execute "open" file-path) ; open by associated program
	 )))

;;; the followings will be deprecated
(defun my-app-open-file-get-file-name ()
  "to get UNC path from cursor line"
  (interactive)
  (let ((f (thing-at-point 'line)))
    (if (string-match "\\(\\([a-z]:\\|\\\\\\)[^>]+\\) *" f)
	(message "%s" (match-string 1 f))
      (message "not match")
      nil)))

;;; supported by slack emacs-jp
(defun my-app-open-file-path2explore (fpath)
  "convert UNC path to emacs internal path"
  (if fpath
      (progn
	(let ((regexps '(("/" . "\\\\")
			 ("/cygdrive/\\(.\\)" . "\\1:")))
	      (file-w32 fpath))
	  (dolist (find-repl regexps)
	    (setq file-w32 (replace-regexp-in-string
			    (car find-repl) (cdr find-repl)
			    file-w32 nil nil)))
	  ;;(message "%s" file-w32)
	  file-w32
	  ))))

(provide 'my-app-open-file)
