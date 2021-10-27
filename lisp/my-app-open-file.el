(defun my-w32-open-file (file-path &optional open-explore-dir)
  "open the file-path by application associated it's file suffix or Windows explore."
  ;; *important* Do not check file-exist-p, because return nil even if actual file exists
  ;;   refer
  ;;   consult-file-externally in consult
  ;;   https://sakashushu.blog.ss-blog.jp/2014-04-29 "体当たり開始"
  (interactive "fFile-path: ")
  (cond ((file-directory-p file-path)
	 (if open-explore-dir
	     ;;(w32-shell-execute "explore" file-path "/e,/select,")
	     ;; the above sometimes make Windows Emacs freeze, use following

	     ;;(async-shell-command (format "%s \"%s\"" "xstart" file-path) nil nil)
	     ;; this pop-up *Async Shell Command* buffer, not good

	     ;; https://stackoverflow.com/a/22982525
	     (call-process-shell-command (format "%s \"%s\"" "xstart" file-path) nil 0)
	   (dired-find-file)))
	((member (file-name-extension file-path) '("MOV" "doc" "docx" "gif" "jpeg" "mp4" "pdf" "ppt" "pptx" "xls" "xlsm" "xlsx"))
	 (if (or (string= (file-name-extension file-path) "MOV")
		 (string= (file-name-extension file-path) "mp4"))
	     ;; Since windows 8.1, (w32-shell-execute "open" file-name) is not available in case of "MOV", "mp4"
	     ;;(w32-shell-execute "open" file-path))

	     ;; use shell-command-to-string,
	     ;; but on windows10, both w32-shell-execute and shell-command-to-string are available
	     ;; so use shell-command-to-string.

	     ;;(shell-command-to-string (format "%s %s" "start" file-path)) ; use the following
	     ;;(shell-command-to-string (string-join `("start" ,file-path) " "))

	     ;; But if there is space in file name, can't open the file with associated program, neither (shell-quote-argument file-name)
	     ;; need to modify "start" script then put ~/.config/fish/functions/xstart
	     (shell-command-to-string (format "%s \"%s\"" "xstart" file-path))

	     ;; Windows 10 is no problem
	   (w32-shell-execute "open" file-path)))
	(t
	 (dired-find-file))
	 ))

;;; the followings are deprecated
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
