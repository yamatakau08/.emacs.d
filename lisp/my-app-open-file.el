(defun my-w32-open-file (file-path &optional open-explore-dir)
  "open the file-path by application associated it's file suffix or Windows explore."
  ;; *important* Do not check file-exist-p, because return nil even if actual file exists
  ;;   refer
  ;;   consult-file-externally in consult
  ;;   https://sakashushu.blog.ss-blog.jp/2014-04-29 "体当たり開始"
  (interactive "fFile-path: ")
  (cond ((file-directory-p file-path)
	 (if open-explore-dir
	     ;; (w32-shell-execute "explore" file-path "/e,/select,")
	     ;; this can open folder in which has Japanese character
	     ;; but sometimes makes Windows Emacs freeze, use following

	     (w32-shell-execute nil file-path) ; nil see help
	   ;; monitor if this doesn't make Emacs freeze

	   ;;(async-shell-command (format "%s \"%s\"" "xstart" file-path) nil nil)
	   ;; this pop-up *Async Shell Command* buffer, not good

	   ;; https://stackoverflow.com/a/22982525
	   ;; this can't open directory if directory name has multi byte. e.g. Japanese character
	   ;;(call-process-shell-command (format "%s \"%s\"" "xstart" file-path) nil 0)
	   (dired-find-file)))
	((not (file-name-extension file-path))
	 ;; add this clause, because without suffix such as "tako" ".emacs" have an error on next condition
	 ;; member-ignore-case have Wrong type argument: stringp, nil
	 (dired-find-file))
	((member-ignore-case (file-name-extension file-path) '("MOV" "doc" "docx" "gif" "jpeg" "mp4" "pdf" "ppt" "pptx" "xls" "xlsm" "xlsx" "html" "gdoc" "mp3"))
	     ;; Since windows 8.1, (w32-shell-execute "open" file-name) is not available in case of "MOV", "mp4"

	     ;;(shell-command-to-string (format "%s %s" "start" file-path)) ; use the following
	     ;;(shell-command-to-string (string-join `("start" ,file-path) " "))

	     ;; But if there is space in file name, can't open the file with associated program, neither (shell-quote-argument file-name)
	     ;; need to modify "start" script then put ~/.config/fish/functions/xstart
	     ;;(shell-command-to-string (format "%s \"%s\"" "xstart" file-path))

	     ;; On Windows 10, w32-shell-execute has no problem
	     (w32-shell-execute "open" file-path))
	(t
	 (dired-find-file))
	))

;; to open html file in share folder which is exported by org with browser on windows environment
(defun advice:w32-shell-execute-filter-args (args)
  (setcar (cdr args) (replace-regexp-in-string "/" "\\\\" (cadr args))) ; pass ("open" "path is replaced with '/'")
  args ; return args processed for w32-shell-execute function to execute
)

(advice-add 'w32-shell-execute
	    :filter-args
            'advice:w32-shell-execute-filter-args)

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
