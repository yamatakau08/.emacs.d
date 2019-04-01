;;; to open UNC represent file with associated application

;;;
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
	  (message "%s" file-w32)))))

;;; supported by slack emacs-jp
(defun my-app-open-file-w32-shell-execute (file-w32)
  "actual execution part to open file with associated application"
  (if (file-directory-p file-w32)
      (w32-shell-execute "explore" file-w32 "/e,/select,")  ; when dir, open it by Explorer
    ;; (w32-shell-execute "open" "explorer" (concat "/e,/select," file-w32)) ; when file, select file in Explorer
    ;; *important* Do not check file-exist-p, because return nil even if actual file exists on UNC path environment.
    ;; open by default App
    (w32-shell-execute "open" "explorer" file-w32)))

;;;
(defun my-app-open-file (&optional file-w32)
  "function open the file with associated application arg file-w32 is for testing"
  (interactive)
  (if file-w32
      (my-app-open-file-w32-shell-execute (my-app-open-file-path2explore file-w32))
    (my-app-open-file-w32-shell-execute
     (my-app-open-file-path2explore (my-app-open-file-get-file-name)))))
