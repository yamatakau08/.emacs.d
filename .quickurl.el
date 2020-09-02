;; Todo
;; M-x quickurl results link_files https://raw.githubusercontent.com/yamatakau08/link_files/master/
(use-package quickurl
  :custom
  (quickurl-urls
   '(("FSF" "https://www.fsf.org/" "The Free Software Foundation")
     ("emacs"  . "http://www.emacs.org/")
     ;; when point is somewhere at "link_" in "link_files", M-x quickurl results link <URL:https://raw.githubusercontent.com/yamatakau08/link_files/master/>_files
     ;; it's not expected
     ("link_files" . "https://raw.githubusercontent.com/yamatakau08/link_files/master/")
     ("hagbard" "http://www.hagbard.demon.co.uk" "Hagbard's World")))

  :config
  ;; redefine
  (defun quickurl-format-url (url)
    "replace with only URL, default is \"<URL:%s>\""
    (format "%s" (quickurl-url-url url)))

  (defun my-quickurl (&optional lookup)
    "Insert a URL based on LOOKUP.
If not supplied LOOKUP is taken to be the word at point in the current
buffer, this default action can be modified via
`quickurl-grab-lookup-function'."
    (interactive)
    (when (or lookup
              (setq lookup (funcall quickurl-grab-lookup-function)))
      (quickurl-load-urls)
      (let ((url (quickurl-find-url lookup)))
	(if (null url)
            (error "No URL associated with \"%s\"" lookup)
	  (let ((pos (string-match (car url) (thing-at-point 'line))))
	    (if pos
		(progn
		  (move-to-column pos)
		  (quickurl-insert url))))))))

  (defun my-move-point-to-begining-of-string-in-line (string)
    (interactive "sSearch String: ")
    (let ((pos (string-match string (thing-at-point 'line))))
      (if pos
	  (move-to-column pos))))
  )
  
  
