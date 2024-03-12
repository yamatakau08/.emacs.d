(use-package files
  :custom
  (make-backup-files nil) ; no backup files
  (auto-save-default nil)

  :bind
  (("C-x f" . my-find-file))

  :config
  (defun my-find-file ()
    (interactive)
    (let ((url (thing-at-point 'url))
	  (file (if (derived-mode-p 'dired-mode)
		    (dired-get-file-for-visit)
		  (my-thing-at-point-filename))))
      (cond (url
	     (browse-url-default-browser url))
	    (file
	     (my-open-file-with-app file))
	    (t
	     (call-interactively #'find-file)))))
  )

(provide '.files)

