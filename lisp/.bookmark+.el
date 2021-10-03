(use-package bookmark+
  ;;https://www.emacswiki.org/emacs/BookmarkPlus#h5o-35
  :straight t

  :bind
  (:map bookmark-bmenu-mode-map
	("C-j" . my-bmkp-jump))

  :config
  (defun my-bmkp-jump-file-externally (bookmark-entry)
    "this is for handler in bookmark file, refer consult-file-externally"
    (interactive)
    (let ((file-name (let-alist bookmark-entry .filename)))
      ;;(w32-shell-execute "open" (expand-file-name file-name)))) ; no need to expand-file-name
      (w32-shell-execute "open" file-name)))

  (defun my-bmkp-jump ()
    "open the bookmark with Windows explore or application associated the file suffix"
    (interactive)
    (let* ((bookmark (bookmark-bmenu-bookmark))
	   (filename (bookmark-get-filename bookmark)))
      ;;(bmkp-jump-1 bookmark #'my-bmkp-jump-display-func)
      (cond ((eq (window-system) 'w32)
	     (cond ((file-directory-p filename)
		    (w32-shell-execute "explore" filename "/e,/select,"))
		   ((string= (file-name-extension filename) "mp4")
		    ;;(w32-shell-execute "start" filename)) ; for Windows 8.1
		    (shell-command-to-string (format "%s %s" "start" filename)))
		   (t
		    (w32-shell-execute "open" filename))))
	    (t (bookmark-jump (bookmark-bmenu-bookmark))))))

  )

(provide '.bookmark+)
