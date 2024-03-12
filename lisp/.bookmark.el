(use-package bookmark
  :bind
  (:map bookmark-bmenu-mode-map
	("C-j" . my-bookmark-jump))

  :custom
  (bookmark-default-file
   (if (eq (window-system) 'w32)
       "c:/Users/0000910700/OneDrive - Sony/lisp/bookmarks"
     (locate-user-emacs-file "bookmarks" ".emacs.bmk")))

  :config
  (require 'bookmark+)

  (defun my-bookmark-jump ()
    "open directory with Windows explore"
    (interactive)
    (let* ((id (tabulated-list-get-id))
	   (filename (let-alist id .filename)))
      (if (eq (window-system) 'w32)
	  (if (file-directory-p filename)
	      (my-open-file-with-app filename)
	    (bookmark-jump (bookmark-bmenu-bookmark)))
	(bookmark-jump (bookmark-bmenu-bookmark)))))

  (defun bookmark-jump-display-func (bookmark)
    (let* (;;(entry (bookmark-get-bookmark (format "%s" bookmark))) ; pass
	   ;;(entry (bookmark-get-bookmark (with-current-buffer bookmark))) ; fail
	   (entry (bookmark-get-bookmark (buffer-name bookmark))) ; pass
	   (filename (let-alist entry .filename)))
      (if (eq (window-system) 'w32)
	  (if (file-directory-p filename)
	      (my-open-file-with-app filename)
	    (bookmark-jump (bookmark-bmenu-bookmark)))
	(bookmark-jump (bookmark-bmenu-bookmark)))))

  (defun advice:bookmark-jump-filter-args (args)
    (list (car args) #'bookmark-jump-display-func))

  ;;once comment this adevice, because this make always use explore to open directory.
  ;;need to use separately manually.
  ;;(advice-add 'bookmark-jump :filter-args 'advice:bookmark-jump-filter-args)
  )

(provide '.bookmark)
