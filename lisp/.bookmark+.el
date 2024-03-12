(use-package bookmark+
  ;;https://www.emacswiki.org/emacs/BookmarkPlus#h5o-35
  :straight t

  :bind
  (:map bookmark-bmenu-mode-map
	("C-j" . my-bmkp-jump))

  :config
  (defun my-bmkp-jump ()
    "open the bookmark with Windows explore or application associated the file suffix"
    (interactive)
    (let* ((bookmark (bookmark-bmenu-bookmark))
	   (filename (bookmark-get-filename bookmark)))
      ;;(bmkp-jump-1 bookmark #'my-bmkp-jump-display-func) ; need to consider to use bmkp-jump-1
      (cond ((eq (window-system) 'w32)
	     (cond ((eq (bookmark-get-handler bookmark) #'bmkp-jump-url-browse)
		    (bmkp-jump-url-browse bookmark))
		   (t
		    (my-open-file-with-app filename))))
	    (t (bookmark-jump (bookmark-bmenu-bookmark))))))

  )

(provide '.bookmark+)
