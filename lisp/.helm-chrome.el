(use-package helm-chrome
  :ensure t
  :config
  ;;(setq helm-chrome-source (append helm-chrome-source '(:migemo t))) ; migemo not effect
  ;;(setq helm-chrome-source `(,@helm-chrome-source (migemo . t))) ; migemo not effect

  (setq helm-chrome-source
	(helm-build-in-buffer-source "Chrome::Bookmarks"
	  :init (lambda () (unless helm-chrome--json
			     (helm-chrome-reload-bookmarks)))
	  :data (lambda ()
		  (cl-loop for name being the hash-keys of helm-chrome--bookmarks
			   collect name))
	  :candidate-number-limit 9999
	  :coerce (lambda (candidate) (gethash candidate helm-chrome--bookmarks))
	  :action '(("Browse URL(s)" . (lambda (_candidate)
					 (mapc #'browse-url (helm-marked-candidates))))
		    ("Show URL" . message))
	  :migemo t)))

(provide '.helm-chrome)
