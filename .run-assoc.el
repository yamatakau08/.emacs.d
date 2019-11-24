;; for git clone
(add-to-list 'load-path "~/.emacs.d/my-git-source-get")
(require 'my-git-source-get)

;; check if ~/.emacs.d/run-assoc/run-assoc.el"
;; if it does not exist, git clone
(let ((file (expand-file-name "~/.emacs.d/run-assoc/run-assoc.el")))
  (if (not (file-exists-p file))
      (my-git-source-get "https://github.com/emacsmirror/run-assoc.git")))

;; set for instant-maximized-window
(add-to-list 'load-path "~/.emacs.d/run-assoc")

;;
(require 'run-assoc)

(setq associated-program-alist
      '(
	("open" "\\.pdf$") ; open windows/mac
	;;("xdg-open" "\\.pdf$") ; xdg-open linux
	("gnochm" "\\.chm$")
	("mplayer" "\\.mp3$")
	((lambda (file)
	   (let ((newfile (concat (file-name-sans-extension (file-name-nondirectory file)) ".txt")))
	     (cond
	      ((get-buffer newfile)
	       (switch-to-buffer newfile)
	       (message "Buffer with name %s exists, switching to it" newfile))
	      ((file-exists-p newfile)
	       (find-file newfile)
	       (message "File %s exists, opening" newfile))
	      (t (find-file newfile)
		 (= 0 (call-process "antiword" file
				    newfile t "-")))))) "\\.doc$")
	("evince" "\\.ps$")
	("fontforge" "\\.\\(sfd\\(ir\\)?\\|ttf\\|otf\\)$")
	((lambda (file)
	   (browse-url (concat "file:///" (expand-file-name file)))) "\\.html?$")))

;; refer https://www.emacswiki.org/emacs/RunAssoc
(defun helm-find-files-maybe-run-assoc (orig-fun &rest args)
  (let ((sel (helm-get-selection)))
    ;; NB, we only want to do this action if we're looking at the *helm find files* buffer
    (if (and (string= helm-buffer "*helm find files*")
	     (string-match (mapconcat (lambda (x) (second x)) associated-program-alist "\\|")
			   (helm-get-selection)))
	(run-associated-program sel)
      (apply orig-fun args))))

(advice-add 'helm-execute-selection-action :around #'helm-find-files-maybe-run-assoc)
