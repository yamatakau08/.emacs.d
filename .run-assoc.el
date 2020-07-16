;; for git clone
(add-to-list 'load-path "~/.emacs.d/my-git-source-get")
(require 'my-git-source-get)

;; check if ~/.emacs.d/run-assoc/run-assoc.el exists
;; if it does not exist, git clone
(let ((file (expand-file-name "~/.emacs.d/run-assoc/run-assoc.el")))
  (if (not (file-exists-p file))
      (my-git-source-get
       ;; "https://github.com/emacsmirror/run-assoc.git" ; original site
       "https://github.com/yamatakau08/run-assoc.git"    ; my fork
       )))

;; set load-path
(add-to-list 'load-path "~/.emacs.d/run-assoc")

;;
(require 'run-assoc)

(cond
 ((eq system-type 'windows-nt)
  (setq associated-program-alist
	'(((lambda (file)
	     ;; note: if html is in file suffix regexp, helm-find-file can't open url which has html,
	     ;; I don't include htlm in file suffix regexp
	     ;;(w32-shell-execute "open" file)) "\\.\\(pdf\\|png\\|JPG\\|msg\\|pptx\\|xls\\|xlsx\\|xlsm\\|docx\\|avi\\|mp4\\)$")))
	     ;; don't include JPG/png image file suffixes, they are appeared in the emacs buffer with normal file open
	     (w32-shell-execute "open" file)) "\\.\\(pdf\\|msg\\|pptx\\|xls\\|xlsx\\|xlsm\\|doc\\|docx\\|avi\\|mp4\\|png\\)$")))
  )
 ((eq system-type 'darwin)
  (setq associated-program-alist
	'(("open" "\\.pdf$")))
      )
 ((eq system-type 'gnu/linux)
  ;; refer https://github.com/emacsmirror/run-assoc.git
  (setq associated-program-alist
	'(("gnochm" "\\.chm$")
	  ("evince" "\\.pdf$")
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
  ))

;; following setting refer https://www.emacswiki.org/emacs/RunAssoc
(defun helm-find-files-maybe-run-assoc (orig-fun &rest args)
  (let ((sel (helm-get-selection)))
    ;; NB, we only want to do this action if we're looking at the *helm find files* buffer
    (if (and
	 ;;(string= helm-buffer "*helm find files*")
	 (or (string= helm-buffer "*helm find files*") (string= helm-buffer "*helm mini*")) ; modified to enable this advice func in *helm mini* buffer
	     (string-match (mapconcat (lambda (x) (second x)) associated-program-alist "\\|")
			   (helm-get-selection nil t) ; add args nil and t to be able to switch *scratch* and other buffers is not file
			   ))
	(run-associated-program sel)
      (apply orig-fun args))))

(advice-add 'helm-execute-selection-action :around #'helm-find-files-maybe-run-assoc)

;; assign key C-x C-f to run-associated-program instead of find-file to open file seamlessly
(global-set-key "\C-x\C-f" 'run-associated-program)

;; replace return is dired-find-file in dired-run-associated-program
(define-key dired-mode-map [return] 'dired-run-associated-program)
