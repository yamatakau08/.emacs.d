;; check if ~/.emacs.d/run-assoc/run-assoc.el exists
;; if it does not exist, git clone
(let ((file (expand-file-name "~/.emacs.d/run-assoc/run-assoc.el")))
  (if (not (file-exists-p file))
      (my-git-source-get
       ;; "https://github.com/emacsmirror/run-assoc.git" ; original site
       "https://github.com/yamatakau08/run-assoc.git"    ; my fork
       )))

(use-package run-assoc
  :load-path "run-assoc"

  :bind (("C-x f" . my-run-associated-program)
	 :map dired-mode-map
	 ("RET" . dired-run-associated-program))

  :config
  (custom-set-variables
   '(associated-program-alist
     (cond
      ((eq system-type 'windows-nt)
       '(((lambda (file) ; for mp4 file on only windows8.1
	    ;;(shell-command-to-string (format "%s %s" "start" (replace-regexp-in-string "/" "\\\\\\\\" file))))
	    ;; the above is the test code
	    ;; no need to execute the function "replace-regexp-in-string" in run-assoc
	    ;; Since it doesn't work w32-shell-execute open with windows media player, use shell-command-to-string
	    (shell-command-to-string (format "%s %s" "start" file)))
	  "\\.\\(mp4\\|wmv\\)$")
	 ((lambda (file)
	    ;; note: don't include ".htlm" in the file suffix regexp
	    ;; if ".html" is in file suffix regexp, helm-find-file can't open the that url.
	    (w32-shell-execute "open" file))
	  "\\.\\(pdf\\|msg\\|pptx\\|xls\\|xlsx\\|xlsm\\|doc\\|docx\\|avi\\|jpg\\|JPG\\|png\\|PNG\\|wmv\\|mp4\\)$")))
      ((eq system-type 'darwin)
       '(("open" "\\.\\(pdf\\|mp4\\)$")))
      ((eq system-type 'gnu/linux)
       ;; refer https://github.com/emacsmirror/run-assoc.git
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
	    (browse-url (concat "file:///" (expand-file-name file)))) "\\.html?$"))))))

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

  (defun my-run-associated-program ()
    "original function which browse url or open file with associated program in buffer, otherwise query the file in mini buffer"
    (interactive)
    (let ((url (thing-at-point 'url))
	  (file-name-arg (thing-at-point 'filename t))) ; t: return value without property
      (if url
	  (browse-url-default-browser url)
	(if file-name-arg
	    (run-associated-program file-name-arg)
	  (call-interactively #'run-associated-program)))))
  )

(provide '.run-assoc)
