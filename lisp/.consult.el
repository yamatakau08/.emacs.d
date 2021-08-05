(use-package consult
  :ensure t

  :custom
  (consult-ripgrep-command
   "rg --ignore-case --null --line-buffered --color=ansi --max-columns=1000\
   --no-heading --line-number --hidden . -e ARG OPTS")

  :bind
  (("M-g g" . consult-goto-line)
   ("C-x b" . consult-buffer)
   ("C-s"   . consult-line)
   ("C-r"   . consult-line)
   ("C-x f" . my-consult-file-externally)
   :map dired-mode-map
   ("C-RET" . my-consult-dired-file-exteranally)
   )

  :config
  (recentf-mode) ; enable for consult-recetf-file command, refere https://github.com/minad/consult#virtual-buffers

  (consult-customize
   ;;:preview-key '(:debounce 3 any) ; after 3s
   :preview-key nil)

  (defun consult-ripgrep1 (&optional dir initial)
    (interactive "P")
    ;; pass
    ;; (minibuffer-with-setup-hook
    ;; 	#'beginning-of-line
    ;;   (consult--grep "Ripgrep" consult-ripgrep-command dir "searchword -- --max-depth 1"))

    ;; pass
    ;; (consult--minibuffer-with-setup-hook
    ;; 	#'beginning-of-line
    ;;   (consult--grep "Ripgrep" consult-ripgrep-command dir "searchword -- --max-depth 1"))

    ;; pass
    (consult--minibuffer-with-setup-hook
	(lambda ()
	  (beginning-of-line)
	  (forward-char))
      (consult--grep "Ripgrep" consult-ripgrep-command dir "searchword -- --max-depth 1"))
    )

  (defun my-consult-file-externally ()
    (interactive)
    (let ((url (thing-at-point 'url))
	  (file-name (thing-at-point 'filename t))) ; t: return value without property
      (if url
	  (browse-url-default-browser url)
	(if file-name
	    (consult-file-externally (expand-file-name (thing-at-point 'filename t)))
	  (call-interactively #'find-file)))))

  (defun my-consult-dired-file-exteranally ()
    (interactive)
    (let ((file-name (dired-get-file-for-visit)))
      (cond ((string= (file-name-extension file-name) "mp4")
	     ;; for windows 8.1
	     (shell-command-to-string (format "%s %s" "start" file-name)))
	    (t
	     (consult-file-externally file-name)))))

  )

(provide '.consult)
