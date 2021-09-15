(use-package consult

  ;;:ensure t
  ;;:load-path "consult-0.9"

  ;;; straight
  ;; use consult Version 0.9 for consult-grep,ripgrep Ver 0.10 doesn't work well on Mac
  ;; Windows environment, worse
  :straight t ; check straight version lock function ~/.emacs.d/lisp/straight-default.el

  ;;:straight (:branch "0.9") ; pass ,  but have Warning (straight): Could not check out branch "0.9" of repository "consult" Disable showing Disable logging
  ;;:straight (:commit "ee58941308d83a717728f056ea753e80f68cfbc0") ; pass

  ;; fail, don't get all files
  ;;:straight (:branch "async-fix") ; probably equivalent tag 0.9

  ;; staright not support :ref
  ;;:straight (:ref "0.9")

  :custom
  (consult-ripgrep-args ; this variable since Version 0.10 add "--hidden"
   "rg --hidden --line-buffered --color=never --max-columns=1000 --path-separator /\
   --smart-case --no-heading --line-number .")

  :bind
  (("M-g g" . consult-goto-line)
   ("C-x b" . consult-buffer)
   ("C-s"   . consult-line)
   ("C-r"   . consult-line)
   ("C-x f" . my-consult-file-externally)
   ;; M-s bindings (search-map)
   ;;("M-s r" . consult-ripgrep1)
   :map dired-mode-map
   ("C-RET" . my-consult-dired-file-exteranally)
   )

  :config
  (recentf-mode) ; enable for consult-recetf-file command, refere https://github.com/minad/consult#virtual-buffers

  (consult-customize
   ;;:preview-key '(:debounce 3 any) ; after 3s
   :preview-key nil)

  (defun consult--directory-prompt-1 (prompt dir) ; redefine to show directory on Version 0.10 above
    "Format PROMPT, expand directory DIR and return them as a pair."
    (let ((edir (file-name-as-directory (expand-file-name dir)))
          (ddir (file-name-as-directory (expand-file-name default-directory))))
      (cons
       (format "%s (%s): " prompt (consult--abbreviate-directory dir))
       edir)))

  (defun consult--format-directory-prompt (prompt dir) ; redefine to show directory on Version 0.9
    "Format PROMPT, expand directory DIR and return them as a pair."
    (save-match-data
      (let ((edir (file-name-as-directory (expand-file-name dir)))
            (ddir (file-name-as-directory (expand-file-name default-directory))))
	(cons
	 (if (string= ddir edir)
             (concat prompt (format " (%s): " dir))
           (let ((adir (abbreviate-file-name edir)))
             (if (string-match "/\\([^/]+\\)/\\([^/]+\\)/\\'" adir)
		 (format "%s in …/%s/%s/: " prompt
			 (match-string 1 adir) (match-string 2 adir))
               (format "%s in %s: " prompt adir))))
	 edir))))

  (defun my-consult-file-externally ()
    (interactive)
    (let ((url (thing-at-point 'url))
	  (file-name (my-thing-at-point-filename)))
      (if url
	  (browse-url-default-browser url)
	(if file-name
	    (consult-file-externally (expand-file-name file-name))
	  (call-interactively #'find-file)))))

  (defun my-consult-dired-file-exteranally ()
    (interactive)
    (let ((file-name (dired-get-file-for-visit)))
      (cond ((string= (file-name-extension file-name) "mp4")
	     ;; for windows 8.1
	     (shell-command-to-string (format "%s %s" "start" file-name)))
	    (t
	     (consult-file-externally file-name)))))

  ;; :preface Symbol's function definitons is void: consult--grep
  ;; When M-x consult-ripgrep1 just after launching Emacs, consult--grep called from consult-ripgrep1 is internal function.
  ;; So revise to call consult-ripgrep public function
  ;; :demand consult-ripgrep1 is not found when M-x just after launching Emacs
  :preface

  (defun my-consult-ripgrep1 (&optional dir)
    (interactive "P")
    ;; --max-depth 1 works, 0 doesn't work
    ;; see rg manual --max-depth <NUM>
    (consult-ripgrep dir "pattern -- --ignore-case --hidden --max-depth 1"))

  (defun my-consult-dired-grep ()
    "Search for regexp in files marked dired mode, this works on other than Windows"
    ;; https://github.com/minad/consult/issues/407#issuecomment-905342672
    (interactive)
    (let ((consult-grep-args ; Version 0.10 or above works but sometimes show the lines not match the pattern in my log dir files
	   ;; "grep --line-buffered --color=never --ignore-case --exclude-dir=.git --line-number -I -r ." ; original
	   ;; replace "-r ." with "-e ARG OPTS %s" (files)
	   (format "grep --line-buffered --color=never --ignore-case --exclude-dir=.git --line-number -I -e ARG OPTS %s"
		   (mapconcat #'shell-quote-argument (dired-get-marked-files) " "))); this works, even if marked file is only one file. need "-e ARG OPTS"
	  (consult-grep-command ; Version 0.9 works
	   ;; "grep --null --line-buffered --color=always --extended-regexp --exclude-dir=.git --line-number -I -r . -e ARG OPTS" ; original
	   ;; replace "-r ." and "%s" (files)
	   (format "grep --null --line-buffered --color=always --extended-regexp --exclude-dir=.git --line-number -I -e ARG OPTS %s"
		   (mapconcat #'shell-quote-argument (dired-get-marked-files) " ")))
	   )
      (consult-grep nil "pattern -- --ignore-case --hidden")))

  (defun my-consult-dired-ripgrep ()
    "Search for regexp in files marked dired mode, this works on other than Windows"
    ;; https://github.com/minad/consult/issues/421
    (interactive)
    (let ((consult-ripgrep-args ; Version 0.10 or above
	   ;; "rg --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --line-number ." ; original
	   ;; replace '.' with "%s" (files) and add "-e ARG OPTS"
	   (format "rg --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --line-number %s -e ARG OPTS"
		   (mapconcat #'shell-quote-argument (dired-get-marked-files) " ")))
	  (consult-ripgrep-command ; up to Version 0.9
	   ;; "rg --null --line-buffered --color=ansi --max-columns=1000 --no-heading --line-number . -e ARG OPTS" ; original
	   ;; replace '.' with "%s" (files)
	   (format "rg --null --line-buffered --color=ansi --max-columns=1000 --no-heading --line-number %s -e ARG OPTS"
		   (mapconcat #'shell-quote-argument (dired-get-marked-files) " "))
	   ))
      (consult-ripgrep nil "pattern -- --ignore-case --hidden")))

  )

;; sample for using consult under studying
;; not yet open the url in highlight.
(defvar my-consult-sample-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-l") #'my-consult-sample-dummy)
    map)
  "Additional keymap used by `my-consult-sample'.")

(defun my-consult-sample-dummy ()
  "dummy function for checking  when type C-l in my-consult-sample session"
  (interactive)
  (message "my-consult-sample-dummy"))

(defun my-consult-sample ()
  "sample function using consult"
  (interactive)
  (let* ((cands '(("google" . "http://google.com/")
   		  ("yahoo"  . "http://yahoo.com/")))
   	 selected
   	 url)
    (setq selected (consult--read cands
				  :keymap my-consult-sample-map))
    (setq url (cdr (assoc selected cands)))
    (browse-url-default-browser url)))

;; in testing
;; rg <PATTERN> <FILE>...
;; dired-shell-cmd
;; dired-run-shell-command
;;(shell-command-to-string cmd) ; M-: output is in mini buffer, M-x where is it output?
;;(dired-shell-command cmd) ; where is it output?
(defun my-consult-ripgrep-dired-marked-files (pattern)
  "refer https://github.com/minad/consult/wiki#counsel-grep-or-swiper-equivalent"
  (interactive "sPattern: ")
  (let* ((marked-files (dired-get-marked-files))
	 (files (string-join marked-files " "))
	 ;;(cmd (format "rg --line-number --with-filename --crlf --color always %s %s" pattern files)))
	 (cmd (format "rg --line-number --with-filename --crlf %s %s" pattern files)))
    (message "%s" (shell-command-to-string cmd))
    ))

(provide '.consult)
