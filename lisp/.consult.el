(use-package consult

  ;;:ensure t
  ;;:load-path "consult-0.9"

  :straight t ; installed tag 0.9 with straight version lock function

  ;; pass ,  but have Warning (straight): Could not check out branch "0.9" of repository "consult" Disable showing Disable logging
  ;;:straight (:branch "0.9")
  ;;:straight (:commit "ee58941308d83a717728f056ea753e80f68cfbc0")

  ;; fail, don't get all files
  ;;:straight (:branch "async-fix") ; probably equivalent tag 0.9

  ;; staright not support :ref
  ;;:straight (:ref "0.9")

  :custom
  (consult-ripgrep-args ; add "--hidden"
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

  (defun consult--directory-prompt-1 (prompt dir) ; redefine
    "Format PROMPT, expand directory DIR and return them as a pair."
    (let ((edir (file-name-as-directory (expand-file-name dir)))
          (ddir (file-name-as-directory (expand-file-name default-directory))))
      (cons
       (format "%s (%s): " prompt (consult--abbreviate-directory dir))
       edir)))

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

  (defun my-consult-dired-grep ()
    "Search for regexp in files marked dired mode, this works for only mac"
    ;; https://github.com/minad/consult/issues/407#issuecomment-905342672
    (interactive)
    (let ((consult-grep-args
	   ;;(format "grep --line-buffered --color=never --ignore-case --exclude-dir=.git --line-number -I %s" ; removed "-r ." and add files by from original
	   ;;  (mapconcat #'shell-quote-argument (dired-get-marked-files) " ")) ; if marked file is only one file, doesn't work well.
	   (format   "grep --line-buffered --color=never --ignore-case --exclude-dir=.git --line-number -I -e ARG OPTS %s"
		     (mapconcat #'shell-quote-argument (dired-get-marked-files) " ")) ; this works, even if marked file is only one file.
	   ))
      (consult-grep)))

  ;; :preface Symbol's function definitons is void: consult--grep when M-x consult-ripgrep1 just after launching Emacs, called consult--grep is conslut's internal function from consult-ripgrep1
  ;; revise to call consult-ripgrep
  ;; :demand consult-ripgrep1 is not found when M-x just after launching Emacs
  :preface

  (defun consult-ripgrep1 (&optional dir)
    (interactive "P")
    ;; --max-depth 1 works, 0 doesn't work
    ;; see rg manual --max-depth <NUM>
    (consult-ripgrep dir "searchword -- --max-depth 1"))
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

;; rg <PATTERN> <FILE>...
;; dired-shell-cmd
;; dired-run-shell-command
;;(shell-command-to-string cmd) ; M-: output is in mini buffer, M-x where is it output?
;;(dired-shell-command cmd) ; where is it output?
(defun consult-ripgrep-dired-marked-files (pattern)
  "refer https://github.com/minad/consult/wiki#counsel-grep-or-swiper-equivalent"
  (interactive "sPattern: ")
  (let* ((marked-files (dired-get-marked-files))
	 (files (string-join marked-files " "))
	 ;;(cmd (format "rg --line-number --with-filename --crlf --color always %s %s" pattern files)))
	 (cmd (format "rg --line-number --with-filename --crlf %s %s" pattern files)))
    (message "%s" (shell-command-to-string cmd))
    ))

(defun consult-ripgrep-one-file ()
  "Call `consult-ripgrep' for the current buffer (a single file)."
  (interactive)
  (let ((consult-ripgrep-args
         (concat "rg "
                 "--line-buffered "
                 "--color=never "
                 "--line-number "
                 "--smart-case "
                 "--no-heading "
                 "--max-columns=250 "
                 "--max-columns-preview "
                 "--with-filename "
                 (shell-quote-argument buffer-file-name))))
    (consult-ripgrep)))

(defun consult-ripgrep-work-on-mac ()
  "Call `consult-ripgrep' for the multiple file works on Mac, not on Windows"
  (interactive)
  (let ((consult-ripgrep-args
         (concat "rg "
                 "--line-buffered "
                 "--color=never "
                 "--line-number "
                 "--smart-case "
                 "--no-heading "
                 "--max-columns=250 "
                 "--max-columns-preview "
                 "--with-filename "
                 ;;(shell-quote-argument "/User/yama/tmp/log1")
                 (shell-quote-argument "/tmp/log1")
		 " "
                 ;;(shell-quote-argument "/User/yama/tmp/log2"))))
                 (shell-quote-argument "/tmp/log2"))))
    (consult-ripgrep)))

(defun my-consult-dired-ripgrep ()
    "Search for regexp in files marked dired mode, this works for only mac"
    ;; https://github.com/minad/consult/issues/407#issuecomment-905342672
    (interactive)
    (let ((consult-ripgrep-args
	   ;;(format "rg --line-buffered --color=never --max-columns=1000 --path-separator --smart-case --no-heading --line-number --with-filename "
	   (format "rg --line-buffered --color=never --line-number --smart-case --no-heading --max-columns=250 --max-columns-preview --with-filename "
		   (mapconcat #'shell-quote-argument (dired-get-marked-files) " "))
	   ))
      (consult-ripgrep)))

(provide '.consult)
