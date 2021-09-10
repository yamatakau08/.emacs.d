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

;; "grep --line-buffered --color=never --ignore-case --exclude-dir=.git --line-number -I -r ."
;; "grep --line-buffered --color=never --ignore-case --exclude-dir=.git --line-number -I "c:/Temp/SC3/Log/BISYAMON3G-2575/halog_20210820_103646_+0900_fw2.28.0_SC/log/logc20"
;; (defun consult--grep-sample-builder (input)
;;   "Build command line given INPUT."
;;   (pcase-let* ((cmd (split-string-and-unquote consult-grep-args))
;;                (type (consult--grep-regexp-type (car cmd)))
;;                (`(,arg . ,opts) (consult--command-split input))
;;                (`(,re . ,hl) (funcall consult--regexp-compiler arg type)))
;;     (when re
;;       (list :command
;;             (append cmd
;;                     (list (if (eq type 'pcre) "--perl-regexp" "--extended-regexp")
;;                           "-e" (consult--join-regexps re type))
;;                     opts)
;;             :highlight hl))))

(defun consult-grep-test ()
  (interactive)
  (let*
      ((consult-grep-args "grep --line-buffered --color=never --ignore-case --exclude-dir=.git --line-number -I ") ; do not search recursively
       (file "c:/Temp/Log/log1")
       (dir (file-name-directory file))
       (file-name (file-name-nondirectory file))
       (input (format "dummy -- %s" file-name)))
    (consult-grep dir input)))

;; https://github.com/minad/consult/issues/407#issuecomment-905342672
;; works only Mac, not Windows
(defun consult-grep-one-file0 ()
  "Call `consult-grep' for the current buffer (a single file)."
  (interactive)
  (let ((consult-grep-args
         (concat "grep "
                 "--line-buffered "
                 "--color=never "
                 "--line-number "
                 "-I "
                 "-e ARG OPTS "
                 (shell-quote-argument buffer-file-name))))
    (message "[consult-grep-one-file] consult-grep-args: %s" consult-grep-args)
    (consult-grep)))

;; https://github.com/minad/consult/issues/407#issuecomment-906427582
;; Not work on Windows
(defun consult-grep-one-file ()
  "Call `consult-grep' for the current buffer (a single file)."
  (interactive)
  (let ((consult-grep-args
         (concat "grep "
                 "--line-buffered "
                 "--color=never "
                 "--line-number "
                 "--with-filename "
                 (shell-quote-argument buffer-file-name))))
    (message "[consult-grep-one-file] consult-grep-args: %s" consult-grep-args)
    (consult-grep)))

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

(defun my-consult-grep-logc-on-mac ()
  "in testing on Mac works"
  (interactive)
  (let* ((file "/Users/yama/bin/ptools/mylogcat/SC3/Log/BISYAMON3G-2575/var/logc1") ; 1
	 ;;(file "~/bin/ptools/mylogcat/SC3/Log/BISYAMON3G-2575/var/logc1") ; 2
	 ;;(consult-grep-args (format "grep --line-buffered --color=never --ignore-case --exclude-dir=.git --line-number -I %s" file)) ; 1,2 fail, need "-e ARG OPTS" as option

	 (consult-grep-args (format "grep --line-buffered --color=never --ignore-case --exclude-dir=.git --line-number -I -e ARG OPTS %s" (expand-file-name file))) ;; 1 pass
	 )
    (consult-grep)))

;; (defun consult--grep (prompt builder dir initial)
;;   "Run grep in DIR.

;; BUILDER is the command builder.
;; PROMPT is the prompt string.
;; INITIAL is inital input."
;;   (let* ((prompt-dir (consult--directory-prompt prompt dir))
;;          (default-directory (cdr prompt-dir))
;;          (read-process-output-max (max read-process-output-max (* 1024 1024))))
;;     (consult--read
;;      (consult--async-command builder
;;        (consult--grep-format builder)
;;        :file-handler t) ;; allow tramp
;;      :prompt (car prompt-dir)
;;      :lookup #'consult--lookup-member
;;      :state (consult--grep-state)
;;      :initial (consult--async-split-initial initial)
;;      :add-history
;;      (when-let (thing (thing-at-point 'symbol))
;;        (consult--async-split-initial thing))
;;      :require-match t
;;      :category 'consult-grep
;;      :group #'consult--grep-group
;;      :history '(:input consult--grep-history)
;;      :sort nil)))

(defun consult-grep-specified-files ()
  (interactive)
  (let ((consult-grep-args
	 (concat "grep --line-buffered --color=never --ignore-case --exclude-dir=.git --line-number -I " ; removed "-r ." by from original
		 ;;(mapconcat #'shell-quote-argument (dired-get-marked-files) " "))))
		 (mapconcat #'shell-quote-argument '("/tmp/Log/log1" "/tmp/Log/log2" "/tmp/Log/log3") " "))))
    (consult-grep nil "-- --invert-match")))

(defun consult-ripgrep-invert-match (&optional dir initial)
  (interactive "P")
  (let ((consult-ripgrep-args "rg --hidden --invert-match --line-buffered --color=never --max-columns=1000 --path-separator /\
   --smart-case --no-heading --line-number ."))
    (consult--grep "Ripgrep" #'consult--ripgrep-builder dir initial)))

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
                 (shell-quote-argument "/tmp/log1")
		 " "
                 (shell-quote-argument "/tmp/log2"))))
    (consult-ripgrep)))

;; in testing
(defun my-consult--ripgrep-builder (input)
  "Build command line given INPUT."
  (pcase-let* ((cmd (split-string-and-unquote consult-ripgrep-args))
               (type (consult--ripgrep-regexp-type (car cmd)))
               (`(,arg . ,opts) (consult--command-split input))
               (`(,re . ,hl) (funcall consult--regexp-compiler arg type)))
    (when re
      (let ((ret (list :command
		       (append cmd
			       (and (eq type 'pcre) '("-P"))
			       (list  "-e" (consult--join-regexps re type))
			       (shell-quote-argument "c:/Temp/SC3/Log/BISYAMON3G-2575/halog_20210820_103646_+0900_fw2.28.0_SC/log/logc1")
			       opts
			       )
		       :highlight hl)))
	(message "%s" ret)
	ret))))

(defun consult-ripgrep-tako (&optional dir initial)
  (interactive "P")
  (consult--grep "Ripgrep" #'my-consult--ripgrep-builder dir initial))

(provide '.consult)
