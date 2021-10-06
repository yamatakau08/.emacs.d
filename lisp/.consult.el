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
   ("C-c b" . consult-bookmark)
   ;; M-s bindings (search-map)
   ;;("M-s r" . consult-ripgrep1)
   :map dired-mode-map
   ("C-<return>" . my-consult-dired-file-exteranally)
   )

  :config
  (recentf-mode) ; enable for consult-recetf-file command, refere https://github.com/minad/consult#virtual-buffers

  (consult-customize
   consult-buffer ; Since consult-buffer on Windows file access is a bit slow, disable preview
   ;;:preview-key '(:debounce 3 any) ; after 3s
   :preview-key (kbd "M-.") ; this doesn't effect?
   )

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

  ;;
  ;; my-consult-bookmark
  ;;
  (defvar my-consult-bookmark--map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-j") #'my-consult-bookmark--jump)
      (define-key map (kbd "C-l") #'my-consult-bookmark--jump)
      map)
    "Additional keymap used by `my-consult-bookmark'.")

  (defun my-consult-bookmark--jump ()
    (interactive)
    (let* ((cand (consult-vertico--candidate))
	   (bmkp-record (get-text-property 0 'bmkp-full-record cand))
	   (location (let-alist (cdr bmkp-record) .location))
	   (filename (let-alist (cdr bmkp-record) .filename)))
      (cond (location
	     (browse-url-default-browser location))
	    (filename
	     (my-w32-open-file filename)))))

  (defun my-consult-bookmark (name)
    "If bookmark NAME exists, open it, otherwise create a new bookmark with NAME.
The command supports preview of file bookmarks and narrowing. See the
variable `consult-bookmark-narrow' for the narrowing configuration."
    (require 'consult)
    (interactive
     (list
      (let ((narrow (mapcar (pcase-lambda (`(,x ,y ,_)) (cons x y))
                            consult-bookmark-narrow)))
	(consult--read
	 (consult--bookmark-candidates)
	 :prompt "Bookmark: "
	 :preview-key nil ;; added
	 :state (consult--bookmark-preview)
	 :category 'bookmark
	 :history 'bookmark-history
	 ;; Add default names to future history.
	 ;; Ignore errors such that `consult-bookmark' can be used in
	 ;; buffers which are not backed by a file.
	 :add-history (ignore-errors (bookmark-prop-get (bookmark-make-record) 'defaults))
	 :group (consult--type-group narrow)
	 :narrow (consult--type-narrow narrow)
	 :keymap my-consult-bookmark--map))))
    (bookmark-maybe-load-default-file)
    ;; original
    ;; (if (assoc name bookmark-alist)
    ;;     (bookmark-jump name)
    ;;   (bookmark-set name))
    (if (assoc name bookmark-alist)
	(let* ((bookmark (bmkp-get-bookmark name 'NOERROR))
	       (filename (bookmark-get-filename bookmark)))
	  (cond ((eq (window-system) 'w32)
		 (cond ((eq (bookmark-get-handler bookmark) #'bmkp-jump-url-browse)
			(bmkp-jump-url-browse bookmark))
		       (t
			(my-w32-open-file filename))))
		(t (bookmark-jump (bookmark-bmenu-bookmark)))))))

  ;; redefine consult-bookmark
  (advice-add 'consult-bookmark :override #'my-consult-bookmark)

  ;; my-consult-buffer
  (defvar my-consult-buffer--map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-l") #'my-consult-buffer--jump)
      map)
    "Additional keymap used by `my-consult-buffer'.")

  (defun my-consult-buffer--jump ()
    (interactive)
    (let* ((cand (consult-vertico--candidate))
	   (entry (get-text-property 0 'consult-multi cand))
	   (kind  (car entry)))
      (cond ((string= kind "file")
	     (let ((file (cdr entry)))
	       (my-w32-open-file file)))
	    ((string= kind "bookmark")
	     (my-consult-bookmark--jump)))))

  (defun my-consult-buffer ()
    "Enhanced `switch-to-buffer' command with support for virtual buffers.

The command supports recent files, bookmarks, views and project files as virtual
buffers. Buffers are previewed. Furthermore narrowing to buffers (b), files (f),
bookmarks (m) and project files (p) is supported via the corresponding keys. In
order to determine the project-specific files and buffers, the
`consult-project-root-function' is used. See `consult-buffer-sources' and
`consult--multi' for the configuration of the virtual buffer sources."
    (interactive)
    (when-let (buffer (consult--multi consult-buffer-sources
                                      :require-match
                                      (confirm-nonexistent-file-or-buffer)
                                      :prompt "Switch to: "
				      :keymap my-consult-buffer--map ; add own keymap
				      :preview-key nil ; add
                                      :history 'consult--buffer-history
                                      :sort nil))
      ;; When the buffer does not belong to a source,
      ;; create a new buffer with the name.
      (unless (cdr buffer)
	(consult--buffer-action (car buffer)))))

  ;; redefine consult-buffer
  (advice-add 'consult-buffer :override #'my-consult-buffer)

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
		   (mapconcat #'shell-quote-argument (dired-get-marked-files) " ")))
	  )
      (consult-ripgrep nil "pattern -- --ignore-case --hidden")))

  ;; consult-find
  ;; https://github.com/minad/consult/issues/317#issuecomment-849635051
  ;; apply consult-find-args, but that doesn't work!
  ;; (when (eq (window-system) 'w32)
  ;;   (setq consult-find-args
  ;;         (replace-regexp-in-string "\\*" "\\\\*" consult-find-args)))
  )

;; Sample code using consult for studying
(defvar my-consult-sample-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-l") #'my-consult-sample--open-browser)
    map)
  "Additional keymap used by `my-consult-sample'.")

(defun my-consult-sample--open-browser ()
  "Dummy function for checking  when type C-l in my-consult-sample session"
  (interactive)
  (let* ((candidate (consult-vertico--candidate))
	 (url (consult--lookup-cdr nil my-consult-sample--candidates candidate)))
    (browse-url-default-browser url)))

(defvar my-consult-sample--candidates
  '(("google" . "http://google.com/")
    ("yahoo"  . "http://yahoo.com/")))

(defun my-consult-sample ()
  "Sample function using consult"
  (interactive)
  (require 'consult)
  (let (selected
   	url)
    (setq selected (consult--read my-consult-sample--candidates
				  :keymap my-consult-sample-map))
    (setq url (consult--lookup-cdr nil my-consult-sample--candidates selected))
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

(defun my-consult-dired-ripgrep-on-windows ()
  ;; https://github.com/minad/consult/issues/419
  (interactive)
  (let ((consult-ripgrep-args ; Version 0.10 or above
	 ;; "rg --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --line-number ." ; original
	 ;; replace '.' with "%s" (files) and add "-e ARG OPTS"
	 (format "rg --line-buffered --color=never --max-columns=1000 --path-separator \\\ --smart-case --no-heading --line-number %s -e ARG OPTS"
		 (mapconcat #'shell-quote-argument (dired-get-marked-files) " "))))
    (message "%S" consult-ripgrep-args)
    (consult-ripgrep nil "pattern -- --ignore-case --hidden")))

;; (when (eq (window-system) 'w32)
;;   (setq consult-find-args "find . -not ( -wholename \\*/.\\* -prune)"))

(provide '.consult)
