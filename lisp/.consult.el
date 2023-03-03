(use-package consult

  ;;:init
  ;;(require 'bookmark+) ; Since sometimes fail to call bookmark+ function, explicitly load.

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
  ;; (completion-styles '(substring basic))
  ;; see step 4. https://github.com/minad/consult#bug-reports
  ;; without completion-styles setting, consult--line doesn't work well
  ;; since consult recommend completion-styles 'orderless add conditon "(if (featurep 'orderless) ...)"
  ;; in .orderless, I set completion-styles as '(orderless basic)

  ;; see the comment :config section
  (completion-styles (if (featurep 'orderless)
			 '(orderless basic)
		       '(substring basic)))

  :bind
  (("M-g g" . consult-goto-line)
   ("C-x b" . consult-buffer)
   ("C-s"   . consult-line-symbol-at-point)
   ("C-r"   . consult-line-symbol-at-point)
   ("C-x f" . my-consult-file-externally)
   ("C-c b" . consult-bookmark)
   ;; M-s bindings (search-map)
   ;;("M-s r" . consult-ripgrep1)
   )

  :config
  ;;(add-to-list 'completion-styles 'substring) ; conao3 advice, see the comment :custom section in .orderless.el

  (recentf-mode) ; enable for consult-recetf-file command, refere https://github.com/minad/consult#virtual-buffers

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

  ;;
  ;; consult-line
  ;;
  (defun consult-line-symbol-at-point ()
    (interactive)
    (consult-line (thing-at-point 'symbol)))

  ;;
  ;; consult-buffer
  ;;
  (defvar my-consult-buffer--map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-j") #'my-consult-buffer--jump)
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

  ;; Configure initial narrowing per command
  ;; https://github.com/minad/consult/wiki#start-command-with-initial-narrowing
  ;; https://github.com/minad/consult/issues/450 my question
  ;; stop using this since 2022/09/28, if available this again, unncomment the following block
  ;;(defvar consult-initial-narrow-config
  ;;  '((consult-buffer . ?m))) ; bookmark

  ;; Add initial narrowing hook
  ;; stop using this since 2022/09/28
  ;;(defun consult-initial-narrow ()
  ;;  (when-let (key (alist-get this-command consult-initial-narrow-config))
  ;;    (setq unread-command-events (append unread-command-events (list key 32)))))
  ;;(add-hook 'minibuffer-setup-hook #'consult-initial-narrow)

  (consult-customize
   consult-buffer
   ;;:preview-key nil
   ;;:preview-key (kbd "M-.") ; not work
   ;; but I still enable this setting,
   ;; this have an error consult--key-parse: [134217774] is not a valid key definition; see `key-valid-p' since conslut Version: 0.32?
   ;; refer https://github.com/minad/consult#multiple-sources
   ;; By default, consult-buffer previews buffers, bookmarks and files.
   ;; Loading recent files or bookmarks can result in expensive operations.
   ;; However it is possible to configure a manual preview as follows.
   ;; referring the above, I understand this :preview-key for manual preview,
   ;; so comment this and select the default behavior.
   ;;:preview-key '(:debounce 3 any)
   :keymap my-consult-buffer--map
   ;;:initial "m" ; bookmark not work
   ;;:narrow ?m ; error
   ;;:name     "Bookmark" ; error
   )

  ;;
  ;; consult-grep
  ;;
  (consult-customize
   consult-grep
   :preview-key '(:debounce 3 any))

  ;;
  ;; consult-ripgrep
  ;;
  (consult-customize
   consult-ripgrep
   ;;:preview-key  (kbd "M-]") ; not work
   :preview-key '(:debounce 3 any)
   )

  (defun my-consult-ripgrep (&optional dir inital)
    (interactive "P")
    (consult--minibuffer-with-setup-hook ; to move point beginning
	(lambda ()
	  (beginning-of-line)
	  (forward-char))
      (consult--grep "Ripgrep" #'consult--ripgrep-builder dir "pattern -- --ignore-case --hidden --max-depth 1")))

  ;; redefine consult-bookmark
  (advice-add 'consult-ripgrep :override #'my-consult-ripgrep)

  ;;
  ;; consult-bookmark
  ;;
  (defvar my-consult-bookmark--map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-j") #'my-consult-bookmark--jump)
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

  (consult-customize
   consult-bookmark
   :preview-key nil
   :keymap my-consult-bookmark--map)

  ;; :preface Symbol's function definitons is void: consult--grep
  ;; When M-x consult-ripgrep1 just after launching Emacs, consult--grep called from consult-ripgrep1 is internal function.
  ;; So revise to call consult-ripgrep public function
  ;; :demand consult-ripgrep1 is not found when M-x just after launching Emacs
  :preface

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
    (define-key map (kbd "C-j") #'my-consult-sample--open-browser)
    map)
  "Additional keymap used by `my-consult-sample'.")

(defun my-consult-sample--open-browser ()
  "function is called when type C-l in my-consult-sample session"
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
				  :initial "yahoo"
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
