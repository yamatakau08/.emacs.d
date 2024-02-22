;;; config.el -*- lexical-binding: t; -*-

;;; for emergency debug
(setq debug-on-error  nil) ; enable enter debugger if an error is signaled
(setq debug-on-signal nil) ; requested to set by wl maintainer when email the bug on wanderlust
(setq debug-on-quit   nil) ; enable interrupt C-g when Emacs is super slow.

;; Since default directory is set / when launching Emacs on Mac from dock,
;; set it HOME directory.
(setq default-directory (getenv "HOME"))
;; https://qiita.com/t2psyto/items/05776f010792ba967152
(setq command-line-default-directory (format "%s/" (getenv "HOME")))

;; for my setting files
(add-to-list 'load-path "~/.emacs.d/lisp")

;;; my-load
(defun my-load (file)
  (if (file-exists-p file)
      (load file)
    (y-or-n-p (message "%s is not found,proceed?" file))))

;;; load private settings
(if (file-exists-p "~/.emacs.d/lisp/private.el")
    (require 'private)
  (error "prepare private.el in ~/.emacs.d/lisp"))

;; check if ip-address is company's network
(require 'my-network-type)

(setenv "TZ" "JST-9") ; gnu サイトから入手したWindows binaryだと、time-zone が日本になっていないので、実時間と mode-line 時間表示が異なるので設定

(set-language-environment "Japanese")

(when (eq system-type 'windows-nt)
  ;; After execute (set-language-environment "Japanese") function have some errors on Windows Emacs + Cygwin.

  ;; Since file-encoding is set to japanese-shift-jis-unix when open the buffer for the new file, set to utf-8-unix
  ;; same as Linux, Mac environment.
  ;;(prefer-coding-system 'utf-8-unix)

  ;; suggested not to use prefer-coding-system
  (setq-default buffer-file-coding-system 'utf-8-unix)

  ;; the following is workaround for "ediff-files" with the japanese filename doesn't work,
  ;; because "diff" requires the file-name is encoding cp932
  ;;(setq default-process-coding-system '(japanese-shift-jis-dos . cp932))

  ;; but the above has negative effects.
  ;; e.g. consult-grep with search string in japanese character outputs garbled characters and the japanese filename also garbled.
  ;; so use the following is effective in case of "diff" command only.
  ;; the following is the workaround in case prefer-coding system
  ;; and is not needed in case (setq-default buffer-file-coding-system 'utf-8-unix)
  ;; but keep it enable
  (add-to-list 'process-coding-system-alist '("diff" utf-8-unix . cp932-unix))
  ;; the following is the workaround for consult-find to work with the japanese file name
  (add-to-list 'process-coding-system-alist '("find" utf-8-unix . cp932-unix))
  ;; the following is the workaround for consult-grep to work with search string in Japanese and the japanese filename
  (add-to-list 'process-coding-system-alist '("grep" utf-8-unix . cp932-unix)))

;;; refer https://qiita.com/catatsuy/items/3dda714f4c60c435bb25
;(defun set-exec-path-from-shell-PATH ()
;  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.
;This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
;  (interactive)
;  ;; fish doesn't work
;  ;;(let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
;  ;; fish work
;  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (getenv "PATH"))))
;    (setenv "PATH" path-from-shell)
;    (setq exec-path (split-string path-from-shell path-separator))))
;
;(set-exec-path-from-shell-PATH)

;; without this, migemo doesn't work and orderless using migemo also doesn't work.
;; move to ~/.emacs.d/lisp/.migemo.el
;;(add-to-list 'exec-path "~/bin/cmigemo-default-win64")
;;(add-to-list 'exec-path "/usr/local/bin")

;; for "git clone" or one file get which is not get by package system
(add-to-list 'load-path "~/.emacs.d/my-git-source-get")
(require 'my-git-source-get)

;;
;; for package manage
;;
(require 'url) ; the below .url-var use url-proxy-services in url.el variable
(require '.url-vars) ; set proxy, should be loaded after company-network-p,use-package is active
;; (if (eq system-type 'gnu/linux)
;;     ;;(my-git-source-get "git@github.com:raxod502/straight.el.git" "straight/repos"))
;;     (message "execute the following command manually")
;;     (message "git clone git@github.com:raxod502/straight.el.git straight/repos/straight.el"))
(require '.straight) ; call before "package" to succeed downloading on Emacs 27.0.50, Linux
(require '.package)

;; emacs
(use-package emacs
  :no-require t
  :custom
  ;; for model-line
  ;; 改行コードを、DOS等の環境名ではなくコード名で表示する
  ;; https://qiita.com/kai2nenobu/items/ddf94c0e5a36919bc6db
  (eol-mnemonic-unix "(LF)")
  (eol-mnemonic-dos  "(CRLF)")
  (eol-mnemonic-mac  "(CR)")

  (use-short-answers t) ; introuduced by https://blog.web-apps.tech/y-or-n-for-kill-modified-buffer/

  ;; When login shell is fish shell, shell-file-name is set "/bin/fish".
  ;; Since that makes some packaes xref, my-plantuml... doesn't work," then have an error "No such file or directory, /bin/fish", set shell-file-name explicitly to /bin/bash.
  ;; After that, found dired-do-shell-command "tar zxvf" and eshell also doesn't work with the same reason.
  (shell-file-name
   (let ((cygwin-shell-file-name "c:/cygwin64/bin/bash.exe")) ;; should use "c:/...",without ".exe" is also available
     ;; when set "/bin/bash", it doesn't work well have error "shell-command-on-region: Searching for program: No such file or directory, /bin/bash"
     (if (eq system-type 'windows-nt)
	 cygwin-shell-file-name
       "/bin/bash")))
  )

;(message "[debug] befor exec-path: %s" exec-path)
;(use-package exec-path-from-shell
;  :ensure t
;  :config (exec-path-from-shell-initialize)
;  )
;(message "[debug] after exec-path: %s" exec-path)

;;
;; library
;;
(require '.vc) ; (custom-set-variables '(vc-handled-backends nil)) to suppress Emacs file open is extremly slow
(require '.files) ; no backup
(require '.simple) ; line-number,column-number in mode line
(require '.time)
(require '.frame)

(require '.info)
(require '.paren)
(require '.dired)
(require '.dired-x) ; only disable key assign
(require '.thingatpt)
(require '.ffap)
(require '.windmove)
(require '.ansi-color)
(require '.whitespace)
(require '.autoinsert)
(require '.quickurl)
(require '.shell)
(require '.ruby-mode)
(require '.nxml-mode)
(require '.sgml-mode)
(require '.hideshow)
(require '.ispell)
;;(require '.flyspell)
(require '.calendar)
(require '.cc-mode)
(require '.webjump)
(require '.saveplace)
(require '.hl-line)
;;(require '.warnings)
(require '.bookmark)
(require '.bookmark+)
(require '.pixel-scroll)
(require '.ediff)

;;
;; package
;;
(require '.ddskk)
(require '.howm)
(require '.request)  ; curl for anki/jira/confluence
(require '.google-translate)
(require '.esqlite) ; for my own tool eced
(require '.migemo)
;;(require '.origami)
;;(require '.search-web)

;; helm
(require '.helm)
(require '.helm-swoop)
(require '.helm-ag)
(require '.helm-posframe)
;;(require '.helm-google)
(require '.helm-chrome)

(require '.magit)
(require '.wl)
(require '.sdic)
(require '.company)

;; org-mode
(require '.org)
(require '.ox-html)
(require '.ox-confluence)
(require '.company-org-block)

(require '.visual-regexp)
(require '.dumb-jump)
(require '.dired-posframe)
(require '.frog-jump-buffer)
(require '.tabbar)
;;(require '.centaur-tabs)
(require '.macrostep)
(require '.flycheck)
(require '.ruby-refactor)
(require '.emr)
(require '.japanese-holidays)

;;(require '.jiralib2)
(require '.helm-jira)
;;(require '.org-drill)
;;(require '.anki-editor)
;;(require '.ivy)
;;(require '.counsel)
;;(require '.openwith)
;;(require '.all-the-icons)
;;(require '.all-the-icons-dired)
;;(require '.password-cache)
(require '.beacon)
(require '.dired-narrow)
(require '.dired-subtree)
(require '.dired-filter)
;;(require '.selectrum)
;;(require '.selectrum-prescient)

(require '.ppp)
;;(require '.seml-mode)

(require '.consult)
(require '.compat)
(require '.vertico)
(require '.orderless)
(require '.marginalia)
(require '.embark)

(require '.consult-dir)

;(require '.affe)
(require '.wgrep)

(require '.fish-mode)

;;(require '.ag)
;;(require '.wgrep-ag)

(require '.plantuml-mode)


;;
;; useful packages are not registerd in elpa ...
;;
;;(require 'google) ; replace with new function defined in .webjump.el
(require '.instant-maximized-window)
;;(require '.run-assoc)
;;(require '.col-highlight) ; this makes point move super slow in message buffer has json big data.

;;
;; my-own function, utility etc...
;;
(require 'my-app-open-file) ; to open file associated application
;;(require 'my-skips)
;;(require '.my-anki-connect)
(require '.eced-menu) ; english conversation expression dictionary

(require '.my-confluence)
(require '.helm-confluence)
(require '.my-anki-browse)
(require '.helm-anki-browse)
(require '.my-qiita)

(require '.my-plantuml)
(require '.my-say)

(require 'ffmpegcomp)

;; developing
;;(require '.yahoo-weather-mode.el)
;;(require '.weatherline-mode.el)

;; custom-set-variables,faces is automatically added in the end of init.el
;; when that function uses variables are executed.
;; To avoid to that, use the following settings
;; https://ymotongpoo.hatenablog.com/entry/2017/11/07/000921
(auto-insert-mode nil) ; to avoid autoinsert not to work when creating custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
(auto-insert-mode t)

;; don't use custom-file and put the contents into nul-device
;;(setq custom-file null-device)
;; in case of custom-file is set to null-device, the folloing message happens, when also executes M-x list-package.
;; File exists, but cannot be read
;; Saving file c:/yama/NUL...
;; Wrote c:/yama/NUL
;; helm-M-x-execute-command: Renaming: Invalid argument, c:/yama/tmp1JWWLO, c:/yama/NUL
;; Package refresh done

(emacs-init-time) ; not put out in Message bffer

(require '.my-modus-themes)

(provide 'init)
;;; init.el ends here
