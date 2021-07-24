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
(my-load "~/.emacs.d/private.el")

;; check if ip-address is company's network
(my-load "~/.emacs.d/my-network-type.el")

(set-language-environment "Japanese")
(setenv "TZ" "JST-9") ; gnu サイトから入手したWindows binaryだと、time-zone が日本になっていないので、実時間と mode-line 時間表示が異なるので設定

;;; 新規作成時のファイルの文字コードを utf-8-unix
(set-default-coding-systems 'utf-8-unix)
;;; sdic 和英検索がcygwinで正しく動作しない対応
(prefer-coding-system 'utf-8-unix)

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
(add-to-list 'exec-path "~/bin/cmigemo-default-win64")
(add-to-list 'exec-path "/usr/local/bin")

;;
;; for package, use-package
;;
(my-load "~/.emacs.d/.package.el")

;; emacs
(use-package emacs
  :no-require t
  :custom
  ;; for model-line
  ;; 改行コードを、DOS等の環境名ではなくコード名で表示する
  ;; https://qiita.com/kai2nenobu/items/ddf94c0e5a36919bc6db
  (eol-mnemonic-unix "(LF)")
  (eol-mnemonic-dos  "(CRLF)")
  (eol-mnemonic-mac  "(CR)"))

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

(require '.url-vars) ;; set proxy, should be loaded after company-network-p,use-package is active
(require '.info)
(require '.paren)
;;(require '.dired) ; replace .openwith.el in dired-mode
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
(require '.flyspell)
(require '.calendar)
(require '.cc-mode)
(require '.webjump)
(require '.saveplace)
(require '.hl-line)

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
;;(require '.selectrum)
;;(require '.selectrum-prescient)

(require '.ppp)
(require '.seml-mode)

(require '.vertico)
(require '.orderless)
(require '.consult)
(require '.marginalia)
(require '.embark)
(require '.affe)
(require '.wgrep)

(require '.fish-mode)

;;(require '.ag)
;;(require '.wgrep-ag)

;;
;; useful packages are not registerd in elpa ...
;;
;;(require 'google) ; replace with new function defined in .webjump.el
(require '.instant-maximized-window)
(require '.run-assoc)

;;
;; my-own function, utility etc...
;;
;;(my-load "~/.emacs.d/my-app-open-file.el") ; to open file associated application
(my-load "~/.emacs.d/my-skips.el")
;;(my-load "~/.emacs.d/.my-anki-connect.el")
(my-load "~/.emacs.d/.eced.el") ; english conversation expression dictionary
(my-load "~/.emacs.d/.my-confluence.el")
(my-load "~/.emacs.d/.helm-confluence.el")
(my-load "~/.emacs.d/.my-anki-browse.el")
(my-load "~/.emacs.d/.helm-anki-browse.el")
(my-load "~/.emacs.d/.my-plantuml.el")
(my-load "~/.emacs.d/.my-say.el")
(my-load "~/.emacs.d/.helm-qiita.el")

;; developing
;;(my-load "~/.emacs.d/.yahoo-weather-mode.el")
;;(my-load "~/.emacs.d/.weatherline-mode.el")

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
