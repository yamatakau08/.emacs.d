;;; for emergency debug
(setq debug-on-error  nil) ; enable enter debugger if an error is signaled
(setq debug-on-signal nil) ; requested to set by wl maintainer when email the bug on wanderlust
(setq debug-on-quit   nil) ; enable interrupt C-g when Emacs is super slow.

;; Since default directory is set / when launching Emacs on Mac from dock,
;; set it HOME directory.
(setq default-directory (getenv "HOME"))
;; https://qiita.com/t2psyto/items/05776f010792ba967152
(setq command-line-default-directory (format "%s/" (getenv "HOME")))

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
(my-load "~/.emacs.d/.vc.el") ; (custom-set-variables '(vc-handled-backends nil)) to suppress Emacs file open is extremly slow
(my-load "~/.emacs.d/.files.el") ; no backup
(my-load "~/.emacs.d/.simple.el") ; line-number,column-number in mode line
(my-load "~/.emacs.d/.time.el")
;;(my-load "~/.emacs.d/.mode-line.el")
(my-load "~/.emacs.d/.frame.el")

(my-load "~/.emacs.d/.url-vars.el")
(my-load "~/.emacs.d/.info.el")
(my-load "~/.emacs.d/.paren.el")
;;(my-load "~/.emacs.d/.dired.el") ; replace .openwith.el in dired-mode
(my-load "~/.emacs.d/.dired-x.el") ; only disable key assign
(my-load "~/.emacs.d/.thingatpt.el")
(my-load "~/.emacs.d/.ffap.el")
(my-load "~/.emacs.d/.windmove.el")
(my-load "~/.emacs.d/.ansi-color.el")
(my-load "~/.emacs.d/.whitespace.el")
(my-load "~/.emacs.d/.autoinsert.el")
(my-load "~/.emacs.d/.quickurl.el")
(my-load "~/.emacs.d/.shell.el")
(my-load "~/.emacs.d/.ruby-mode.el")
(my-load "~/.emacs.d/.nxml-mode.el")
(my-load "~/.emacs.d/.sgml-mode.el")
(my-load "~/.emacs.d/.hideshow.el")
(my-load "~/.emacs.d/.flyspell.el")
(my-load "~/.emacs.d/.calendar.el")
(my-load "~/.emacs.d/.cc-mode.el")
(my-load "~/.emacs.d/.url-vars.el") ;; set proxy, should be loaded after company-network-p,use-package is active
(my-load "~/.emacs.d/.webjump.el")
(my-load "~/.emacs.d/.saveplace.el")

;;
;; package
;;
(my-load "~/.emacs.d/.ddskk.el")
(my-load "~/.emacs.d/.howm.el")
(my-load "~/.emacs.d/.request.el")
(my-load "~/.emacs.d/.google-translate.el")
(my-load "~/.emacs.d/.esqlite.el") ; for my own tool eced
(my-load "~/.emacs.d/.migemo.el")
;;(my-load "~/.emacs.d/.origami.el")
;;(my-load "~/.emacs.d/.search-web.el")

;; helm
(my-load "~/.emacs.d/.helm.el")
(my-load "~/.emacs.d/.helm-swoop.el")
(my-load "~/.emacs.d/.helm-ag.el")
(my-load "~/.emacs.d/.helm-posframe.el")
;;(my-load "~/.emacs.d/.helm-google.el")
(my-load "~/.emacs.d/.helm-chrome.el")

(my-load "~/.emacs.d/.magit.el")
(my-load "~/.emacs.d/.wl.el")
(my-load "~/.emacs.d/.sdic.el")
(my-load "~/.emacs.d/.company.el")

;; org-mode
(my-load "~/.emacs.d/.org.el")
(my-load "~/.emacs.d/.ox-html.el")
(my-load "~/.emacs.d/.ox-confluence.el")

(my-load "~/.emacs.d/.visual-regexp.el")
(my-load "~/.emacs.d/.dumb-jump.el")
(my-load "~/.emacs.d/.dired-posframe.el")
(my-load "~/.emacs.d/.frog-jump-buffer.el")
(my-load "~/.emacs.d/.tabbar.el")
;;(my-load "~/.emacs.d/.centaur-tabs.el")
(my-load "~/.emacs.d/.request.el") ; curl for anki/jira/confluence
(my-load "~/.emacs.d/.macrostep.el")
(my-load "~/.emacs.d/.flycheck.el")
(my-load "~/.emacs.d/.ruby-refactor.el")
(my-load "~/.emacs.d/.emr.el")
(my-load "~/.emacs.d/.japanese-holidays.el")

;;(my-load "~/.emacs.d/.jiralib2.el")
(my-load "~/.emacs.d/.helm-jira.el")
;;(my-load "~/.emacs.d/.org-drill.el")
;;(my-load "~/.emacs.d/.anki-editor.el")
;;(my-load "~/.emacs.d/.ivy.el")
;;(my-load "~/.emacs.d/.counsel.el")
;;(my-load "~/.emacs.d/.openwith.el")
;;(my-load "~/.emacs.d/.all-the-icons.el")
;;(my-load "~/.emacs.d/.all-the-icons-dired.el")
;;(my-load "~/.emacs.d/.password-cache.el")
(my-load "~/.emacs.d/.beacon.el")
(my-load "~/.emacs.d/.dired-narrow.el")
(my-load "~/.emacs.d/.dired-subtree.el")
(my-load "~/.emacs.d/.helm-chrome.el")
;;(my-load "~/.emacs.d/.selectrum.el")
;;(my-load "~/.emacs.d/.selectrum-prescient.el")

(my-load "~/.emacs.d/.ppp.el")
(my-load "~/.emacs.d/.seml.el")

;;
;; useful packages are not registerd in elpa ...
;;
;;(my-load "~/.emacs.d/google.el") ; replace with new function defined in .webjump.el
(my-load "~/.emacs.d/.instant-maximized-window.el")
(my-load "~/.emacs.d/.run-assoc.el")

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
;;(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;;(when (file-exists-p custom-file)
;;  (load custom-file))
;; don't use custom-file and put the contents into nul-device
(setq custom-file null-device)
