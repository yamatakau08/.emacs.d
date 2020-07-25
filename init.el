;;; for emergency debug
(setq debug-on-error  nil) ; enable enter debugger if an error is signaled
(setq debug-on-signal nil) ; requested to set by wl maintainer when email the bug on wanderlust
(setq debug-on-quit   nil) ; enable interrupt C-g when Emacs is super slow.

;;; my-load
(defun my-load (file)
  (if (file-exists-p file)
      (load file)
    (y-or-n-p (message "%s is not found,proceed?" file))))

;;; to suppress Emacs file open is extremly slow
(setq vc-handled-backends nil)

;;; load private settings
(my-load "~/.emacs.d/private.el")

;;; check if ip-address is company's network
(my-load "~/.emacs.d/company-network-p.el")
(my-load "~/.emacs.d/.url-vars.el") ;; set proxy, should be after company-network-p and before .package.el

;; package
(my-load "~/.emacs.d/.package.el")

(set-language-environment "Japanese")
(setenv "TZ" "JST-9") ; gnu サイトから入手したWindows binaryだと、time-zoneが日本になっていないので、実時間と mode-line 時間表示が異なるので設定

;;; no backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;;; 新規作成時のファイルの文字コードを utf-8-unix
(set-default-coding-systems 'utf-8-unix)
;;; sdic 和英検索がcygwinで正しく動作しない対応
(prefer-coding-system 'utf-8-unix)

;;; 対応する括弧(),ブレース{},大括弧［] を強調
(show-paren-mode t)

;;; refer https://qiita.com/catatsuy/items/3dda714f4c60c435bb25
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
;; fish doesn't work
;  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
;; fish work
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (getenv "PATH"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

;;; to open file associated application
;(my-load "~/.emacs.d/my-app-open-file.el")

;;;
(my-load "~/.emacs.d/window-setting.el")

;;;
(my-load "~/.emacs.d/font-setting.el")

;;;
(my-load "~/.emacs.d/.mode-line.el")

;;;
(my-load "~/.emacs.d/.cursor-setting.el")

;;; ddskk
(my-load "~/.emacs.d/.ddskk.el")

;;;
(my-load "~/.emacs.d/.howm.el")

;;;
(my-load "~/.emacs.d/.request.el")
(my-load "~/.emacs.d/.google-translate.el")

;;;
(my-load "~/.emacs.d/.migemo.el")

;;;
;(my-load "~/.emacs.d/.search-web.el")

;;;
(my-load "~/.emacs.d/google.el")

;;; replace .openwith.el in dired-mode
;(my-load "~/.emacs.d/.dired.el")

;;
(my-load "~/.emacs.d/.dired-x.el")

;;;
(my-load "~/.emacs.d/.thingatpt.el")
(my-load "~/.emacs.d/.ffap.el")

;; helm
(my-load "~/.emacs.d/.helm.el")
;; http://extra-vision.blogspot.com/2016/09/helm-emacs.html
;; helm-migemo is not needed, is included helm itself
;(my-load "~/.emacs.d/.helm-migemo.el")
;(my-load "~/.emacs.d/.helm-google.el")
(my-load "~/.emacs.d/.helm-swoop.el")
(my-load "~/.emacs.d/.helm-ag.el")

;;;
(my-load "~/.emacs.d/.magit.el")

;;;
(my-load "~/.emacs.d/.wl.el")

;;;
(my-load "~/.emacs.d/.cygwin-mount.el")

;;; org-mode
(my-load "~/.emacs.d/.org.el")
(my-load "~/.emacs.d/.ox-html.el")
(my-load "~/.emacs.d/.ox-confluence.el")
(my-load "~/.emacs.d/my-skips.el")

;;;
(my-load "~/.emacs.d/.sdic.el")

;;;
;(my-load "~/.emacs.d/.org-drill.el")

;;;
;(my-load "~/.emacs.d/.anki-editor.el")

;;;
;;(my-load "~/.emacs.d/.my-anki-connect.el")

;;;
(my-load "~/.emacs.d/.tramp.el")

;;;
;(my-load "~/.emacs.d/.ox-confluence.el")

;;;
(my-load "~/.emacs.d/.company.el")

;;;
(my-load "~/.emacs.d/.windmove.el")

;;;
(my-load "~/.emacs.d/.visual-regexp.el")

;;;
(my-load "~/.emacs.d/.ansi-color.el")

;;;
(my-load "~/.emacs.d/.whitespace.el")

;;;
(my-load "~/.emacs.d/.dumb-jump.el")

;;;
(my-load "~/.emacs.d/.instant-maximized-window.el")

;;;
(my-load "~/.emacs.d/.info.el")

;;; nxml
(my-load "~/.emacs.d/.nxml-mode.el")

;;;
(my-load "~/.emacs.d/.sgml-mode.el")

;;;
(my-load "~/.emacs.d/.hideshow.el")

;;;
;(my-load "~/.emacs.d/.origami.el")

;;;
;(my-load "~/.emacs.d/.ivy.el")

;;;
;(my-load "~/.emacs.d/.counsel.el")

;;;
(my-load "~/.emacs.d/.ruby-mode.el")

;;;
;(my-load "~/.emacs.d/.openwith.el")

;;;
(my-load "~/.emacs.d/.run-assoc.el")

;; english conversation expression dictionary
(my-load "~/.emacs.d/.eced.el")

;;
(my-load "~/.emacs.d/.eshell.el")

;;
(my-load "~/.emacs.d/say.el")

;; jira/confluence
;(my-load "~/.emacs.d/.request.el")
;(my-load "~/.emacs.d/.jiralib2.el")
;(my-load "~/.emacs.d/.helm-jira.el")
(my-load "~/.emacs.d/.my-confluence.el")

;; test
;(my-load "~/.emacs.d/restclient_test.el")

;;
(my-load "~/.emacs.d/.all-the-icons.el")
(my-load "~/.emacs.d/.all-the-icons-dired.el")

;;
;(my-load "~/.emacs.d/.yahoo-weather-mode.el")
;(my-load "~/.emacs.d/.weatherline-mode.el")

(my-load "~/.emacs.d/.autoinsert.el")

(my-load "~/.emacs.d/.quickurl.el")

(my-load "~/.emacs.d/.my-anki-browse.el")

(my-load "~/.emacs.d/.my-plantuml.el")

;; custom-set-variables,faces is automatically added in the end of init.el
;; when that function uses variables are executed.
;; To avoid to that, use the following settings
;; https://ymotongpoo.hatenablog.com/entry/2017/11/07/000921
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
