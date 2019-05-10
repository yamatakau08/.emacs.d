;;; for emergency debug
(setq debug-on-error  t) ; enable enter debugger if an error is signaled
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
(load "~/.emacs.d/company-network-p.el")

;; package
(load "~/.emacs.d/.package.el")

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
(my-load "~/.emacs.d/my-app-open-file.el")

;;; window setting
(load "~/.emacs.d/window-setting.el")

;;; font setting
(load "~/.emacs.d/font-setting.el")

;;; mode-line setting
(load "~/.emacs.d/.mode-line.el")

;;; cursr setting
(load "~/.emacs.d/.cursor-setting.el")

;;; ddskk
(load "~/.emacs.d/.ddskk.el")

;;; howm
(load "~/.emacs.d/.howm.el")

;;; google-translate
(load "~/.emacs.d/.google-translate.el")

;;; migemo
(load "~/.emacs.d/.migemo.el")

;;; search-web
;(load "~/.emacs.d/.search-web.el")

;;; for google
(load "~/.emacs.d/google")

;;; dired
(load "~/.emacs.d/.dired.el")

;;; helm
(load "~/.emacs.d/.helm.el")

;;; magit
(load "~/.emacs.d/.magit.el")

;;; wl
(load "~/.emacs.d/.wl.el")

;;; cygwin-mount
(load "~/.emacs.d/.cygwin-mount.el")

;;; org-mode
(load "~/.emacs.d/.org.el")
(load "~/.emacs.d/my-skips.el")

;;; helm-google
;(load "~/.emacs.d/.helm-google.el")

;;; sdic
(load "~/.emacs.d/.sdic.el")

;;; org-drill
;(load "~/.emacs.d/.org-drill.el")

;;; anki-editor
;(load "~/.emacs.d/.anki-editor.el")

;;;
(load "~/.emacs.d/anki.el")

;;;
(load "~/.emacs.d/.tramp.el")

;;;
;(load "~/.emacs.d/.ox-confluence.el")

;;;
(load "~/.emacs.d/.company.el")

;;;
(load "~/.emacs.d/.windmove.el")

;;;
(load "~/.emacs.d/.visual-regexp.el")

;;;
(load "~/.emacs.d/.ansi-color.el")

;;;
(load "~/.emacs.d/.whitespace.el")

