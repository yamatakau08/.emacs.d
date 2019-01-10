;;; 個人設定読み込み
(if (file-exists-p "~/.emacs.d/private.el")
    (load "~/.emacs.d/private.el"))

;;; check if ip-address is private
(if (file-exists-p "~/.emacs.d/get-ip-address.el")
    (load "~/.emacs.d/get-ip-address.el"))

;; package
;(package-initialize) ; need this line with comment! to prevent form inserting the message automatically 
(load "~/.emacs.d/.package.el")

(set-language-environment "Japanese")
(setenv "TZ" "JST-9") ; gnu サイトから入手したWindows binaryだと、time-zoneが日本になっていないので、実時間と mode-line 時間表示が異なるので設定
;;; no backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;;; 改行コードを、DOS等の環境名ではなくコード名で表示する
;;; http://d.hatena.ne.jp/mhrs/20061227/p2
(setq eol-mnemonic-unix "(LF)")
(setq eol-mnemonic-dos  "(CRLF)")
(setq eol-mnemonic-mac  "(CR)")

;;; 新規作成時のファイルの文字コードを utf-8-unix
(set-default-coding-systems 'utf-8-unix)

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

;;; window setting
(load "~/.emacs.d/window-setting.el")

;;; font setting
(load "~/.emacs.d/font-setting.el")

;;; mode-line setting
(load "~/.emacs.d/.mode-line.el")

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
(if (eq system-type 'windows-nt)
    (load "~/.emacs.d/.cygwin-mount.el"))

;;; org-mode
(load "~/.emacs.d/.org-mode.el")

;;; helm-google
;(load "~/.emacs.d/.helm-google.el")
