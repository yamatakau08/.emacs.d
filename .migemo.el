;(require 'migemo) ; package使用していている場合不要 
 
(cond
 ((eq system-type 'windows-nt)
  ;; https://nagayasu-shinya.com/emacs-cmigemo-windows/
  ;; の説明にあるような環境変数等を設定しなくても動く
  ;; http://grugrut.hatenablog.jp/entry/2015/04/emacs-migemo-on-windows
  ;; の"正しい設定" より、windows環境の外部コマンドを使うような場合には、絶対パスで記述する方がよいようだ。
  ;; が、相対パスでも動作する
  ;; file suffix .exeを指定しなくても動作するが、.exe を付けておくと、
  ;; file-exist-p 等での存在確認時には、.exe がないと nil になってしまう
  (setq migemo-command "C:/yama/bin/cmigemo-default-win64/cmigemo.exe")
  (setq migemo-command "~/bin/cmigemo-default-win64/cmigemo.exe") ; 相対パスでも動作する

  (setq migemo-options '("-q" "--emacs" "-i" "\a")) ;cygwin ruby では動作せず 調査必要!

  ;; windows環境では、cp932-unix,utf-8-unixどちらでも動く→utf-8-unixでしか動作しなくなった？ 
  ;; cp932-unix
  ;(setq migemo-coding-system 'cp932-unix)
  ;(setq migemo-dictionary "C:/yama/.emacs.d/conf/migemo/dict/cp932/migemo-dict")
  ;; utf-8-unix
  (setq migemo-coding-system 'utf-8-unix)
  (setq migemo-dictionary "C:/yama/.emacs.d/conf/migemo/dict/utf-8/migemo-dict"))

 ((eq system-type 'darwin)
  (setq migemo-command "/usr/local/bin/cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-coding-system 'utf-8-unix)
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict"))

 ((eq system-type 'gnu/linux)
  (setq migemo-command "/usr/bin/cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-coding-system 'utf-8-unix)
  (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict"))
)

(load-library "migemo")
(migemo-init)

(setq migemo-user-dictionary  nil)
(setq migemo-regex-dictionary nil)

;; judge if migemo-command exists
(unless (file-exists-p migemo-command)
  (message "Install migemo-command: %s" migemo-command)
  (message "web site for cmigemo on windows https://www.kaoriya.net/software/cmigemo/")
  (setq migemo-isearch-enable-p nil) ; disable migemo
)

