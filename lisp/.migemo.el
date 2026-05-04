(use-package migemo
  ;; About migemo itself, refer http://0xcc.net/migemo/
  ;; get `cmigemo` on Windows from https://www.kaoriya.net/software/cmigemo/

  :ensure t

  :init
  (if (eq system-type 'windows-nt)
      (add-to-list 'exec-path "~/.emacs.d/bin/cmigemo-default-win64")
    (add-to-list 'exec-path "/usr/local/bin")
    (add-to-list 'exec-path "~/.nix-profile/bin"))

  :custom
  ;; migemo options
  ;; Since the defualt settings is available on windows, comment this part
  ;; (migemo-options
  ;;  (cond ((eq system-type 'windows-nt)
  ;; 	  ;; (append migemo-options '("-i" "\a"))) ; fail,because this block is executed before (require migemo) on use-package
  ;;      ;; cmigemo --help doesn't show -i \a
  ;; 	  '("-q" "--emacs" "-i" "\a")) ; in :custom block, should set the all value explicitly
  ;;      ;; '("-q" "--emacs" "-i" "\a") or '("-q" "--emacs" "-i") or '("-q" "--emacs") is default,all work,so the comment this migemo-options setting
  ;; 	 (t
  ;; 	  '("-q" "--emacs")) ; pass, set the value explicitly
  ;; 	  ))

  ;; by setting migemo-directory
  ;; migemo-dictionary, migemo-user-dictionary, migemo-regex-dictionary are also set.
  (migemo-directory "~/.emacs.d/conf/migemo/dict/utf-8")

)

(provide '.migemo)

;; memo on windows
;; http://grugrut.hatenablog.jp/entry/2015/04/emacs-migemo-on-windows の"正しい設定"
;; Windows環境の外部コマンドを使うような場合には、絶対パスで記述する方がよいようだ。が、相対パスでも動作する

;; migemo-command
;; 相対パス設定でも動作する
;; file suffix `.exe` を指定しなくても動作するが `.exe` を付けておく方が良い。
;; file-exist-p 等のファイル存在確認時に、`.exe` がないと nil になってしまうので。
;; (setq migemo-command "c:/yama/bin/cmigemo-default-win64/cmigemo.exe") => t
;; (setq migemo-command "~/bin/cmigemo-default-win64/cmigemo.exe") => t
;; (setq migemo-command "~/bin/cmigemo-default-win64/cmigemo") => nil

;; migemo-options
;; (setq migemo-options '("-q" "--emacs" "-i" "\a"))
;; default の '("-q" "--emacs") で動作する

;; migemo-coding-system
;; default is 'utf-8-unix, both 'utf-8-unix and 'cp932-unix work on Windows environment
;; (setq migemo-coding-system 'cp932-unix)

;; migemo-dictionary
;; 辞書ファイルがある "directory" を設定する
;; http://grugrut.hatenablog.jp/entry/2015/04/emacs-migemo-on-windows の "正しい設定" より
;; Windows file path表記形式で設定 cmigemo.exe(dosコマンド)が辞書ファイルを参照するので。
;; (setq migemo-dictionary "c:/yama/.emacs.d/conf/migemo/dict/utf-8/migemo-dict")
