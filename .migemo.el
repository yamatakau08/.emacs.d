(use-package migemo ; migemo refer http://0xcc.net/migemo/
  :ensure t

  :if (if (executable-find "cmigemo") ; should set cmigemo's path in exec-path be set in init.el
	  t
	(message "[debug] .migemo.el Install cmigemo for on windows https://www.kaoriya.net/software/cmigemo/")
	nil)

  :custom
  (migemo-options
   (cond ((eq system-type 'windows-nt)
	  ;;(append migemo-options '("-i" "\a"))) ; fail,because this block is executed before (require migemo)
	  '("-q" "--emacs" "-i" "\a")) ; set the value explicitly
	 (t
	  '("-q" "--emacs")); pass set the value explicitly
	  ))

  ;; by setting migemo-directory, migemo-dictionary is also set
  (migemo-directory
   (cond ((eq system-type 'windows-nt)
	  "c:/yama/.emacs.d/conf/migemo/dict/utf-8")
	 ((eq system-type 'darwin)
	  "/usr/local/share/migemo/utf-8")
	 ((eq system-type 'gnu/linux)
	  "/usr/share/cmigemo/utf-8")))

  (migemo-user-dictionary  nil) ; don't use user-dictionary
  (migemo-regex-dictionary nil)

  :config
  (cond
   ((eq system-type 'windows-nt)
    ;; https://nagayasu-shinya.com/emacs-cmigemo-windows/
    ;; の説明にあるような環境変数等を設定しなくても動く

    ;; http://grugrut.hatenablog.jp/entry/2015/04/emacs-migemo-on-windows
    ;; の"正しい設定" より、Windows環境の外部コマンドを使うような場合には、絶対パスで記述する方がよいようだ。
    ;; が、相対パスでも動作する

    ;; file suffix .exeを指定しなくても動作するが、.exe を付けておく。
    ;; file-exist-p 等のファイル存在確認時に、.exe がないと nil になってしまう
    ;; (setq migemo-command "c:/yama/bin/cmigemo-default-win64/cmigemo.exe")
    ;; 相対パスでも動作する
    ;; (setq migemo-command "~/bin/cmigemo-default-win64/cmigemo")
    (setq migemo-command "~/bin/cmigemo-default-win64/cmigemo.exe")

    ;;(setq migemo-options '("-q" "--emacs" "-i" "\a"))

    ;;(setq migemo-coding-system 'cp932-unix) ; default is 'utf-8-unix, both 'utf-8-unix and 'cp932-unix work on windows environment
    ;;(setq migemo-dictionary "c:/yama/.emacs.d/conf/migemo/dict/cp932/migemo-dict")
    ;; 辞書ファイルは、Windowsのfile表記形式で設定 cmigemo.exeがdosベースのコマンドなので。
    ;; refer http://grugrut.hatenablog.jp/entry/2015/04/emacs-migemo-on-windows "正しい設定"
    ;; migemo-dictionary は辞書ファイルのあるdirectoryを設定する
    (setq migemo-dictionary "c:/yama/.emacs.d/conf/migemo/dict/utf-8/migemo-dict")
    )

   ((eq system-type 'cygwin) ; cygwin terminal emacs
    ;; 'cygwin時も'windows-nt で代用可能かと試したが動作せず
    (setq migemo-command "~/bin/cmigemo-cygwin/build/cmigemo")

    ;; cmigemo --help doesn't show -i \a
    ;;(setq migemo-options '("-q" "--emacs" "-i" "\a")) ; work
    ;; (setq migemo-options '("-q" "--emacs" "-i"))      ; work
    ;; (setq migemo-options '("-q" "--emacs"))           ; work
    ;; (setq migemo-coding-system 'cp932-unix) ; default is 'utf-8-unix, both 'utf-8-unix and 'cp932-unix work on windows environment
    ;; (setq migemo-dictionary "c:/yama/.emacs.d/conf/migemo/dict/cp932/migemo-dict")
    ;; 辞書ファイルは、Windowsのfile表記形式で設定'windows-nt時と同様
    (setq migemo-dictionary "c:/yama/.emacs.d/conf/migemo/dict/utf-8/migemo-dict")
    ))
   )
