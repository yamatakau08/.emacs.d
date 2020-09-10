(use-package migemo ; migemo refer http://0xcc.net/migemo/
  :ensure t

  :custom
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
    ;;  (setq migemo-command "c:/yama/bin/cmigemo-default-win64/cmigemo.exe")
    ;; 相対パスでも動作する
    ;; (setq migemo-command "~/bin/cmigemo-default-win64/cmigemo")
    (setq migemo-command "~/bin/cmigemo-default-win64/cmigemo.exe")

    (setq migemo-options '("-q" "--emacs" "-i" "\a"))

    ;; windows環境では、cp932-unix,utf-8-unixどちらでも動く
    ;; cp932-unix
    ;;(setq migemo-coding-system 'cp932-unix)
    ;;(setq migemo-dictionary "c:/yama/.emacs.d/conf/migemo/dict/cp932/migemo-dict")
    ;; utf-8-unix
    (setq migemo-coding-system 'utf-8-unix) ; no need migemo.el set 'utf-8-unix as default
    ;; 辞書ファイルは、Windowsのfile表記形式で設定 cmigemo.exeがdosベースのコマンドなので。
    ;; refer http://grugrut.hatenablog.jp/entry/2015/04/emacs-migemo-on-windows "正しい設定"
    ;; migemo-dictionary は辞書ファイルのあるdirectoryを設定する
    (setq migemo-dictionary "c:/yama/.emacs.d/conf/migemo/dict/utf-8/migemo-dict")
    )

   ((eq system-type 'cygwin) ; cygwin terminal emacs
    ;; 'cygwin時も'windows-nt で代用可能かと試したが動作せず
    (setq migemo-command "~/bin/cmigemo-cygwin/build/cmigemo")

    ;; cmigemo --help doesn't show -i \a
    (setq migemo-options '("-q" "--emacs" "-i" "\a")) ; work
					; (setq migemo-options '("-q" "--emacs" "-i"))      ; work
					; (setq migemo-options '("-q" "--emacs"))           ; work

					;  (setq migemo-coding-system 'cp932-unix)
					;  (setq migemo-dictionary "c:/yama/.emacs.d/conf/migemo/dict/cp932/migemo-dict")

    ;; 辞書ファイルは、Windowsのfile表記形式で設定'windows-nt時と同様
    (setq migemo-coding-system 'utf-8-unix)
    (setq migemo-dictionary "c:/yama/.emacs.d/conf/migemo/dict/utf-8/migemo-dict")
    )

   ((eq system-type 'darwin)
    (setq migemo-command "/usr/local/bin/cmigemo")
    (setq migemo-options '("-q" "--emacs"))
    (setq migemo-coding-system 'utf-8-unix)
    (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
    )

   ((eq system-type 'gnu/linux)
    (setq migemo-command "/usr/bin/cmigemo")
    (setq migemo-options '("-q" "--emacs"))
    (setq migemo-coding-system 'utf-8-unix)
    (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
    )
   )

  ;; judge if migemo-command exists
  (unless (file-exists-p migemo-command)
    (message "Install migemo-command: %s" migemo-command)
    (message "web site for cmigemo on windows https://www.kaoriya.net/software/cmigemo/")
    (setq migemo-isearch-enable-p nil) ; disable migemo
    )
  )
