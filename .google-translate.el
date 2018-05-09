; https://qiita.com/tadsan/items/6c658cc471be61cbc8f6
; "設定ファイルの書き方"より、packageを使用する場合
; load-path追加必要無し
;(add-to-list 'load-path "~/.emacs.d/elpa/google-translate-20170713.119")
; require する必要無し 
;(require 'google-translate)
;(require 'google-translate-default-ui)

; Windows版 "GNU Emacs 25.3.1 (x86_64-w64-mingw32) of 2017-09-26" で、
; google-translate--search-tkk: Failed to search TKK
; の症状に遭遇
; proxy設定確認と
; https://github.com/atykhonov/google-translate/issues/52#issuecomment-265949189 より
; http://emacs.1067599.n8.nabble.com/bug-11788-url-http-does-not-properly-handle-https-over-proxy-td46070.html
; のパッチをあてる事で解決 

(global-set-key "\C-ct" 'google-translate-at-point)
(global-set-key "\C-cT" 'google-translate-query-translate)

(setq google-translate-default-source-language "en"
      google-translate-default-target-language "ja")
