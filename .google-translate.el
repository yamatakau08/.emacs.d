; packageを使用する場合、
; load-pathに追加必要無し
;(add-to-list 'load-path "~/.emacs.d/elpa/google-translate-20170713.119")
; require する必要無し 
; https://qiita.com/tadsan/items/6c658cc471be61cbc8f6
; "設定ファイルの書き方"より、
;(require 'google-translate)
;(require 'google-translate-default-ui)

(global-set-key "\C-ct" 'google-translate-at-point)
(global-set-key "\C-cT" 'google-translate-query-translate)

(setq google-translate-default-source-language "en"
      google-translate-default-target-language "ja")
