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

(require 'google-translate-default-ui) ; need for my-google-translate-at-point

;(global-set-key "\C-ct" 'google-translate-at-point)
(global-set-key "\C-ct" 'my-google-translate-at-point)
(global-set-key "\C-cT" 'google-translate-query-translate)

(setq google-translate-default-source-language "en"
      google-translate-default-target-language "ja")

;;; see google-translate-default-ui.el
(global-set-key (kbd "C-c r") 'google-translate-at-point-reverse)
(global-set-key (kbd "C-c R") 'google-translate-query-translate-reverse)

;;;
;;; my original function is enable not to care the word is English or Japanese
;;;
(defun my-google-translate-at-point ()
  (interactive)
  (if (string-match "\\cj" (thing-at-point 'word))
      (%google-translate-at-point nil t)
    (%google-translate-at-point nil nil)
    )
  )

;;; window (buffer with translation) gets focus in google-translate-core-ui.el
(setq google-translate-pop-up-buffer-set-focus t)

(setq mydic_org "~/.emacs.d/dict/mydic.org") ; share for sdic

(defun my-google-translate-register-item (from to)
  (interactive
   (if (string= "*Google Translate*" (buffer-name))
       (let ((from (read-string "From: " (thing-at-point 'word)))
	     (to (read-string "To: ")))
	 (list from to))))
  (if (string-match "\\cj" from)
      (progn (setq jword from)
	     (setq eword to))
    (progn (setq eword from)
	   (setq jword to)))
  ; refer https://sleepy-yoshi.hatenablog.com/entry/20110322/p1
  (with-temp-buffer
    (insert (format "* English :drill:\n%s\n** Answer\n%s\n" eword jword))
    (append-to-file nil t mydic_org))
)
