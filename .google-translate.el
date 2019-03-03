; https://qiita.com/tadsan/items/6c658cc471be61cbc8f6
; "設定ファイルの書き方"より、packageを使用する場合
; load-path追加必要無し
;(add-to-list 'load-path "~/.emacs.d/elpa/google-translate-20170713.119")
; require する必要無し 
;(require 'google-translate)
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
  (if (string-match "\\cj" (thing-at-point 'word)) ; utilize '\\cj' is used in "sdic"
      (%google-translate-at-point nil t)
    (%google-translate-at-point nil nil)
    )
  )

;;; window (buffer with translation) gets focus in google-translate-core-ui.el
(setq google-translate-pop-up-buffer-set-focus t)

;;; push note into Anki web directly
(setq gt-anki-deck "英語") ; gt: google-translate
(defun my-google-translate-register-item (from to)
  (interactive
   (if (string= "*Google Translate*" (buffer-name))
       (let ((from (read-string "From: " (thing-at-point 'word)))
	     (to (read-string "To: ")))
	 (list from to))))
  ;; defined in ~/.anki-editor
  (my-anki-editor-push-note gt-anki-deck from to)
)

;;; advice to enable "r" key to register items in "*Google Translate*" buffer
(defun google-translate-buffer-insert-translation-advice (&rest args)
  (local-set-key "r" 'my-google-translate-register-item))
(advice-add 'google-translate-buffer-insert-translation :before
	    #'google-translate-buffer-insert-translation-advice)
