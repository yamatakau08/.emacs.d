;;; https://qiita.com/tadsan/items/6c658cc471be61cbc8f6
;;; "設定ファイルの書き方"より、packageを使用する場合
;;; load-path追加必要無し
;;; (add-to-list 'load-path "~/.emacs.d/elpa/google-translate-20170713.119")
;;; require する必要無し 
;;; (require 'google-translate)

;;; for registering word in Anki
(add-to-list 'load-path "~/.emacs.d/my-anki-connect")
(require 'my-anki-connect)

(require 'google-translate-default-ui) ; need for my-google-translate-at-point

;;;
(setq google-translate-default-source-language "en"
      google-translate-default-target-language "ja")

;(global-set-key "\C-ct" 'google-translate-at-point)
;(global-set-key "\C-cT" 'google-translate-query-translate)
(global-set-key "\C-ct" 'my-google-translate-at-point)
(global-set-key "\C-cT" 'my-google-query-translate)

;;; see google-translate-default-ui.el
(global-set-key (kbd "C-c r") 'google-translate-at-point-reverse)
(global-set-key (kbd "C-c R") 'google-translate-query-translate-reverse)

;;;
;;; my original function is enable not to care if the word is English or Japanese
;;;
(defun my-google-translate-at-point ()
  (interactive)
  (if (string-match "\\cj" (thing-at-point 'word)) ; utilize '\\cj' is used in "sdic"
      (%google-translate-at-point nil t)
    (%google-translate-at-point nil nil)
    )
  )

;;;
;;;
;;;
(defun my-google-query-translate ()
  (interactive)
  (let ((from (read-string "From: ")))
	(if (string-match "\\cj" from)
	    (google-translate-translate "ja" "en" from nil)
	  (google-translate-translate "en" "ja" from nil))))

;;; window (buffer with translation) gets focus in google-translate-core-ui.el
(setq google-translate-pop-up-buffer-set-focus t)

;;;
(setq gt-anki-push-deck "Default") ; gt: google-translate
;(setq gt-anki-push-note-func my-anki-editor-push-note)
(setq gt-anki-push-note-func 'my-anki-connect-push-note)

(defun my-google-translate-register-item-in-anki ()
  "push note which have from and to word Basic note-type in Anki deck \
   specified gt-anki-push-deck variable through AnkiConnect"
  (interactive
   (if (string= "*Google Translate*" (buffer-name))
       (let ((front (read-string "Front: " (thing-at-point 'word)))
	     (back  (read-string "Back : " )))
	 (funcall gt-anki-push-note-func gt-anki-push-deck front back) ; my function defined in anki-connect.el
	 ))))

;;; advice to enable "r" key to register items in "*Google Translate*" buffer
(defun google-translate-buffer-insert-translation-advice (&rest args)
  (local-set-key "r" 'my-google-translate-register-item-in-anki))

(advice-add 'google-translate-buffer-insert-translation :before
	    #'google-translate-buffer-insert-translation-advice)
