;;; my-anki-connect.el --- function which communicate with anki-connect
;;
;; Copyright (C) 2019 by yamatakau08@gmail.com
;;
;; Author: yamatakau08@gmail.com
;; Version: 0.01
;; Package-Requires:
;; URL:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

;;;
;;; refer ank-connect supported actions
;;; https://foosoft.net/projects/anki-connect/index.html#supported-actions
;;;

(defun my-anki-connect-version ()
  "get the version"
  (let ((jsondata (json-encode '((:action  . "version")
				 (:version . 6)))))
    (my-anki-connect-push jsondata)))

;;;
(defun my-anki-connect-sync ()
  "Anki data sync with Anki Web"
  (let ((jsondata (json-encode '((:action  . "sync")
				 (:version . 6)))))
    (my-anki-connect-push jsondata)))

;;;
(defun my-anki-connect-push (jsondata)
  ""
  (let ((cmd
	 ;; need --noproxy localhost on Windows Environment
	 ;; to communicate AnkiConnect and other environmet is available --noproxy option
	 (format "curl --noproxy localhost localhost:8765 --request POST --data \"%s\""
		 ;; json data '"' を '\"' に変換
		 (replace-regexp-in-string "\"" "\\\\\"" jsondata))))
    (message "%s" cmd)
    (shell-command-to-string cmd)))

;;;
(defun my-anki-encode-string (string)
  "convert string based on AnkiConnect to accept json data with curl command
   AnkiConnect on Windows accept sjis, other is utf-8"
  (if (eq system-type 'windows-nt)
      (encode-coding-string string 'sjis) string))

;;; json-encode
;;; "{\"action\": \"addNote\", \"version\": 6, \"params\": {\"note\": {\"deckName\": \"Default\", \"modelName\": \"Basic\", \"fields\": {\"Front\": \"front content\", \"Back\": \"back content\"}, \"tags\": []}}}"
;;; ドット表記のリストで渡すと,"{\"action\":\"addNote\"}" のようになる
;;; 入れ子にするには、ドット表記のcdr にリスト (:note (:deckName "Default")) のようにする
;;; :action の表記で、json-encode 文字列に変換してくれる
(defun my-anki-connect-make-json-addNotes (deck front back)
  "make json data AnkiConnect can accept"
  (let ((edeck  (my-anki-encode-string deck))
	(efront (my-anki-encode-string front))
	(eback  (my-anki-encode-string back)))
    (json-encode `((:action  . "addNote")
		   (:version . 6)
		   (:params  (:note (:deckName  . ,edeck)
				    (:modelName . "Basic")
				    (:fields    . ((:Front . ,efront)
						   (:Back  . ,eback)))
				    (:tags "")))))))

;;;
;;; fail when back is "発表する" etc
;;; fail (my-anki-connect-push-note ): \224\255\225\\\202\267\202\351\ (extra '\' between 225 and 202 with json-encode)
;;; pass (my-anki-connect-push-notex): \224\255\225\\202\267\202\351\
(defun my-anki-connect-push-note (deck front back)
  "Push note which has front and back in deck specified by json data argment with func use json-encode "
  (let ((cmd
	 ;; need --noproxy localhost on Windows Environment to communicate AnkiConnect and other environmet is available --noproxy option
	 (format "curl --noproxy localhost localhost:8765 --request POST --data \"%s\""
		 ;; json data '"' を '\"' に変換
		 (replace-regexp-in-string "\"" "\\\\\"" (my-anki-connect-make-json-addNotes deck front back)))))
    (message "%s" cmd)
    (shell-command-to-string cmd))
  nil
)

;;; curl --noproxy localhost localhost:8765 -X POST -d "{\"action\": \"addNote\", \"version\": 6, \"params\": {\"note\": {\"deckName\": \"Default\", \"modelName\": \"Basic\", \"fields\": {\"Front\": \"front content\", \"Back\": \"back content\"}, \"tags\": []}}}"
;;; need --noproxy localhost on Windows Environment to communicate anki-connect
;;; option -X is as same as --request
;;; refer https://qiita.com/tadsan/items/17d32514b81f1e8f208a
(defun my-anki-connect-push-notex (deck front back)
  "Push note which has front and back in deck specified by argments with json data argument directly"
  (let ((cmd
	 (format
	  "curl     \
--noproxy localhost \
localhost:8765      \
--request POST      \
--data \"{\\\"action\\\": \\\"addNote\\\", \\\"version\\\": 6, \\\"params\\\": {\\\"note\\\": {\\\"deckName\\\": \\\"%s\\\", \\\"modelName\\\": \\\"Basic\\\", \\\"fields\\\": {\\\"Front\\\": \\\"%s\\\", \\\"Back\\\": \\\"%s\\\"}, \\\"tags\\\": []}}}\""
	  (if (eq system-type 'windows-nt)
	      (encode-coding-string deck  'sjis) deck)
	  (if (eq system-type 'windows-nt)
	      (encode-coding-string front 'sjis) front)
	  (if (eq system-type 'windows-nt)
	      (encode-coding-string back  'sjis) back)
;	  (shell-quote-argument deck) ; this doesn't work because deck 英語 quotes 英\語
;	  (shell-quote-argument front)
;	  (shell-quote-argument back)
	  )))
    (message "%s" cmd)
    (shell-command-to-string cmd)))

;;;
(defun my-anki-connect-register-card (deck)
  ;;; register card in deck specified args
  (let ((front (read-string "Front: "
			    (if (region-active-p)
				(buffer-substring (region-beginning) (region-end))
			      (thing-at-point 'word))))
	(back  (read-string "Back : " )))
    (my-anki-connect-push-note deck front back)))

(defun my-anki-connect-register-card-read-front ()
  (interactive)
  (goto-line (+ 2 1))
  )

(defun my-anki-connect-register-card-read-back ()
  (interactive)
  (goto-line (+ 4 1))
  )

(provide 'my-anki-connect)

;;; my-anki-conn.el ends here
