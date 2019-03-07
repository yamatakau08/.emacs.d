;;; 
(defun my-anki-connect-version ()
  ""
  (let ((cmd (format "curl --noproxy localhost localhost:8765 -X POST -d \"{\\\"action\\\": \\\"version\\\", \\\"version\\\": 6}\"")))
    (message "%s" cmd)
    (shell-command-to-string cmd)))

;;;
(defun my-anki-connect-english-register-item (deck from to)
  (interactive
   (let ((from (read-string "From: "))
         (to (read-string "To: "))
	 (deck nil))
     `(,deck ,from ,to)))
  ;; defined in ~/.anki-editor
  (my-anki-connect-push-note "英語" from to))

;;; json-encode
;;; "{\"action\": \"addNote\", \"version\": 6, \"params\": {\"note\": {\"deckName\": \"Default\", \"modelName\": \"Basic\", \"fields\": {\"Front\": \"front content\", \"Back\": \"back content\"}, \"tags\": []}}}"
;;; ドット表記のリストで渡すと,"{\"action\":\"addNote\"}" のようになる
;;; 入れ子にするには、ドット表記のcdr にリスト (:note (:deckName "Default")) のようにする
;;; :action の表記で、json-encode 文字列に変換してくれる
(defun my-anki-connect-make-json (deck front back)
  (let ((edeck  (if (eq system-type 'windows-nt)
		    (encode-coding-string deck  'sjis) deck))
	(efront (if (eq system-type 'windows-nt)
		    (encode-coding-string front 'sjis) front))
	(eback  (if (eq system-type 'windows-nt)
		    (encode-coding-string back  'sjis) back)))
    (json-encode `((:action  . "addNote")
		   (:version . 6)
		   (:params  (:note (:deckName  . ,edeck)
				    (:modelName . "Basic")
				    (:fields    . ((:Front . ,efront)
						   (:Back  . ,eback)))
				    (:tags "")))))))

(defun my-anki-connect-push-note (deck front back)
  (let ((cmd 
	 (format "curl --noproxy localhost localhost:8765 --request POST --data \"%s\""
		 ;; json data '"' を '\"' に変換
		 (replace-regexp-in-string "\"" "\\\\\"" (my-anki-connect-make-json deck front back)))))
    (message "%s" cmd)
    (shell-command-to-string cmd)))

;;; curl --noproxy localhost localhost:8765 -X POST -d "{\"action\": \"addNote\", \"version\": 6, \"params\": {\"note\": {\"deckName\": \"Default\", \"modelName\": \"Basic\", \"fields\": {\"Front\": \"front content\", \"Back\": \"back content\"}, \"tags\": []}}}"
;;; need --noproxy localhost on Windows Environment to communicate anki-connect
;;; option -X is as same as --request
;;; refer https://qiita.com/tadsan/items/17d32514b81f1e8f208a
(defun my-anki-connect-push-notex (deck front back)
  ""
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
;    (message "%s" cmd)
    (shell-command-to-string cmd)))
