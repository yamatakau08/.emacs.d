;;;
(cond
 ((eq system-type 'darwin)
  (start-process "Anki" nil "/Applications/Anki.app/Contents/MacOS/Anki")))

(require 'anki-editor) ; to register item to upload the note directly in Anki Web

;;;
(defun my-anki-editor-push-note (deck front back)
  "push note into Anki Web"
  (with-temp-buffer
    (org-mode)         ; need to push the item to Anki Web via anki-editor
    (anki-editor-mode) ; need to push the item to Anki Web via anki-editor
    (insert (format "\n* Item\n  :PROPERTIES:\n  :ANKI_DECK: %s\n  :ANKI_NOTE_TYPE: Basic\n  :END:\n** Front\n   %s\n** Back\n   %s\n" "Default" front back))
    (anki-editor-push-notes)))

;;; 
(defun my-anki-connect-english-register-item (deck from to)
  (interactive
   (let ((from (read-string "From: "))
         (to (read-string "To: "))
	 (deck nil))
     `(,deck ,from ,to)))
  ;; defined in ~/.anki-editor
  (my-anki-editor-push-note "英語" from to)
  )

;;; curl --noproxy localhost localhost:8765 -X POST -d "{\"action\": \"addNote\", \"version\": 6, \"params\": {\"note\": {\"deckName\": \"Default\", \"modelName\": \"Basic\", \"fields\": {\"Front\": \"front content\", \"Back\": \"back content\"}, \"tags\": []}}}"
;;; need --noproxy localhost on Windows Environment to communicate anki-connect
;;; option -X is as same as --request

;;; refer https://qiita.com/tadsan/items/17d32514b81f1e8f208a
;(shell-command-to-string "curl --noproxy localhost localhost:8765 -X POST -d "{\"action\": \"addNote\", \"version\": 6, \"params\": {\"note\": {\"deckName\": \"Default\", \"modelName\": \"Basic\", \"fields\": {\"Front\": \"front content\", \"Back\": \"back content\"}, \"tags\": []}}}")
(defun my-anki-connect-push-note (deck front back)
  ""
  (let ((cmd
	 (format
	  "curl     \
--noproxy localhost \
localhost:8765      \
--request POST      \
--data \"{\\\"action\\\": \\\"addNote\\\", \\\"version\\\": 6, \\\"params\\\": {\\\"note\\\": {\\\"deckName\\\": \\\"%s\\\", \\\"modelName\\\": \\\"Basic\\\", \\\"fields\\\": {\\\"Front\\\": \\\"%s\\\", \\\"Back\\\": \\\"%s\\\"}, \\\"tags\\\": []}}}\"" 
	  deck front back
;	  (shell-quote-argument deck) ; this doesn't work because deck 英語 quotes 英\語
;	  (shell-quote-argument front)
;	  (shell-quote-argument back)
	  )))
    (message "%s" cmd)
    (shell-command-to-string cmd)))

(defun my-anki-connect-version ()
  ""
  (let ((cmd (format "curl --noproxy localhost localhost:8765 -X POST -d \"{\\\"action\\\": \\\"version\\\", \\\"version\\\": 6}\"")))
    (message "%s" cmd)
    (shell-command-to-string cmd)))
