;;;
;;; functions for anki
;;;

;;; for adding note in Anki
;(add-to-list 'load-path "~/.emacs.d/my-anki-connect")
(require 'my-anki-connect "~/.emacs.d/my-anki-connect/my-anki-connect.el")

;;;
(defun my-anki-add-note-english ()
  "add note in Anki"
  (interactive)
  (let ((deck "英語")
	(front (read-string "Front: "))
	(back  (read-string "Back : ")))
    (my-anki-connect-push-notex deck front back)))

(global-set-key "\C-cr" 'my-anki-add-note-english)
