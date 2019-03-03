;;;
(start-process "Anki" nil "/Applications/Anki.app/Contents/MacOS/Anki")

(require 'anki-editor) ; to register item to upload the note directly in Anki Web

;;;
(defun my-anki-editor-push-note (deck front back)
  "push note into Anki Web"
  (with-temp-buffer
    (org-mode)         ; need to push the item to Anki Web via anki-editor
    (anki-editor-mode) ; need to push the item to Anki Web via anki-editor
    (insert (format "\n* Item\n  :PROPERTIES:\n  :ANKI_DECK: %s\n  :ANKI_NOTE_TYPE: Basic\n  :END:\n** Front\n   %s\n** Back\n   %s\n" deck front back))
    (anki-editor-push-notes)))
