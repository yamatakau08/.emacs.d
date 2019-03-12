;;;
(cond
 ((eq system-type 'darwin)
  (start-process "Anki" nil "/Applications/Anki.app/Contents/MacOS/Anki")))

(require 'anki-editor) ; to register item to upload the note directly in Anki Web

;;;
(defun my-anki-editor-push-note (deck front back)
  "push note into Anki. anki-editor-push-notes works on only Mac, don't work on Windows"
  (cond
   ((eq system-type 'darwin)
    (with-temp-buffer
      (org-mode)         ; need to push the item to Anki Web via anki-editor
      (anki-editor-mode) ; need to push the item to Anki Web via anki-editor
      (insert (format "\n* Item\n  :PROPERTIES:\n  :ANKI_DECK: %s\n  :ANKI_NOTE_TYPE: Basic\n  :END:\n** Front\n   %s\n** Back\n   %s\n" "Default" front back))
      (anki-editor-push-notes)))
   (t (message "%s: anki-editor-push-notes doesnt support!" system-type)
      nil) ; if there is not nil, error "command-execute: Wrong type argument: listp," occurs on call side
   ))
