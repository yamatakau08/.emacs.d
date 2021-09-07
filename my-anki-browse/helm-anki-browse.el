;;; helm-anki-browse.el

;; Copyright (C) 2020

;; Author: Takahiro Yamaguchi
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; reference
;; utilize helm from my own program
;; https://writequit.org/denver-emacs/presentations/2016-03-01-helm.html#orgheadline24
;; http://wikemacs.org/wiki/How_to_write_helm_extensions#A_list_of_candidates_and_an_action

;; Todo
;; error message when execute "helm-resume" ad-Advice-helm-resume: Symbol’s value as variable is void: helm-marked-buffer-name
;; in helm-anki-browser buffer, how to sort candidates
;; => https://abicky.net/2014/01/04/170448/ helm-ff-sort-candidates
;; in helm buffer, C-c C-e helm-anki-browse--updateNoteFields error
;; after executing helm-anki-browse--updateNoteFields-commit, return helm-anki-browse buffer then update the list
;; edit note buffer, "Front" and "Back" field edit is prohibited after erasing the field content
;; When anki-is not launched, helm-ank-browse error handling
;; support to change deck of note in helm-buffer
;; If there is nothing candidate after increment search, f2 has error.
;; - helmi mini beffer, input a part of strig, then F2, have curios behavior.
;; on Mac, helm-anki-browse buffer indent is not aligned

;; Memo
;; msys2 mingw32.exe "my-anki-browse-cardsInfo" doesn't work correctly!, on the other hand mingw64.exe works
;; - (my-anki-browse-cardsInfo [1594024606584])
;;   - => nil               ; mingw32
;;     => [((cardId . ....] ; mingw64

;;; Code:
(require 'my-anki-browse)

;; public
(defun helm-anki-browse (&optional deck)
  "helm anki browse"
  (interactive)
  (if (my-anki-browse-anki-alivep)
      (let (candidates)
	(if (string= system-configuration "i686-w64-mingw32") ; see memo on the abovle
	    (setq candidates (helm-anki-browse--xcandidates deck))
	  (setq candidates (helm-anki-browse--candidates deck)))

	(if candidates
	    (helm :sources (helm-build-sync-source "Anki notes"
			     :candidates candidates
			     :action '(("Update"      . (lambda (candidate)
							  (helm-anki-browse--updateNoteFields candidate)))
				       ("Create"      . (lambda (candidate)
							  (helm-anki-browse--addNote candidate)))
				       ("Delete"      . (lambda (candidate)
							  (helm-anki-browse--deleteNote candidate)))
				       ("Go to Deck"  . (lambda (candidate)
							  (helm-anki-browse--gotoDeck candidate)))
				       ("Change Deck" . (lambda (candidate)
							  (helm-anki-browse--changeDeck candidate))))
			     :candidate-number-limit 9999
			     ;; :header-line "Created" ; can't effect, should be alist, but in case of using alist, (migemo . t) is not effect!
			     :persistent-help "View note"
			     ;;:keymap helm-anki-browse--keymap
			     :migemo t)
		  :buffer helm-anki-browse--buffer-name)
	  (message "[helm-anki-browse] no candidate")))))

;; private
(defvar helm-anki-browse--buffer-name                  "*helm-anki-browser*")
(defvar helm-anki-browse--updateNoteFields-buffer-name "*helm-anki-browse-updateNoteFields*")
(defvar helm-anki-browse--addNote-buffer-name          "*helm-anki-browse-addNote*")

(defvar helm-anki-browse--keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c C-e") 'helm-anki-browse--updateNoteFields)
    (define-key map (kbd "C-c C-c") 'helm-anki-browse--addNote)
    map)
  "Keymap for `helm-anki-browse'.")

(defvar helm-anki-browse--updateNoteFields-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'helm-anki-browse--updateNoteFields-commit)
    (define-key map (kbd "C-c C-d") 'helm-anki-browse--updateNoteFields-delete)
    (define-key map (kbd "C-c C-k") 'helm-anki-browse--updateNoteFields-abort)
    map))

(defalias #'helm-anki-browse--addNote-abort #'helm-anki-browse--updateNoteFields-abort)

(defvar helm-anki-browse--addNote-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'helm-anki-browse--addNote-commit)
    (define-key map (kbd "C-c C-k") 'helm-anki-browse--addNote-abort)
    map))

(defun helm-anki-browse--xcandidates (deck)
  "for mingw32, it doesn't support changeDeck function, because return nil"
  (let (candidates
	noteId
	Frontvalue
	Backvalue
	FrontBackValue
	notes
	note)

    (if deck
	(setq notes (my-anki-browse-deck-notes deck))
      (setq notes (call-interactively 'my-anki-browse-deck-notes)))

    (dotimes (i (length notes))
      (setq note (aref notes i))
      (setq noteId (let-alist note .noteId))
      (setq Frontvalue (let-alist note .fields.Front.value))
      (setq Backvalue  (let-alist note .fields.Back.value))
      (setq FrontBackvalue  (format "%-30s: %s" (truncate-string-to-width Frontvalue 30) (truncate-string-to-width Backvalue 50)))
      (add-to-list 'candidates `(,FrontBackvalue . (:deckName ,(my-anki-browse-current-deck) :noteId ,noteId :Front ,Frontvalue :Back ,Backvalue)) t))
    candidates))

(defun helm-anki-browse--candidates (deck)
  (let (candidates
	noteId
	Frontvalue
	Backvalue
	FrontBackValue
	cards
	card
	cardId)

    (if deck
	(setq cards (my-anki-browse-cardsInfo (my-anki-browse-findCards deck)))
      (setq cards (my-anki-browse-cardsInfo (call-interactively 'my-anki-browse-findCards))))

    (dotimes (i (length cards))
      (setq card (aref cards i))
      (setq cardId (let-alist card .cardId))
      (setq noteId (let-alist card .note))
      (setq Frontvalue (let-alist card .fields.Front.value))
      (setq Backvalue  (let-alist card .fields.Back.value))
      ;; FrontBackvalue is for search result buffer helm-buffer.
      (setq FrontBackvalue  (format "%-30s: %s" (truncate-string-to-width Frontvalue 30) (truncate-string-to-width Backvalue 50)))
      (add-to-list 'candidates `(,FrontBackvalue . (:deckName ,(my-anki-browse-current-deck) :noteId ,noteId :cardId ,cardId :Front ,Frontvalue :Back ,Backvalue)) t))
    candidates))

(defsubst helm-anki-browse--func-to-keys (func map)
  (key-description (car-safe (where-is-internal func map))))

;; update note
(defun helm-anki-browse--updateNoteFields (candidate)
  (let ((buffer-name helm-anki-browse--updateNoteFields-buffer-name))
    (with-current-buffer (get-buffer-create buffer-name)
      (setq header-line-format
	    (format "%s: Commit, %s: Abort, %s: Delete"
		    (helm-anki-browse--func-to-keys #'helm-anki-browse--updateNoteFields-commit helm-anki-browse--updateNoteFields-map)
		    (helm-anki-browse--func-to-keys #'helm-anki-browse--updateNoteFields-abort  helm-anki-browse--updateNoteFields-map)
		    (helm-anki-browse--func-to-keys #'helm-anki-browse--updateNoteFields-delete helm-anki-browse--updateNoteFields-map)))
      ;; https://meech.hatenadiary.org/entry/20100414/1271197161
      ;; refer "read-only と sticky"
      ;; but the followin is incomplete.
      (let ((inhibit-read-only t))
	(erase-buffer)
	(insert (propertize (format "noteId: %s\n" (plist-get candidate :noteId)) 'read-only t)
		(propertize (format "cardId: %s\n" (plist-get candidate :cardId)) 'read-only t)
		(propertize "Front : " 'read-only t 'rear-nosticky t 'front-sticky t)
		(format "%s" (plist-get candidate :Front))
		(propertize "\n" 'read-only t 'rear-nonsticky t)
		(propertize "Back  : " 'read-only t 'rear-nosticky t 'front-sticky t)
		(format "%s" (plist-get candidate :Back))))
      (switch-to-buffer (get-buffer buffer-name))
      (use-local-map helm-anki-browse--updateNoteFields-map))))

(defun helm-anki-browse--updateNoteFields-commit ()
  (interactive)
  (let ((noteid (string-to-number (helm-anki-browse--updateNoteFields-get-field "noteId")))
	(front  (helm-anki-browse--updateNoteFields-get-field "Front"))
	(back   (helm-anki-browse--updateNoteFields-get-field "Back")))
    (my-anki-browse-updateNoteFields noteid front back)
    (helm-anki-browse--updateNoteFields-exit)
    (helm-anki-browse (my-anki-browse-current-deck))))

(defun helm-anki-browse--updateNoteFields-abort ()
  (interactive)
  (when (y-or-n-p "Discard changes ?")
    (message "Abort edit"))
  (helm-anki-browse--updateNoteFields-exit))

(defun helm-anki-browse--updateNoteFields-delete ()
  (interactive)
  (let ((noteid (helm-anki-browse--updateNoteFields-get-field "noteId")))
    (my-anki-browse-deleteNotes `(,noteid))
    (helm-anki-browse--updateNoteFields-exit)))

(defun helm-anki-browse--updateNoteFields-exit ()
  (kill-buffer (get-buffer helm-anki-browse--updateNoteFields-buffer-name))
  ;; to reflect the update, execute helm-anki-browse again
  (helm-anki-browse (my-anki-browse-current-deck)))

(defun helm-anki-browse--updateNoteFields-get-field (field)
  (let (start end)
    ;;(beginning-of-buffer) ; do not use in Lisp program, read help
    (goto-char (point-min))
    (setq start (re-search-forward (format "^%s *: *" field)))
    (setq end   (point-at-eol))
    (buffer-substring-no-properties start end)))

;; add note
(defun helm-anki-browse--addNote (candidate)
  (with-current-buffer (get-buffer-create helm-anki-browse--addNote-buffer-name)
    (setq header-line-format
	  (format "%s: Add, %s: Abort"
		  (helm-anki-browse--func-to-keys #'helm-anki-browse--addNote-commit helm-anki-browse--addNote-map)
		  (helm-anki-browse--func-to-keys #'helm-anki-browse--addNote-abort  helm-anki-browse--addNote-map)))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "deck  : %s\n" (plist-get candidate :deckName))
	      "Front : \n"
	      "Back  : ")
      (goto-char 20) ; move point to Front field
      (switch-to-buffer (get-buffer helm-anki-browse--addNote-buffer-name))
      (use-local-map helm-anki-browse--addNote-map))))

(defun helm-anki-browse--addNote-commit ()
  (interactive)
  (let ((deck  (helm-anki-browse--updateNoteFields-get-field "Deck"))
	(front (helm-anki-browse--updateNoteFields-get-field "Front"))
	(back  (helm-anki-browse--updateNoteFields-get-field "Back")))
    (my-anki-browse-addNote deck front back)
    (kill-buffer (get-buffer helm-anki-browse--addNote-buffer-name))
    ;; to reflect the adding note, execute helm-anki-browse again
    (helm-anki-browse (my-anki-browse-current-deck))))

;; delete note
(defun helm-anki-browse--deleteNote (candidate)
  (my-anki-browse-deleteNotes `(,(plist-get candidate :noteId)))
  ;; to reflect the deleting note, execute helm-anki-browse again
  (helm-anki-browse (my-anki-browse-current-deck)))

;; change Deck
(defun helm-anki-browse--changeDeck (candidate)
  (let ((cardId (plist-get candidate :cardId))
	deck)
    (let ((completion-cycle-threshold t))
      (setq deck (completing-read "Deck: " (my-anki-browse-deckNames))))
    (my-anki-browse-changeDeck deck `[,cardId])
    (message "[helm-anki-browse]--changeDeck: %s" deck)
    (helm-anki-browse (my-anki-browse-current-deck))))

;; change Deck
(defun helm-anki-browse--gotoDeck (candidate)
  (let ((completion-cycle-threshold t)
	deck)
    (setq deck (completing-read "Deck: " (my-anki-browse-deckNames)))
    (helm-anki-browse deck)))

(provide 'helm-anki-browse)
;;; helm-anki-browse.el ends here
