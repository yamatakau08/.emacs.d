;;; my-anki-browse.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Takahiro Yamaguchi

;; Author: Takahiro Yamaguchi <yamatakau08@gmail.com>
;; Keywords: browse anki cards

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
;; anki structure
;; anki - deck - notes
;; note has fields Front Back if note type is "Basic"
;; note has note types "Basic","Basic (and reserved card)","Basic (optional reversed card)","Cloze"
;; smart function my-anki-browse--debug-message to add prefixe "[debug]"

;; Todo
;; use request get field
;; chage my-anki-browse-version to use my-anki-browse--request, not to use request directly
;; move each (:version . my-anki-browse--anki-coonect-version to my-anki-browse-reqeust ?
;; On Windows, my-anki-browser-alivep use tasklist /| grep -i anki

;;; Code:
(require 'request)

;; for debug
;(setq request-log-level     'debug)
;(setq request-message-level 'debug)

(defgroup my-ank-browse nil
  "My anki browse through anki connect"
  :group 'tools)

(defcustom my-anki-browse-debug nil
  "if t, put out the debug message of this tool"
  :group 'my-anki-browse
  :type  'bool)

(defcustom my-anki-browse-anki-main-deck-name nil
  "anki deck name which you mainly use"
  :group 'my-anki-browse
  :type  'string)

;; public function
(defalias #'my-anki-browse-anki-alivep #'my-anki-browse-version)

(defun my-anki-browse-deck-notes (deck)
  "return every notes info e.g. noteid,front,back... specified deck"
  ;; completing-read
  ;; https://stackoverflow.com/questions/2382524/adding-completion-to-interactive/2382677#2382677
  ;; to list up the completion with interactive
  (interactive
   (list
    (let ((completion-cycle-threshold t))
      (if (member my-anki-browse-anki-main-deck-name (my-anki-browse-deckNames))
	  (completing-read "Deck: " (my-anki-browse-deckNames) nil t my-anki-browse-anki-main-deck-name)
	(completing-read "Deck: " (my-anki-browse-deckNames))))))
  (let (noteids)
    (setq my-anki-browse--current-deck deck)
    (setq noteids (my-anki-browse-findNotes deck))
    (my-anki-browse-notesInfo noteids)))

(defun my-anki-browse-current-deck ()
  "return current deck"
  my-anki-browse--current-deck)

(defun my-anki-browse-notesInfo (noteids)
  "Returns a list of objects containing for each note ID the note fields, tags, note type and the cards belonging to the note.
https://github.com/FooSoft/anki-connect/blob/master/actions/notes.md
e.g. noteids: array [1,2]
1,2 noteid"
  ;; (interactive "nnoteIDs: ")
  (my-anki-browse--anki-connect-request
   :data (json-encode
	  `((:action  . "notesInfo")
	    (:version . ,my-anki-browse--anki-connect-version)
	    (:params  (:notes . ,noteids))))))

(defun my-anki-browse-findNotes (deck)
  "Returns an array of note IDs for a given query. Same query syntax as guiBrowse.
https://github.com/FooSoft/anki-connect/blob/master/actions/notes.md"
  (my-anki-browse--anki-connect-request
   :data (json-encode
	  `((:action  . "findNotes")
	    (:version . ,my-anki-browse--anki-connect-version)
	    (:params  (:query . ,(format "deck:%s" deck))))))) ; "," before "(format" is necessary!

(defun my-anki-browse-deckNames ()
  "Gets the complete list of deck names for the current user.
https://github.com/FooSoft/anki-connect/blob/master/actions/decks.md#deck-actions"
  (interactive)
  (let ((deckNames (my-anki-browse--anki-connect-request
		    :type "POST"
		    :data (json-encode
			   `((:action  . "deckNames")
			     (:version . ,my-anki-browse--anki-connect-version))))))
    ;; coerce http://dminor11th.blogspot.com/2012/06/coerce.html?m=1
    ;; to convert array to list to utilize the deckNames
    (coerce deckNames 'list)))

(defun my-anki-browse-deckNamesAndIds ()
  "Gets the complete list of deck names and their respective IDs for the current user.
https://github.com/FooSoft/anki-connect/blob/master/actions/decks.md#deck-actions"
  (my-anki-browse--anki-connect-request
   :type "POST"
   :data (json-encode
	  `((:action  . "deckNamesAndIds")
   	    (:version . ,my-anki-browse--anki-connect-version)))))

(defun my-anki-browse-addNote (deckName Front Back)
  "Creates a note using the given deck and model, with the provided field values and tags.
Returns the identifier of the created note created on success, and null on failure.
https://github.com/FooSoft/anki-connect/blob/master/actions/notes.md"
  (interactive
   (list
    (let ((completion-cycle-threshold t))
      (if (member my-anki-browse-anki-main-deck-name (my-anki-browse-deckNames))
	  (completing-read "deckName: " (my-anki-browse-deckNames) nil t my-anki-browse-anki-main-deck-name)
	(completing-read "deckName: " (my-anki-browse-deckNames))))
    (read-string "Front: ")
    (read-string "Back : ")))

  (let ((modelName "Basic")) ;; modelName fixed "Basic"
    (my-anki-browse--anki-connect-request
     :type "POST"
     :data (json-encode
	    `((:action  . "addNote")
	      (:version . my-anki-browse--anki-connect-version)
	      (:params (:note . (:deckName ,deckName :modelName ,modelName :fields (:Front ,Front :Back ,Back)))))))))

(defun my-anki-browse-updateNoteFields (noteid Front Back)
  "Modify the fields of an exist note.
You can also include audio files which will be added to the note with an optional audio property.
Please see the documentation for addNote for an explanation of objects in the audio array.
Gets the complete list of deck names and their respective IDs for the current user.
https://github.com/FooSoft/anki-connect/blob/master/actions/notes.md"
  (my-anki-browse--anki-connect-request
   :type "POST"
   :data (json-encode
	  `((:action  . "updateNoteFields")
	    (:version . my-anki-browse--anki-connect-version)
	    (:params (:note . (:id  ,noteid :fields (:Front ,Front :Back ,Back))))))))

(defun my-anki-browse-deleteNotes (noteids)
  "Deletes notes with the given ids.
If a note has several cards associated with it, all associated cards will be deleted.
https://github.com/FooSoft/anki-connect/blob/master/actions/notes.md"
  (my-anki-browse--anki-connect-request
   :type "POST"
   :data (json-encode
	  `((:action  . "deleteNotes")
	    (:version . my-anki-browse--anki-connect-version)
	    (:params  (:notes . ,noteids))))))

(defun my-anki-browse-sync ()
  "Synchronizes the local Anki collections with AnkiWeb.
https://github.com/FooSoft/anki-connect/blob/master/actions/miscellaneous.md"
  (my-anki-browse--anki-connect-request
   :type "POST"
   :data (json-encode
	  `((:action  . "sync")
	    (:version . my-anki-browse--anki-connect-version)))))

(defun my-anki-browse-version ()
  "Gets the version of the API exposed by this plugin. Currently versions 1 through 6 are defined.
https://github.com/FooSoft/anki-connect/blob/master/actions/miscellaneous.md"
  (interactive)
  (request
    my-anki-browse--anki-connect-url
    :sync t
    :type "POST"
    :headers '(("Content-Type" . "application/json"))
    :data (json-encode
	   `((:action  . "version")))
           ;;(:version . 6))) ;; without version, anki-connect returns version 6
    :parser 'json-read ; parse-error occurs without json-encode at ":data" part
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(setq my-anki-browse--anki-connect-version data)
		(my-anki-browse--debug-message "[my-anki-browse] -version: %s" data)))
    :error (cl-function
	    (lambda (&rest args &key error-thrown &allow-other-keys)
	      (setq my-anki-browse--anki-connect-version nil)
	      (error "[my-anki-browse] anki is not launched!"))))
  my-anki-browse--anki-connect-version)

;; private
;;
(defvar my-anki-browse--anki-connect-url "localhost:8765")
(defvar my-anki-browse--anki-connect-version nil)
(defvar my-anki-browse--current-deck nil)

(defun my-anki-browse--anki-connect-request (&rest args)
  "private common request api to anki connect"

  (unless my-anki-browse--anki-connect-version
    (my-anki-browse-version))

  (if my-anki-browse--anki-connect-version
      (let (response
	    data
	    result
	    error)
	(setq response
	      (apply #'request
		     my-anki-browse--anki-connect-url
		     :sync t
		     :headers '(("Content-Type" . "application/json"))
		     :parser  'json-read ;'json-read ; note that numerice has decimal point
		     :success (cl-function
			       (lambda (&key data &allow-other-keys)
				 (my-anki-browse--debug-message "[my-anki-browse][debug] -request: %s" data)))
		     args)) ; args are ":type",":data" ... , and should be the last
	(setq data (request-response-data response))
	(setq result (let-alist data .result))
	(setq error  (let-alist data .error))
	(if error
	    (cond ((equal error "'<=' not supported between instances of 'str' and 'int'") nil) ; When update note, have this error but succeed to update on emacs of mingw32.exe
		  ;;((equal error "semaphore never called"                                 ) nil) ; somtimes have this error on the same envorionment
		  (t (message "[my-anki-browse] -request error: %s" error))))
	(my-anki-browse--debug-message "[my-anki-browse][debug] -request result: %s" result)
	result) ; return the data
    (message "[my-anki-browse] get-version error!")
    (message "[my-anki-browse] anki doesn't seem to be launched!")))

(defun my-anki-browse--debug-message (format &rest args)
  "for debug message"
  (if my-anki-browse-debug
      (message format (car args))))

(provide 'my-anki-browse)
;;; my-anki-browse.el ends here
