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

;; Todo
;; my-anki-browse-anki-version
;; [my-anki-browse] -deckNamesAndIds: ((result (英語 . 1550242731310) (国語 . 1550372251942) (カスタム学習セッション . 1550243694498) (その他 . 1554811643083) (Default . 1)) (error))
;;(my-anki-browse-findNotes "その他")
;;[1554811649049 1565823907156]
;;
;;(defun my-anki-browse-cardsInfo (cardids)
;;  (json-encode
;;   `((:action  . "cardsInfo")
;;     (:version . ,my-anki-browse-anki-version)
;;     (:params  (:cards ,cardids)))))
;;
;;(my-anki-browse-cardsInfo (my-anki-browse-findNotes "その他"))
;;"{\"action\":\"cardsInfo\",\"version\":6,\"params\":{\"cards\":[[1554811649049,1565823907156]]}}"

;;; Code:
(require 'request)

;; for debug
;(setq request-log-level     'debug)
;(setq request-message-level 'debug)

;;
(defvar my-anki-browse-anki-connect-url "localhost:8765")
(defvar my-anki-browse-anki-version 6)

(defun my-anki-browse-cardsInfo (cardids)
  "Returns a list of objects containing for each card ID the card fields,
front and back sides including CSS, note type,
the note that the card belongs to, and deck name, as well as ease and interval.
https://github.com/FooSoft/anki-connect/blob/master/actions/cards.md"
  (interactive "ncardIDs: ")
  (request
    my-anki-browse-anki-connect-url
    :sync t
    :type "POST"
    :headers '(("Content-Type" . "application/json"))
    :data (json-encode
	   `((:action  . "cardsInfo")
	     (:version . ,my-anki-browse-anki-version)
	     (:params  (:cards ,cardids))))
    :parser 'json-read ; parse-error occurs without json-encode at ":data" part
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(message "[my-anki-browse] -cardsInfo: %s" data)))))

(defun my-anki-browse-notesInfo (noteids)
  "Returns a list of objects containing for each note ID the note fields, tags, note type and the cards belonging to the note.
https://github.com/FooSoft/anki-connect/blob/master/actions/notes.md"
  (interactive "nnoteIDs: ")
  (request
    my-anki-browse-anki-connect-url
    :sync t
    :type "POST"
    :headers '(("Content-Type" . "application/json"))
    :data (json-encode
	   `((:action  . "notesInfo")
	     (:version . ,my-anki-browse-anki-version)
	     (:params  (:notes ,noteids))))
    :parser 'json-read ; parse-error occurs without json-encode at ":data" part
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(message "[my-anki-browse] -notesInfo: %s" data)))))

(defun my-anki-browse-findNotes (deck)
  "Returns an array of note IDs for a given query. Same query syntax as guiBrowse.
[my-anki-browse] -findNotes: ((result . [1559778375256 1580531127255 1581461417451 1582239433730]) (error))
https://github.com/FooSoft/anki-connect/blob/master/actions/notes.md"
  (interactive "sDeck: ")
  (let (cardids)
    (request
      my-anki-browse-anki-connect-url
      :sync t
      :type "POST"
      :headers '(("Content-Type" . "application/json"))
      :data (json-encode
	     `((:action  . "findNotes")
	       (:version . ,my-anki-browse-anki-version)
	       ;;(:params  (:query . "deck:current")))) ; sample
	       (:params  (:query . ,(format "deck:%s" deck))))) ; "," before "(format" is necessary!
      :parser 'json-read ; parse-error occurs without json-encode at ":data" part
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (message "[my-anki-browse] -findNotes: %s" data) ; for debug
		  (setq cardids (let-alist data .result)))))
    cardids))

(defun my-anki-browse-deckNamesAndIds ()
  "Gets the complete list of deck names and their respective IDs for the current user.
https://github.com/FooSoft/anki-connect/blob/master/actions/decks.md#deck-actions"
  (interactive)
  (request
    my-anki-browse-anki-connect-url
    :sync t
    :type "POST"
    :headers '(("Content-Type" . "application/json"))
    :data (json-encode
	   `(("action"  . "deckNamesAndIds")
	     ("version" . ,my-anki-browse-anki-version)))
    :parser 'json-read ; parse-error occurs without json-encode at ":data" part
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(message "[my-anki-browse] -deckNamesAndIds: %s" data)))))

(defun my-anki-browse-version ()
  "Gets the version of the API exposed by this plugin. Currently versions 1 through 6 are defined.
https://github.com/FooSoft/anki-connect/blob/master/actions/miscellaneous.md"
  (interactive)
  (request
    my-anki-browse-anki-connect-url
    :sync t
    :type "POST"
    :headers '(("Content-Type" . "application/json"))
    :data (json-encode
	   `((:action  . "version")))
	     ;;(:version . 6))) ;; without version, response return version 6
    :parser 'json-read ; parse-error occurs without json-encode at ":data" part
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(message "[my-anki-browse] -version: %s" data)))))

(provide 'my-anki-browse)
;;; my-anki-browse.el ends here
