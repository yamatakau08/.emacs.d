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
;;

;; meme
;; (my-anki-browse-deckNamesAndIds)
;; => ((result (英語 . 1550242731310) (国語 . 1550372251942) (カスタム学習セッション . 1550243694498) (その他 . 1554811643083) (Default . 1)) (error))
;;
;; arg: deck name
;;(my-anki-browse-findNotes "その他")
;; => [1554811649049 1565823907156]
;;
;; 何故かよく分からないが、notesInfo に、noteid を渡すと、その noteid の情報が得られる
;; card だと思うのだが...
;; (my-anki-browse-notesInfo 1554811649049)
;; => ((result . [((noteId . 1554811649049) (tags . []) (fields (Front (value . ディアスポラ) (order . 0)) (Back (value . ギリシャ語 離散の意味<br>パレスチナ以外に移り住んだユダヤ人とその社会を指す<br>今は、故郷や祖先の地を離れ暮らす人やコミュニティを指す) (order . 1))) (modelName . Basic) (cards . [1554811848515]))]) (error))

;;; Code:
(require 'request)

;; for debug
(setq request-log-level     'debug)
(setq request-message-level 'debug)

;;
(defvar my-anki-browse-anki-connect-url "localhost:8765")
(defvar my-anki-browse-anki-connect-version nil)
(defvar my-anki-browse-anki-deck-names nil)

;;
(defun my-anki-browse-deck-cards (deck)
  ;; https://stackoverflow.com/questions/2382524/adding-completion-to-interactive/2382677#2382677
  ;; to list up the completion with interactive
  (interactive
   (list
    (completing-read "Deck: " (my-anki-browse-deckNames))))
  (let (noteids)
    (setq noteids (my-anki-browse-findNotes deck))
    (my-anki-browse-notesInfo noteids)))

(defun my-anki-browse-notesInfo (noteids)
  "Returns a list of objects containing for each note ID the note fields, tags, note type and the cards belonging to the note.
https://github.com/FooSoft/anki-connect/blob/master/actions/notes.md
noteids: array [1,2]"
  (interactive "nnoteIDs: ")
  (my-anki-browse-request
   :data (json-encode
	  `((:action  . "notesInfo")
	    (:version . ,my-anki-browse-anki-connect-version)
	    (:params  (:notes . ,noteids))))))

(defun my-anki-browse-findNotes (deck)
  "Returns an array of note IDs for a given query. Same query syntax as guiBrowse.
https://github.com/FooSoft/anki-connect/blob/master/actions/notes.md"
  (interactive "sDeck: ")
  (my-anki-browse-request
   :data (json-encode
	  `((:action  . "findNotes")
	    (:version . ,my-anki-browse-anki-connect-version)
	    ;;(:params  (:query . "deck:current")))) ; sample
	    (:params  (:query . ,(format "deck:%s" deck))))))) ; "," before "(format" is necessary!

(defun my-anki-browse-deckNames ()
  "Gets the complete list of deck names for the current user.
https://github.com/FooSoft/anki-connect/blob/master/actions/decks.md#deck-actions"
  (interactive)
  (let (deck-names)
    (setq deck-names
	  (my-anki-browse-request
	   :type "POST"
	   :data (json-encode
		  `(("action"  . "deckNames")
		    ("version" . ,my-anki-browse-anki-connect-version)))))
    ;; coerce http://dminor11th.blogspot.com/2012/06/coerce.html?m=1
    ;; to convert array to list to utilize the deck names
    (setq my-anki-browse-anki-deck-names (coerce deck-names 'list))))

(defun my-anki-browse-deckNamesAndIds ()
  "Gets the complete list of deck names and their respective IDs for the current user.
https://github.com/FooSoft/anki-connect/blob/master/actions/decks.md#deck-actions"
  (interactive)
  (my-anki-browse-request
   :type "POST"
   :data (json-encode
	  `(("action"  . "deckNamesAndIds")
   	    ("version" . ,my-anki-browse-anki-connect-version)))))

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
           ;;(:version . 6))) ;; without version, anki-connect returns version 6
    :parser 'json-read ; parse-error occurs without json-encode at ":data" part
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(setq my-anki-browse-anki-connect-version data)
		(message "[my-anki-browse] -version: %s" data)))
    :error (cl-function
	    (lambda (&rest args &key error-thrown &allow-other-keys)
	      (message "[my-anki-browse] -version error: %s" error-thrown)))))

;; private
(defun my-anki-browse-request (&rest args)
  (unless my-anki-browse-anki-connect-version
    (my-anki-browse-version))

  (message "[my-anki-browse][debug] -request args: %s" args)

  (if my-anki-browse-anki-connect-version
      (let (response
	    data
	    result
	    error)
	(setq response
	      (apply #'request
		     my-anki-browse-anki-connect-url
		     :sync t
		     :headers '(("Content-Type" . "application/json"))
		     :parser  'json-read ;'json-read ; note that numerice has decimal point
		     :success (cl-function
			       (lambda (&key data &allow-other-keys)
				 (message "[my-anki-browse][debug] -request: %s" data)))
		     args)) ; args are ":type",":data" ... , and should be the last
	(setq data (request-response-data response))
	(setq result (let-alist data .result))
	(setq error  (let-alist data .error))
	(if error
	    (message "[my-anki-browse] -request error: %s" error))
	(message "[my-anki-browse] -request result: %s" result)
	result) ; return the data
    (message "[my-anki-browse] get-version error!")
    (message "[my-anki-browse] anki doesn't seem to be launched!")))

(provide 'my-anki-browse)
;;; my-anki-browse.el ends here