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
;; in helm-anki-browser buffer, how to sort candidates
;; => https://abicky.net/2014/01/04/170448/ helm-ff-sort-candidates
;; in helm buffer, C-c C-e helm-anki-browse--edit-card error
;; after executing helm-anki-browse--edit-card-commit, return helm-anki-browse buffer then update the list
;; edit card buffer, "Front" and "Back" field edit is prohibited after erasing the field content
;; When anki-is not launched, helm-ank-browser error handling

;; memo
;;(let-alist (car (coerce (my-anki-browse-deck-cards "その他") `list)) .noteId)
;; => 1554811649049
;;(let-alist (car (coerce (my-anki-browse-deck-cards "その他") `list)) .fields.Front.value)
;; => "ディアスポラ"
;;(let-alist (car (coerce (my-anki-browse-deck-cards "その他") `list)) .fields.Back.value)
;; => "ギリシャ語 離散の意味<br>パレスチナ以外に移り住んだユダヤ人とその社会を指す<br>今は、故郷や祖先の地を離れ暮らす人やコミュニティを指す"

;;(length (my-anki-browse-deck-cards "その他"))
;;(aref (my-anki-browse-deck-cards "その他") 0)
;;=> ((noteId . 1554811649049) (tags . []) (fields (Front (value . "ディアスポラ") (order . 0)) (Back (value . "ギリシャ語 離散の意味<br>パレスチナ以外に移り住んだユダヤ人とその社会を指す<br>今は、故郷や祖先の地を離れ暮らす人やコミュニティを指す") (order . 1))) (modelName . "Basic") (cards . [1554811848515]))
;;
;;(aref (my-anki-browse-deck-cards "その他") 1)
;;=> ((noteId . 1565823907156) (tags . []) (fields (Front (value . "盧溝橋 日本と中国 軍司衝突") (order . 0)) (Back (value . "") (order . 1))) (modelName . "Basic") (cards . [1565824058849]))

;;; Code:
(require 'my-anki-browse)

(defvar helm-anki-browse-buffer-name             "*helm-anki-browser*")
(defvar helm-anki-browse-edit-card-buffer-name   "*helm-anki-browse-edit-card*")
(defvar helm-anki-browse-create-card-buffer-name "*helm-anki-browse-create-card*")

(defvar helm-anki-browse-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c C-e") 'helm-anki-browse--edit-card)
    (define-key map (kbd "C-c C-c") 'helm-anki-browse--create-card)
    map)
  "Keymap for `helm-anki-browse'.")

(defvar helm-anki-browse-edit-card-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'helm-anki-browse--edit-card-commit)
    (define-key map (kbd "C-c C-k") 'helm-anki-browse--edit-card-abort)
    map))

(defun helm-anki-browse (&optional deck)
  (interactive)
  (let ((candidates (helm-anki-browse--candidates deck)))
    (if candidates
	(helm :sources (helm-build-sync-source "Anki cards"
			 :candidates candidates
			 :action (lambda (candidate)
				   (helm-anki-browse--edit-card candidate))
			 :candidate-number-limit 9999
			 :keymap helm-anki-browse-keymap
			 :migemo t)
	      :buffer helm-anki-browse-buffer-name)
      (message "[helm-anki-browse] no candidate"))))

;; private
(defun helm-anki-browse--candidates (deck)
  (let (candidates
	noteId
	Frontvalue
	Backvalue
	FrontBackValue
	cards
	card)

    (if deck
	(setq cards (my-anki-browse-deck-cards deck))
      (setq cards (call-interactively 'my-anki-browse-deck-cards)))

    (dotimes (i (length cards))
      (setq card (aref cards i))
      (setq noteId (let-alist card .noteId))
      (setq Frontvalue (let-alist card .fields.Front.value))
      (setq Backvalue  (let-alist card .fields.Back.value))
      (setq FrontBackvalue  (format "%-30s: %s" Frontvalue Backvalue))
      ;;(add-to-list 'candidates `(,Frontvalue . ,Backvalue) t))
      (add-to-list 'candidates `(,FrontBackvalue . (,noteId ,Frontvalue ,Backvalue)) t))
    candidates))

(defsubst helm-anki-browse--edit-func-to-keys (func)
  (key-description (car-safe (where-is-internal func helm-anki-browse-edit-card-map))))

(defun helm-anki-browse--edit-card (candidate)
  (with-current-buffer (get-buffer-create helm-anki-browse-edit-card-buffer-name)
    (setq header-line-format
	  (format "%s: Commit, %s: Abort"
		  ;;(abbreviate-file-name helm-ag--default-directory)
		  (helm-anki-browse--edit-func-to-keys #'helm-anki-browse--edit-card-commit)
		  (helm-anki-browse--edit-func-to-keys #'helm-anki-browse--edit-card-abort)))
    ;; https://meech.hatenadiary.org/entry/20100414/1271197161
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize (format "noteId: %s\n" (nth 0 candidate)) 'read-only t)
	      (propertize "Front : " 'read-only t 'rear-nosticky t 'front-sticky t)
	      (format "%s" (nth 1 candidate))
	      (propertize "\n" 'read-only t 'rear-nonsticky t)
	      (propertize "Back  : " 'read-only t 'rear-nosticky t 'front-sticky t)
	      (format "%s" (nth 2 candidate))))
    (switch-to-buffer (get-buffer helm-anki-browse-edit-card-buffer-name))
    (use-local-map helm-anki-browse-edit-card-map)))

(defun helm-anki-browse--edit-card-commit ()
  (interactive)
  (let ((noteid (helm-anki-browse--edit-card-get-field "noteId"))
	(front  (helm-anki-browse--edit-card-get-field "Front"))
	(back   (helm-anki-browse--edit-card-get-field "Back")))
    (my-anki-browse-updateNoteFields noteid front back)
    (helm-anki-browse--edit-card-exit)))

(defun helm-anki-browse--edit-card-abort ()
  (interactive)
  (when (y-or-n-p "Discard changes ?")
    (message "Abort edit"))
  (helm-anki-browse--edit-card-exit))

(defun helm-anki-browse--edit-card-get-field (field)
  (let (start end)
    ;;(beginning-of-buffer) ; do not use in Lisp program, read help
    (goto-char (point-min))
    (setq start (re-search-forward (format "^%s *: *" field)))
    (setq end   (point-at-eol))
    (buffer-substring-no-properties start end)))

(defun helm-anki-browse--edit-card-exit ()
  (kill-buffer (get-buffer helm-anki-browse-edit-card-buffer-name))
  (helm-anki-browse (my-anki-browse-current-deck)))

(defun helm-anki-browse--create-card (candidate)
  (with-current-buffer (get-buffer-create helm-anki-browse-create-card-buffer-name)
    (setq header-line-format
	  (format "%s: Commit, %s: Abort"
		  ;;(abbreviate-file-name helm-ag--default-directory)
		  (helm-anki-browse--edit-func-to-keys #'helm-anki-browse--edit-card-commit)
		  (helm-anki-browse--edit-func-to-keys #'helm-anki-browse--edit-card-abort)))
    ;; https://meech.hatenadiary.org/entry/20100414/1271197161
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize (format "noteId: %s\n" (nth 0 candidate)) 'read-only t)
	      (propertize "Front : " 'read-only t 'rear-nosticky t 'front-sticky t)
	      (format "%s" (nth 1 candidate))
	      (propertize "\n" 'read-only t 'rear-nonsticky t)
	      (propertize "Back  : " 'read-only t 'rear-nosticky t 'front-sticky t)
	      (format "%s" (nth 2 candidate))))
    (switch-to-buffer (get-buffer helm-anki-browse-edit-card-buffer-name))
    (use-local-map helm-anki-browse-edit-card-map)))

(provide 'helm-anki-browse)
;;; helm-anki-browse.el ends here
