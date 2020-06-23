;;; helm-anki-browse.el

;; Copyright (C) 2020

;; Author:  <0000910700@JPC20165182>
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

;;
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
;;

;;; Code:

(defun helm-anki-browse-candidates (deck)
  (interactive "sDeck: ")
  (let (candidates
	noteId
	Frontvalue
	Backvalue
	FrontBackValue
	cards
	card)
    (setq cards (my-anki-browse-deck-cards deck))
    (dotimes (i (length cards))
      (setq card (aref cards i))
      (setq noteId (let-alist card .noteId))
      (setq Frontvalue (let-alist card .fields.Front.value))
      (setq Backvalue  (let-alist card .fields.Back.value))
      (setq FrontBackvalue  (format "%-30s: %s" Frontvalue Backvalue))
      ;;(add-to-list 'candidates `(,Frontvalue . ,Backvalue) t))
      (add-to-list 'candidates `(,FrontBackvalue . nil) t))
    candidates))

(setq some-helm-source
      `((name . "Anki Browser")
	(candidates . helm-anki-browse-candidates)
	(action . (lambda (candidate)
		    (message "%s" candidate)))))

(defun helm-anki-browse (deck)
  (interactive "sDeck: ")
  (helm :sources
	`((name . "Anki Browser")
	  (candidates . ,(helm-anki-browse-candidates deck))
	  (candidate-number-limit . 9999)
	  (action . (lambda (candidate)
		      (message "%s" candidate))))))

(provide 'helm-anki-browse)
;;; helm-anki-browse.el ends here
