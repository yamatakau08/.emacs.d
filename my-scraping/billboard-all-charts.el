;;; bilboard-all-charts.el --- billboard all charts -*- lexical-binding: t; -*-

;; Copyright (C) 2021

;; Author:  yamatakau08@gmail.com
;; Keywords: tools

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
;;
;;

;;; Code:

(require 'seml-mode)
(require 'helm)

(defun helm-billboard-all-charts ()
  "get all charts"
  (interactive)
  (helm :sources (helm-build-sync-source "All Charts CATEGORIES"
		   :candidates (billboad-all-charts--categories)
		   :candidate-number-limit 10000
		   :migemo t
		   ;;:action (helm-make-actions
		   ;         "playlist" #'spotify-playlist--titles)
		   :persistent-action #'ignore)))

(defun billboad-all-charts--categories ()
  (let (entity tmp categories)
    (setq entity (seml-xpath '(div)
		   (seml-encode-string-from-html
		    (with-temp-buffer
		      (url-insert-file-contents "https://www.billboard.com/charts")
		      (buffer-string)))))
    (setq tmp (seml-xpath '(div div div div div div div) (nth 8 entity)))

    (setq categories
	  (seq-filter
	   (lambda (elt)
	     (string-match "chart-panel__item chart-panel__item--selector" (let-alist (nth 1 elt) .class)))
	   tmp))

    (mapcar (lambda (elt)
	      (replace-regexp-in-string "\n" "" (nth 2 (nth 2 elt)))) categories)

    ))

(provide 'billboard-all-charts)
;;; billboard-top-100.el ends here
