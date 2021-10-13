;;; consult-qiita.el --- Qiita with consult          -*- lexical-binding: t; -*-

;; Copyright (C) 2021

;; Author:  <yamatakau08@gmail.com>
;; Keywords: tools, tools

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

;;; Code:
(require 'my-qiita)
(require 'consult)

(defgroup consult-qiita nil
  "consult-qiita customization group."
  :group 'applications)

(defvar consult-qiita--page-titles-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-j") #'consult-qiita--open-browser)
    map)
  "keymap used by `consult-qiita-page-titles'.")

(defun consult-qiita--open-browser ()
  (interactive)
  (let* ((cand (consult-vertico--candidate))
	 (page-info (cdr (assoc cand consult-qiita--candidates))))
    (my-qiita--action-open-page page-info)))

(defvar consult-qiita--candidates nil)

(defun consult-qiita-page-titles (queryword)
  "Get the titles of Qiita pages in whcih inculude the queryword, then open the selected title on browser"
  (interactive "sQiita Query: ")
  (my-qiita--get-page-titles queryword
   (lambda (page-titles)
     (if (seq-empty-p page-titles)
	 (message "No titles %s in Qiita" queryword)
       (let ((candidates (my-qiita--build-candidate-page-titles page-titles))
	     selected
	     page-info)
	 (setq consult-qiita--candidates candidates)
	 (setq selected
	       (consult--read candidates
			      :sort nil
   			      :keymap consult-qiita--page-titles-map))
	 (setq page-info (cdr (assoc selected consult-qiita--candidates)))
	 (my-qiita--action-open-page page-info))))))

(provide 'consult-qiita)
;;; consult-qiita.el ends here
