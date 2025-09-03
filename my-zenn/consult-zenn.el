;;; consult-zenn.el --- Zenn with consult          -*- lexical-binding: t; -*-

;; Copyright (C) 2025

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
(require 'my-zenn)
(require 'consult)

(defgroup consult-zenn nil
  "consult-zenn customization group."
  :group 'applications)

(defvar consult-zenn--page-titles-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-j") #'consult-zenn--open-browser)
    (define-key map (kbd "RET") #'consult-zenn--open-browser)
    map)
  "keymap used by `consult-zenn-page-titles'.")

(defun consult-zenn--open-browser ()
  (interactive)
  (let* ((selected (consult--vertico-candidate))
	 (page-info (cdr (assoc selected consult-zenn--candidates))))
    (my-zenn--action-open-page (my-zenn-get-url-from-entry page-info))))

(defvar consult-zenn--candidates nil)

(defun consult-zenn-page-titles (queryword)
  "Get the titles of Zenn pages in whcih inculude the queryword, then open the selected title on browser"
  (interactive "sZenn Query: ")
  (my-zenn--get-page-titles queryword
   (lambda (page-titles)
     (if (seq-empty-p page-titles)
	 (message "No titles %s in Zenn" queryword)
       (let ((candidates (my-zenn--build-candidate-page-titles page-titles))
	     selected
	     page-info)
	 (setq consult-zenn--candidates candidates) ; for consult-zenn--page-titles-map
	 (setq selected
	       (consult--read candidates
			      :sort nil
   			      :keymap consult-zenn--page-titles-map))
	 ;; the following C-j and RET the open browser without consult-zenn--page titles-map
	 ;; (setq selected-page-info (cdr (assoc selected candidates)))
	 ;; (my-zenn--action-open-page (my-zenn-get-url-from-entry selected-page-info))
	 )))))

(provide 'consult-zenn)
;;; consult-zenn.el ends here
