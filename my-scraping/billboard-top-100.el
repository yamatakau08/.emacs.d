;;; bilboard-top-100.el --- scraping billboard top 100 -*- lexical-binding: t; -*-

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
;; scraping billboadr top 100
;;

;;; Code:

(require 'seml-mode)

(defun billboard-top-100 ()
  "scraping billboard top 100"
  (interactive)
  (let* ((url "https://www.billboard.com/charts/hot-100")
	 (scraping-name "billboard-top-100")
	 (buffer-html   (format "%s.html"   scraping-name))
	 (buffer-output (format "%s.output" scraping-name)))
    (with-output-to-temp-buffer buffer-html
      ;; Since url-insert-file-contents puts out the contents in current buffer,
      ;; change current buffer to buffer-html
      (set-buffer buffer-html)
      (url-insert-file-contents url))

    (with-output-to-temp-buffer buffer-output
      (let* ((shtml (seml-encode-buffer-from-html buffer-html))
	     (elements (seml-xpath-without-top '(ol li button) shtml)))
	(mapc (lambda (element)
		(if (>= (length element) 2) ; to suppress error in case element is nil or ("CONTINUE TO BILLBOARD BIZ CONTENT")
		    (let* ((rank   (nth 2 (nth 2 (nth 0 element))))
			   (song   (nth 2 (nth 3 (nth 1 element))))
			   (artist (nth 2 (nth 3 (nth 1 element))))
			   (delta-text--default (nth 2 (nth 2 (nth 4 (nth 1 element)))))
			   (delta-text--last    (nth 2 (nth 3 (nth 4 (nth 1 element)))))
			   (delta-text--peak    (nth 2 (nth 4 (nth 4 (nth 1 element)))))
			   (delta-text--week    (nth 2 (nth 5 (nth 4 (nth 1 element)))))
			   (background-image    (cdr   (nth 1 (nth 1 (nth 3 element))))))
		      (print (list rank song artist
				   delta-text--default delta-text--last delta-text--peak delta-text--week
				   background-image))))) elements)))))

(provide 'billboard-top-100)
;;; billboard-top-100.el ends here
