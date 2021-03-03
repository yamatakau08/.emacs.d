;;; recochoku.el --- scraping recochoku              -*- lexical-binding: t; -*-

;; Copyright (C) 2021

;; Author:  <0000910700@JPC20165182>
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

;;; Code:

(require 'seml-mode)

(defun recochoku-single-ranking ()
  "scrape recochoku single ranking"
  (interactive)
  (let* ((url "https://recochoku.jp/ranking/single/daily?affiliate=4410101009")
	 (scraping-name "rechochoku-single-ranking")
	 (buffer-html   (format "%s.html"   scraping-name))
	 (buffer-output (format "%s.output" scraping-name)))
    (with-output-to-temp-buffer buffer-html
      ;; Since url-insert-file-contents puts out the contents in current buffer,
      ;; change current buffer to buffer-html
      (set-buffer buffer-html)
      (url-insert-file-contents url))

    (with-output-to-temp-buffer buffer-output
      (print
       (thread-last (seml-encode-buffer-from-html buffer-html)
	 (seml-xpath-without-top '(html script))
	 (mapcan 'identity)
	 (cl-some (lambda (elm)
                    (string-match "rankingList = \\(.*\\)" elm)
                    (match-string 1 elm)))
	 (json-read-from-string)
	 (alist-get 'ranking)
	 (funcall (lambda (elm) (cl-coerce elm 'list)))
	 (mapcar (lambda (elm)
		   (let-alist elm (list .rank .music.title .music.artist.name)))))))))

;; conao3's code
;;(with-current-buffer "recochoku.html"
;;   (thread-last (seml-encode-buffer-from-html)
;;     (seml-xpath-without-top '(html script))
;;     (mapcan 'identity)
;;     (cl-some (lambda (elm)
;;                (string-match "rankingList = \\(.*\\)" elm)
;;                (match-string 1 elm)))
;;     (json-read-from-string)
;;     (alist-get 'ranking)
;;     (funcall (lambda (elm) (cl-coerce elm 'list)))
;;     (mapcar (lambda (elm)
;;               (let-alist elm (list .rank .music.title .music.artist.name)))))))

(provide 'recochoku)
;;; recochoku.el ends here
