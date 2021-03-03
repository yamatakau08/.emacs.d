;;; amazon-overseas-movie.el --- scraping amazon overseas movie ranking 50 -*- lexical-binding: t; -*-

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
;; scraping amazon overseas movie ranking 50
;;

;;; Code:

(require 'seml-mode)

(defun amazon-overseas-movie-ranking-50 ()
  "scraping amazon overseas movie ranking 50 using pcase to extract the value."
  (interactive)
  (let* (;(url "https://www.amazon.co.jp/gp/bestsellers/dvd/519877011/ref=zg_bs_pg_1?ie=UTF8&pg=1")
	 (url "https://www.amazon.co.jp/%E6%97%A5%E6%9C%AC%E6%98%A0%E7%94%BB-%E6%98%9F4%E3%81%A4%E4%BB%A5%E4%B8%8A-%E3%82%AB%E3%83%86%E3%82%B4%E3%83%AA%E3%83%BC%E5%88%A5/s?rh=n%3A2478493051%2Cp_72%3A2761627051")
	 (scraping-name "amazon-overseas-movie-ranking-50")
	 (buffer-html   (format "%s.html"   scraping-name))
	 (buffer-output (format "%s.output" scraping-name)))
    (with-output-to-temp-buffer buffer-html
      ;; Since url-insert-file-contents puts out the contents in current buffer,
      ;; change current buffer to buffer-html
      (set-buffer buffer-html)
      (url-insert-file-contents url))

    (with-output-to-temp-buffer buffer-output
      (let* ((shtml (seml-encode-buffer-from-html buffer-html))
      	     (elements (car (seml-xpath-without-top '(div ol) shtml))))
	(mapc (lambda (element)
		(pcase element
		  (`(li ,_ (span ,_ (div ,_ (div ,_ (span ,_ (span ,_ ,rank)) ,_) (span ,_ (a ,_ (span ,_ (div ,_ (img ((alt . ,title) ,src ,height ,width)))) ,_) ,_ ,_ ,_ (div ,_ (a ,_ (span ,_ (span ,_ ,price))))))))
		   (print (list rank title price)))
		  (`(li ,_ (span ,_ (div ,_ (div ,_ (span ,_ (span ,_ ,rank)) ,_) (span ,_ (a ,_ (span ,_ (div ,_ (img ((alt . ,title) ,src ,height ,width)))) ,_)  ,_ ,_ ,_ (a . (,_ (span . (,_ ,_ (span ,_ (span ,_ ,price)) ,_))))))))
		   (print (list rank title price))))
		) elements)))))

(defun xamazon-overseas-movie-ranking-50 ()
  "scraping amazon overseas movie ranking 50"
  (interactive)
  (let* ((url "https://www.amazon.co.jp/gp/bestsellers/dvd/519877011/ref=zg_bs_pg_1?ie=UTF8&pg=1")
	 (scraping-name "amazon-overseas-movie-ranking-50")
	 (buffer-html   (format "%s.html"   scraping-name))
	 (buffer-output (format "%s.output" scraping-name)))
    (with-output-to-temp-buffer buffer-html
      ;; Since url-insert-file-contents puts out the contents in current buffer,
      ;; change current buffer to buffer-html
      (set-buffer buffer-html)
      (url-insert-file-contents url))

    (with-output-to-temp-buffer buffer-output
      (let* ((shtml (seml-encode-buffer-from-html buffer-html))
      	     (elements (car (seml-xpath-without-top '(div ol) shtml))))
	(mapc (lambda (element)
		;;(message "%s" element) ; debug
		(let ((rank (nth 2 (nth 2 (nth 2 (nth 2 (nth 2 (nth 2 element)))))))
		      (title (string-trim (nth 2 (nth 3 (nth 2(nth 3 (nth 2 (nth 2 element))))))))
		      ;; price: need to dig into code, judge as nil on some element
		      ;;(price (nth 2 (nth 2 (nth 3 (nth 2 (nth 6 (nth 3 (nth 2 (nth 2 element)))))))))
		      (price (pcase element
			       (`(li ,_ (span ,_ (div ,_ (div ,_ (span ,_ (span ,_ ,rank)) ,_) (span ,_ (a ,_ (span ,_ (div ,_ (img . (((alt . ,title) ,src ,height ,width))))) ,_) ,_ ,_ ,_ (div ,_ (a ,_ (span ,_ (span ,_ ,price))))))))
				price))))
		  (print (list rank title price)))) elements)))))

(provide 'amazon-overseas-movie)
;;; amazon-overseas-movie.el ends here
