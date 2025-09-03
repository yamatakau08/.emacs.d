;;; my-zenn.el --- Get Zenn pages with query word  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author:  <yamatakau08@gmail.com>
;; Keywords: applications

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
(require 'request)

(defgroup my-zenn nil
  "my-zenn customization group."
  :group 'applications)

(defvar my-zenn--site-url "https://zenn.dev")
(defvar my-zenn--api-base-url   (concat my-zenn--site-url "/" "api"))
(defvar my-zenn--api-search-url (concat my-zenn--api-base-url "/" "search"))

(defcustom my-zenn-search-source "articles"
  "source, select amaong articles, books, scraps, users, publications"
  :type 'string
  :group 'my-zenn)

(defcustom my-zenn-search-count 100
  "Max 100, Zenn API limitaiton the number of the articles to retrieve at once"
  :type 'integer
  :group 'my-zenn)

(defcustom my-zenn-search-pages 5
  "retrieve pages, probably my-zenn-search-count * my-zenn--search-page articles will be retrieve"
  :type 'integer
  :group 'my-zenn)

(defcustom my-zenn-search-order "alltime"
  "Zenn API order daily(trending), alltime, latest"
  :type 'string
  :group 'my-zenn)

;; public
(defun my-zenn--build-candidate-page-titles (response-json)
  "Make the list of the sources (articles) with published_at and title for consult."
  (let* ((sources (apply #'vconcat (mapcar (lambda (elm) (let-alist elm .articles)) response-json)))
         (candidates
          (mapcar (lambda (elm)
                    `(,(let-alist elm (format "%s %s" .published_at .title)) . ,elm))
                  sources)))
    candidates))

(defun xmy-zenn--build-candidate-page-titles (response_json)
  "make the list of the sources (articles) with published_at and title for consult"
  (let (sources
	candidates)
    ;; get articles
    (mapcar
     (lambda (elm)
       (setq sources (vconcat sources (let-alist elm .articles))))
     response_json)

    (setq candidates
	  (mapcar
	   (lambda (elm)
	     `(,(let-alist elm (format "%s %s" .published_at .title)) . ,elm))
	   sources))

    candidates)
  )

(defun my-zenn-get-url-from-entry (entry)
  (concat my-zenn--site-url (let-alist entry .path)))

(defun my-zenn--action-open-page (url)
  "Open the given `url' page in the browser."
  (browse-url url))

;; private
(defun my-zenn--build-candidate-pages-title-url (results)
  "make the list of Zenn pages tile url"
  (mapcar
   (lambda (result)
     (let ((title (let-alist result .title))
	   (url   (let-alist result .url)))
       (message "\"%s\",%s" title url))) results))

(defun my-zenn--get-page-titles (queryword callback)
  (let (parsed-data
	(page 1))
    (cl-loop do
	     (my-zenn--request
	      my-zenn--api-search-url
	      :params `(("q" . ,(format "%s" queryword))
			("source" . ,my-zenn-search-source)
			("page" . ,page)
			("count" . ,my-zenn-search-count)
			("order" . ,my-zenn-search-order))
	      :parser 'json-read
	      :success (cl-function
			(lambda (&key data &allow-other-keys)
			  (setq page (let-alist data .next_page))
			  (add-to-list 'parsed-data data t)
			  )))
	     while (and page (<= page my-zenn-search-pages)))
    (if callback
	(funcall callback parsed-data)
      parsed-data)))

(defun my-zenn--request (&rest args)
  (apply #'request
	 (append args
		 ;;
		 ;;`(:headers (("Content-Type" . "application/json") ("Authorization" . (format "Bearer %s" helm-zenn--token))))
		 '(:sync t)
		 ;;
		 `(:error ,(cl-function
			    (lambda (&rest response &key data error-thrown &allow-other-keys)
			      (let ((statusCode (let-alist data .statusCode))
				    (message    (let-alist data .message)))
				(message "[my-zenn--request][error] statusCode:%s %s" statusCode message))))))))

(provide 'my-zenn)
;;; my-zenn.el ends here
