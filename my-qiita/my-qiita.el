;;; my-qiita.el --- Get Qiita pages with query word  -*- lexical-binding: t; -*-

;; Copyright (C) 2021

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

(defgroup my-qiita nil
  "my-qiita customization group."
  :group 'applications)

(defvar my-qiita--api-base-url "https://qiita.com/api/v2")

;; public
(defun my-qiita-pages-title-url (queryword)
  "Get the list of Qiita pages specified queryword"
  (interactive "sQiita Query: ")
  (my-qiita--get-page-titles queryword #'my-qiita--build-candidate-pages-title-url))

(defun my-qiita--build-candidate-page-titles (results)
  "make the list of Qiita pages with updated date and title"
  (mapcar
   (lambda (result)
     (let* ((title (let-alist result .title))
	    (updated_at (let-alist result .updated_at))
	    (updated_at_yyyymmdd (truncate-string-to-width updated_at 10))
	    (updated_at_hhmmss   (truncate-string-to-width updated_at 19 11)))
       `(,(format "%s %s %s" updated_at_yyyymmdd updated_at_hhmmss title) . ,result))) results))

(defun my-qiita--action-open-page (page-info)
  "Open the given `page' in the browser."
  (interactive)
  (let ((url (let-alist page-info .url)))
    (browse-url-default-browser url)))

(defun xmy-qiita--action-open-page ()
  "Open the given `page' in the browser."
  (interactive)
  (browse-url-default-browser "http://www.yahoo.com"))

;; private
(defun my-qiita--build-candidate-pages-title-url (results)
  "make the list of Qiita pages tile url"
  (mapcar
   (lambda (result)
     (let ((title (let-alist result .title))
	   (url   (let-alist result .url)))
       (message "\"%s\",%s" title url))) results))

(defun my-qiita--get-page-titles (queryword callback)
  (let (datas
	(per_page 100))
    (cl-loop
     for page from 1 to 5 do ; page 1 to n
     (my-qiita--request
      (format "%s/items" my-qiita--api-base-url)
      :params `(("page" . ,page) ("per_page" . ,per_page) ("query" . ,(format "title:%s" queryword)))
      :parser 'json-read
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (setq datas (vconcat datas data))))))
    (if callback
	(funcall callback datas)
      datas)))

(defun my-qiita--request (&rest args)
  (apply #'request
	 (append args
		 ;; https://qiita.com/kou_pg_0131/items/57f86a1abc332ed2185d#api%E3%82%92%E5%8F%A9%E3%81%84%E3%81%A6%E3%81%BF%E3%82%8B
		 ;;`(:headers (("Content-Type" . "application/json") ("Authorization" . (format "Bearer %s" helm-qiita--token))))
		 '(:sync t)
		 ;; https://qiita.com/api/v2/docs#%E3%82%A8%E3%83%A9%E3%83%BC%E3%83%AC%E3%82%B9%E3%83%9D%E3%83%B3%E3%82%B9
		 `(:error ,(cl-function
			    (lambda (&rest response &key data error-thrown &allow-other-keys)
			      (let ((statusCode (let-alist data .statusCode))
				    (message    (let-alist data .message)))
				(message "[my-qiita--request][error] statusCode:%s %s" statusCode message))))))))

(provide 'my-qiita)
;;; my-qiita.el ends here
