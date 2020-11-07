;;; helm-confluence.el --- helm for my-confluence    -*- lexical-binding: t; -*-

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

;;

;;; Code:
(require 'request)
(require 'helm)

(defun helm-confluence--build-candidate-pages (ret)
  (let ((results (let-alist ret .results)))
    (mapcar
     (lambda (result)
       (let* ((id    (let-alist result .id))
	      (title (let-alist result .title)))
	 `(,(format "%-10s: %s" id title) . ,result)))
     results)))

(defun helm-confluence--action-open-page (page-info)
  "Open the given `page' in the browser."
  (let ((webui (let-alist page-info ._links.webui)))
    (browse-url-default-browser (format "%s%s" my-confluence-url webui))))

(defun helm-confluence-get-my-pages ()
  "Get the list of the pages of currentUser()"
  (interactive)
  (my-confluence--search-content-by-cql
   (format "creator=currentUser() and type=page") ; added limit=10000 in my-confluence--search-content-by-cql
   (lambda (ret)
     (let* ((helm-src
	     (helm-build-sync-source "my-pages"
	       :candidates (helm-confluence--build-candidate-pages ret)
	       :action (helm-make-actions
                        "Open in browser" #'helm-confluence--action-open-page)
	       :candidate-number-limit 10000
	       :migemo t)))
       (helm :sources helm-src)))))

(defun helm-confluence--get-content-for-space (spacekey)
  (my-confluence--search-content-by-cql
   (format "space=%s and type=page" spacekey)
   (lambda (ret)
     (let* ((helm-src
	     (helm-build-sync-source "content"
	       :candidates (helm-confluence--build-candidate-pages ret)
	       :action (helm-make-actions
                        "content for space" #'helm-confluence--action-open-page)
	       :candidate-number-limit 10000
	       :migemo t)))
       (helm :sources helm-src)))))

(defun helm-confluence--action-get-contet-for-space (spaceinfo)
  "List contest of spaceKey"
  (let ((spacekey (let-alist spaceinfo .key)))
    (helm-confluence--get-content-for-space spacekey)))

(defun helm-confluence--build-candidate-spaces (ret)
  (let ((results (let-alist ret .results)))
    (mapcar
     (lambda (result)
       (let* ((key  (let-alist result .key))
	      (name (let-alist result .name)))
	 `(,(format "%-20s: %s" key name) . ,result)))
     results)))

(defun helm-confluence-get-spaces ()
  "Get the spaces"
  (interactive)
  (my-confluence--get-spaces
   (lambda (ret)
     (let* ((helm-src
	     (helm-build-sync-source "spaces"
	       :candidates (helm-confluence--build-candidate-spaces ret)
	       :action (helm-make-actions
                        "content for space" #'helm-confluence--action-get-contet-for-space)
	       :candidate-number-limit 10000
	       :migemo t)))
       (helm :sources helm-src)))))

(provide 'helm-confluence)
;;; helm-confluence.el ends here
