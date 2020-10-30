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
(require 'cl)
(require 'request)

(defcustom helm-confluence--password nil
  "Password to use when logging in to Confluence.  Not recommended to set this (helm-confluence will save password per session)."
  :type 'string)

(defun helm-confluence--ensure-password ()
  "Ensures that `helm-confluence--password' is set."
  (when (not helm-confluence--password)
    (helm-confluence--read-password)))

(defun helm-confluence--read-password ()
  "Read a new value for `helm-jira-password'."
  (setq helm-confluence--password (read-passwd (format "Confluence-Password for %s: " my-confluence-username))))

(defun helm-confluence--build-basic-auth-token ()
  "Build the base64-encoded auth token from `helm-confluence-username' and `helm-confluence--password'."
  (base64-encode-string (format "%s:%s" my-confluence-username helm-confluence--password)))

(defun helm-confluence--build-auth-header ()
  "Build the Authorization-Header for CONFLUENCE requests."
  (format "Basic %s" (helm-confluence--build-basic-auth-token)))

(defun helm-confluence--request (&rest args)
  (helm-confluence--ensure-password)
  (if (plist-get (car args) :headers)
      (apply 'request (append args '(:sync t)))
    (apply 'request (append args
			    `(:headers (("Authorization" . ,(helm-confluence--build-auth-header))))
			    '(:sync t)))))

(defun helm-confluence-search-content-by-cql (cql callback)
  (helm-confluence--request
   (format "%s/rest/api/content/search" my-confluence-url)
   :type "GET"
   :parser 'json-read
   :params `(("cql" . ,cql))
   :success (cl-function
	     (lambda (&key data &allow-other-keys)
	       (funcall callback (cdr (car data)))))
   :error   (cl-function
	     (lambda (&rest args &key error-thrown &allow-other-keys)
	       (message "Got error: %S" error-thrown)))))

(defun helm-confluence--build-candidate-my-pages (results)
  (mapcar
   (lambda (result)
     (let* ((id    (let-alist result .id))
	    (title (let-alist result .title)))
       `(,(format "%-10s: %s" id title) . ,result)))
   results))

(defun helm-confluence--action-open-page (page-info)
  "Open the given `page' in the browser."
  (let ((webui (let-alist page-info ._links.webui)))
    (browse-url-default-browser (format "%s%s" my-confluence-url webui))))

(defun helm-confluence-get-my-pages ()
  (interactive)
  (helm-confluence-search-content-by-cql
   (format "creator=%s and type=page" my-confluence-username)
   (lambda (results)
     (let* ((helm-src
	     (helm-build-sync-source "my-pages"
               :candidates (helm-confluence--build-candidate-my-pages results)
	       :action (helm-make-actions
                        "Open in browser" #'helm-confluence--action-open-page)
	       :migemo t)))
       (helm :sources helm-src)))))

(provide 'helm-confluence)
;;; helm-confluence.el ends here
