;;; helm-qiita.el --- Get list from Qiita            -*- lexical-binding: t; -*-

;; Copyright (C) 2021

;; Author:  <yamatakau08@gmail.com>
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

;; refer Qiita page https://qiita.com/kou_pg_0131/items/57f86a1abc332ed2185d

;;; Code:
(require 'my-qiita)
(require 'helm)

;; for debug
;;(setq request-message-level 'debug)

(defgroup helm-qiita nil
  "helm-qiita customization group."
  :group 'applications)

(defun helm-qiita-page-titles (queryword)
  "Get the titles of Qiita pages in whcih inculude the queryword, then open the selected title on browser"
  (interactive "sQiita Query: ")
  (my-qiita--get-page-titles queryword
   (lambda (page-titles)
     (if (seq-empty-p page-titles)
	 (message "No titles %s in Qiita" queryword)
       (let* ((helm-src
    	       (helm-build-sync-source "Qiita-pages"
    		 :candidates (my-qiita--build-candidate-page-titles page-titles)
    		 :action (helm-make-actions
                          "Open page n browser" #'my-qiita--action-open-page)
    		 :candidate-number-limit 10000
    		 :migemo t)))
	 (helm :sources helm-src))))))

(provide 'helm-qiita)
;;; helm-qiita.el ends here
