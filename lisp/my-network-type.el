;;; my-network-type.el --- get network type, company,private,not connected  -*- lexical-binding: t; -*-

;; Copyright (C) 2020

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

;;

;;; Code:
(defun my-network-type ()
  (let ((folist (my-network-type--interface-list)))
    (cond ((memq company-ip-first-octet folist)  'company)
	  ((memq company-ip-first-octet2 folist) 'company)
	  ((memq 192                    folist) 'private)
	  (t                                     nil))))

;; private
(defun my-network-type--interface-list ()
  "return all ipv4's network interface first octet"
  (delete-dups
   (mapcar (lambda (interface) (aref (nth 1 interface) 0)) (network-interface-list t 'ipv4))))

(provide 'my-network-type)
;;; my-network-type.el ends here
