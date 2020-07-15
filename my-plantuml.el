;;; my-plantuml.el --- Plantuml                      -*- lexical-binding: t; -*-

;; Copyright (C) 2020

;; Author:  <yamatakau08@gmail.com>
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

(defgroup my-plantuml nil
  "PlantUML execution"
  :group 'tools)

(defcustom my-plantuml-java "/c/Program Files (x86)/Common Files/Oracle/Java/javapath/java.exe"
  "java path"
  :group 'my-plantuml
  :type  'bool)

(defcustom my-plantuml-plantuml "/c/Program Files/PlantUML/plantuml.1.2020.15.jar"
  "plantuml path"
  :group 'my-plantuml
  :type  'bool)

;; public
(defun my-plantuml-exec ()
  (interactive)
  (if (executable-find my-plantuml-java)
      (let (cmd plantuml-file)
	(setq plantuml-file "/C/Temp/Plantuml/uml.txt")
	(setq cmd (format "%s -jar %s -charset UTF-8 %s" my-plantuml-java my-plantuml-plantuml plantuml-file))
	;; "/C/Program\\ Files\\ \\(x86\\)/Common\\ Files/Oracle/Java/javapath/java.exe -jar /C/Program\\ Files/PlantUML/plantuml.1.2020.15.jar -charset UTF-8 /C/Temp/Plantuml/uml.txt"
	(shell-command-to-string cmd))
    (error "[my-plantuml] java not found")))

(provide 'my-plantuml)
;;; my-plantuml.el ends here
